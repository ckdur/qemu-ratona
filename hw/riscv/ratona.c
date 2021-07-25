/*
 * QEMU RISC-V RatonaIO Board
 *
 * Copyright (c) 2017 SiFive, Inc.
 *
 * RISC-V machine with 16550a UART and RatonaIO MMIO
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms and conditions of the GNU General Public License,
 * version 2 or later, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "qemu/units.h"
#include "qemu/log.h"
#include "qemu/error-report.h"
#include "qapi/error.h"
#include "hw/boards.h"
#include "hw/loader.h"
#include "hw/sysbus.h"
#include "hw/qdev-properties.h"
#include "hw/char/serial.h"
#include "target/riscv/cpu.h"
#include "hw/riscv/riscv_hart.h"
#include "hw/riscv/ratona.h"
#include "hw/riscv/boot.h"
#include "hw/riscv/numa.h"
#include "hw/intc/sifive_clint.h"
#include "hw/intc/sifive_plic.h"
#include "hw/char/sifive_uart.h"
#include "chardev/char.h"
#include "sysemu/arch_init.h"
#include "sysemu/device_tree.h"
#include "sysemu/sysemu.h"
#include "hw/pci/pci.h"
#include "hw/pci-host/gpex.h"

#if defined(TARGET_RISCV32)
# define BIOS_FILENAME "opensbi-riscv32-generic-fw_dynamic.bin"
#else
# define BIOS_FILENAME "opensbi-riscv64-generic-fw_dynamic.bin"
#endif

static const struct MemmapEntry {
    hwaddr base;
    hwaddr size;
} ratona_memmap[] = {
    [RATONA_DEBUG] =       {        0x0,         0x100 },
    [RATONA_MROM] =        {     0x1000,        0xf000 },
    [RATONA_CLINT] =       {  0x2000000,       0x10000 },
    [RATONA_PLIC] =        {  0xc000000, RATONA_PLIC_SIZE(RATONA_CPUS_MAX * 2) },
    [RATONA_UART0] =       { 0x10000000,         0x100 },
    [RATONA_DRAM] =        { 0x80000000,           0x0 },
};

static void create_fdt(RISCVRatonaState *s, const struct MemmapEntry *memmap,
    uint64_t mem_size, const char *cmdline)
{
    void *fdt;
    int cpu, socket;
    const char *dtb_filename;
    MachineState *mc = MACHINE(s);
    uint64_t addr, size;
    uint32_t *clint_cells, *plic_cells;
    unsigned long clint_addr, plic_addr;
    uint32_t plic_phandle[MAX_NODES];
    uint32_t cpu_phandle, intc_phandle, pbus_clock_phandle;
    uint32_t phandle = 1;
    char *mem_name, *cpu_name, *core_name, *intc_name;
    char *name, *clint_name, *plic_name, *clust_name;

    dtb_filename = qemu_opt_get(qemu_get_machine_opts(), "dtb");
    if (dtb_filename) {
        fdt = s->fdt = load_device_tree(dtb_filename, &s->fdt_size);
        if (!fdt) {
            error_report("load_device_tree() failed");
            exit(1);
        }
        goto update_bootargs;
    } else {
        fdt = s->fdt = create_device_tree(&s->fdt_size);
        if (!fdt) {
            error_report("create_device_tree() failed");
            exit(1);
        }
    }

    qemu_fdt_setprop_string(fdt, "/", "model", "riscv-ratona,qemu");
    qemu_fdt_setprop_string(fdt, "/", "compatible", "riscv-ratona");
    qemu_fdt_setprop_cell(fdt, "/", "#size-cells", 0x2);
    qemu_fdt_setprop_cell(fdt, "/", "#address-cells", 0x2);

    qemu_fdt_add_subnode(fdt, "/soc");
    qemu_fdt_setprop(fdt, "/soc", "ranges", NULL, 0);
    qemu_fdt_setprop_string(fdt, "/soc", "compatible", "simple-bus");
    qemu_fdt_setprop_cell(fdt, "/soc", "#size-cells", 0x2);
    qemu_fdt_setprop_cell(fdt, "/soc", "#address-cells", 0x2);

    qemu_fdt_add_subnode(fdt, "/cpus");
    qemu_fdt_setprop_cell(fdt, "/cpus", "timebase-frequency",
                          SIFIVE_CLINT_TIMEBASE_FREQ);
    qemu_fdt_setprop_cell(fdt, "/cpus", "#size-cells", 0x0);
    qemu_fdt_setprop_cell(fdt, "/cpus", "#address-cells", 0x1);
    qemu_fdt_add_subnode(fdt, "/cpus/cpu-map");

    for (socket = (riscv_socket_count(mc) - 1); socket >= 0; socket--) {
        clust_name = g_strdup_printf("/cpus/cpu-map/cluster%d", socket);
        qemu_fdt_add_subnode(fdt, clust_name);

        plic_cells = g_new0(uint32_t, s->soc[socket].num_harts * 4);
        clint_cells = g_new0(uint32_t, s->soc[socket].num_harts * 4);

        for (cpu = s->soc[socket].num_harts - 1; cpu >= 0; cpu--) {
            cpu_phandle = phandle++;

            cpu_name = g_strdup_printf("/cpus/cpu@%d",
                s->soc[socket].hartid_base + cpu);
            qemu_fdt_add_subnode(fdt, cpu_name);
#if defined(TARGET_RISCV32)
            qemu_fdt_setprop_string(fdt, cpu_name, "mmu-type", "riscv,sv32");
#else
            qemu_fdt_setprop_string(fdt, cpu_name, "mmu-type", "riscv,sv48");
#endif
            name = riscv_isa_string(&s->soc[socket].harts[cpu]);
            qemu_fdt_setprop_string(fdt, cpu_name, "riscv,isa", name);
            g_free(name);
            qemu_fdt_setprop_string(fdt, cpu_name, "compatible", "riscv");
            qemu_fdt_setprop_string(fdt, cpu_name, "status", "okay");
            qemu_fdt_setprop_cell(fdt, cpu_name, "reg",
                s->soc[socket].hartid_base + cpu);
            qemu_fdt_setprop_string(fdt, cpu_name, "device_type", "cpu");
            riscv_socket_fdt_write_id(mc, fdt, cpu_name, socket);
            qemu_fdt_setprop_cell(fdt, cpu_name, "phandle", cpu_phandle);

            intc_name = g_strdup_printf("%s/interrupt-controller", cpu_name);
            qemu_fdt_add_subnode(fdt, intc_name);
            intc_phandle = phandle++;
            qemu_fdt_setprop_cell(fdt, intc_name, "phandle", intc_phandle);
            qemu_fdt_setprop_string(fdt, intc_name, "compatible",
                "riscv,cpu-intc");
            qemu_fdt_setprop(fdt, intc_name, "interrupt-controller", NULL, 0);
            qemu_fdt_setprop_cell(fdt, intc_name, "#interrupt-cells", 1);

            clint_cells[cpu * 4 + 0] = cpu_to_be32(intc_phandle);
            clint_cells[cpu * 4 + 1] = cpu_to_be32(IRQ_M_SOFT);
            clint_cells[cpu * 4 + 2] = cpu_to_be32(intc_phandle);
            clint_cells[cpu * 4 + 3] = cpu_to_be32(IRQ_M_TIMER);

            plic_cells[cpu * 4 + 0] = cpu_to_be32(intc_phandle);
            plic_cells[cpu * 4 + 1] = cpu_to_be32(IRQ_M_EXT);
            plic_cells[cpu * 4 + 2] = cpu_to_be32(intc_phandle);
            plic_cells[cpu * 4 + 3] = cpu_to_be32(IRQ_S_EXT);

            core_name = g_strdup_printf("%s/core%d", clust_name, cpu);
            qemu_fdt_add_subnode(fdt, core_name);
            qemu_fdt_setprop_cell(fdt, core_name, "cpu", cpu_phandle);

            g_free(core_name);
            g_free(intc_name);
            g_free(cpu_name);
        }

        addr = memmap[RATONA_DRAM].base + riscv_socket_mem_offset(mc, socket);
        size = riscv_socket_mem_size(mc, socket);
        mem_name = g_strdup_printf("/memory@%lx", (long)addr);
        qemu_fdt_add_subnode(fdt, mem_name);
        qemu_fdt_setprop_cells(fdt, mem_name, "reg",
            addr >> 32, addr, size >> 32, size);
        qemu_fdt_setprop_string(fdt, mem_name, "device_type", "memory");
        riscv_socket_fdt_write_id(mc, fdt, mem_name, socket);
        g_free(mem_name);

        clint_addr = memmap[RATONA_CLINT].base +
            (memmap[RATONA_CLINT].size * socket);
        clint_name = g_strdup_printf("/soc/clint@%lx", clint_addr);
        qemu_fdt_add_subnode(fdt, clint_name);
        qemu_fdt_setprop_string(fdt, clint_name, "compatible", "riscv,clint0");
        qemu_fdt_setprop_cells(fdt, clint_name, "reg",
            0x0, clint_addr, 0x0, memmap[RATONA_CLINT].size);
        qemu_fdt_setprop(fdt, clint_name, "interrupts-extended",
            clint_cells, s->soc[socket].num_harts * sizeof(uint32_t) * 4);
        riscv_socket_fdt_write_id(mc, fdt, clint_name, socket);
        g_free(clint_name);

        plic_phandle[socket] = phandle++;
        plic_addr = memmap[RATONA_PLIC].base + (memmap[RATONA_PLIC].size * socket);
        plic_name = g_strdup_printf("/soc/plic@%lx", plic_addr);
        qemu_fdt_add_subnode(fdt, plic_name);
        qemu_fdt_setprop_cell(fdt, plic_name,
            "#address-cells", FDT_PLIC_ADDR_CELLS);
        qemu_fdt_setprop_cell(fdt, plic_name,
            "#interrupt-cells", FDT_PLIC_INT_CELLS);
        qemu_fdt_setprop_string(fdt, plic_name, "compatible", "riscv,plic0");
        qemu_fdt_setprop(fdt, plic_name, "interrupt-controller", NULL, 0);
        qemu_fdt_setprop(fdt, plic_name, "interrupts-extended",
            plic_cells, s->soc[socket].num_harts * sizeof(uint32_t) * 4);
        qemu_fdt_setprop_cells(fdt, plic_name, "reg",
            0x0, plic_addr, 0x0, memmap[RATONA_PLIC].size);
        qemu_fdt_setprop_cell(fdt, plic_name, "riscv,ndev", RATONA_NDEV);
        riscv_socket_fdt_write_id(mc, fdt, plic_name, socket);
        qemu_fdt_setprop_cell(fdt, plic_name, "phandle", plic_phandle[socket]);
        g_free(plic_name);

        g_free(clint_cells);
        g_free(plic_cells);
        g_free(clust_name);
    }

    riscv_socket_fdt_write_distance_matrix(mc, fdt);

    pbus_clock_phandle = phandle++;
    name = g_strdup_printf("/soc/subsystem_pbus_clock");
    qemu_fdt_add_subnode(fdt, name);
    qemu_fdt_setprop_cell(fdt, name, "phandle", pbus_clock_phandle);
    qemu_fdt_setprop_string(fdt, name, "clock-output-names", "subsystem_pbus_clock");
    qemu_fdt_setprop_cell(fdt, name, "clock-frequency", 20000000);
    qemu_fdt_setprop_string(fdt, name, "compatible", "fixed-clock");
    qemu_fdt_setprop_cell(fdt, name, "#clock-cells", 0x0);
    g_free(name);
    
    
    name = g_strdup_printf("/soc/serial@%lx",
        (long)memmap[RATONA_UART0].base);
    qemu_fdt_add_subnode(fdt, name);
    qemu_fdt_setprop_string(fdt, name, "compatible", "sifive,uart0");
    qemu_fdt_setprop_cells(fdt, name, "reg",
        0x0, memmap[RATONA_UART0].base,
        0x0, memmap[RATONA_UART0].size);
    qemu_fdt_setprop_cells(fdt, name, "clocks", pbus_clock_phandle);
    qemu_fdt_setprop_cell(fdt, name, "interrupt-parent", plic_phandle[0]);
    qemu_fdt_setprop_cell(fdt, name, "interrupts", RATONA_UART0_IRQ);
    g_free(name);

    qemu_fdt_add_subnode(fdt, "/chosen");
    qemu_fdt_setprop_string(fdt, "/chosen", "bootargs", "console=hvc0 earlycon=sbi");
update_bootargs:
    if (cmdline) {
        //qemu_fdt_setprop_string(fdt, "/chosen", "bootargs", cmdline);
    }
}

static void ratona_machine_init(MachineState *machine)
{
    const struct MemmapEntry *memmap = ratona_memmap;
    RISCVRatonaState *s = RISCV_RATONA_MACHINE(machine);
    MemoryRegion *system_memory = get_system_memory();
    MemoryRegion *main_mem = g_new(MemoryRegion, 1);
    MemoryRegion *mask_rom = g_new(MemoryRegion, 1);
    char *plic_hart_config, *soc_name;
    size_t plic_hart_config_len;
    target_ulong start_addr = memmap[RATONA_DRAM].base + 0x1F00000;
    target_ulong firmware_end_addr, kernel_start_addr;
    uint32_t fdt_load_addr;
    uint64_t kernel_entry;
    DeviceState *mmio_plic;
    int i, j, base_hartid, hart_count;

    /* Check socket count limit */
    if (RATONA_SOCKETS_MAX < riscv_socket_count(machine)) {
        error_report("number of sockets/nodes should be less than %d",
            RATONA_SOCKETS_MAX);
        exit(1);
    }

    /* Initialize sockets */
    mmio_plic = NULL;
    for (i = 0; i < riscv_socket_count(machine); i++) {
        if (!riscv_socket_check_hartids(machine, i)) {
            error_report("discontinuous hartids in socket%d", i);
            exit(1);
        }

        base_hartid = riscv_socket_first_hartid(machine, i);
        if (base_hartid < 0) {
            error_report("can't find hartid base for socket%d", i);
            exit(1);
        }

        hart_count = riscv_socket_hart_count(machine, i);
        if (hart_count < 0) {
            error_report("can't find hart count for socket%d", i);
            exit(1);
        }

        soc_name = g_strdup_printf("soc%d", i);
        object_initialize_child(OBJECT(machine), soc_name, &s->soc[i],
                                TYPE_RISCV_HART_ARRAY);
        g_free(soc_name);
        object_property_set_str(OBJECT(&s->soc[i]), "cpu-type",
                                machine->cpu_type, &error_abort);
        object_property_set_int(OBJECT(&s->soc[i]), "hartid-base",
                                base_hartid, &error_abort);
        object_property_set_int(OBJECT(&s->soc[i]), "num-harts",
                                hart_count, &error_abort);
        sysbus_realize(SYS_BUS_DEVICE(&s->soc[i]), &error_abort);

        /* Per-socket CLINT */
        sifive_clint_create(
            memmap[RATONA_CLINT].base + i * memmap[RATONA_CLINT].size,
            memmap[RATONA_CLINT].size, base_hartid, hart_count,
            SIFIVE_SIP_BASE, SIFIVE_TIMECMP_BASE, SIFIVE_TIME_BASE,
            SIFIVE_CLINT_TIMEBASE_FREQ, true);

        /* Per-socket PLIC hart topology configuration string */
        plic_hart_config_len =
            (strlen(RATONA_PLIC_HART_CONFIG) + 1) * hart_count;
        plic_hart_config = g_malloc0(plic_hart_config_len);
        for (j = 0; j < hart_count; j++) {
            if (j != 0) {
                strncat(plic_hart_config, ",", plic_hart_config_len);
            }
            strncat(plic_hart_config, RATONA_PLIC_HART_CONFIG,
                plic_hart_config_len);
            plic_hart_config_len -= (strlen(RATONA_PLIC_HART_CONFIG) + 1);
        }

        /* Per-socket PLIC */
        s->plic[i] = sifive_plic_create(
            memmap[RATONA_PLIC].base + i * memmap[RATONA_PLIC].size,
            plic_hart_config, base_hartid,
            RATONA_PLIC_NUM_SOURCES,
            RATONA_PLIC_NUM_PRIORITIES,
            RATONA_PLIC_PRIORITY_BASE,
            RATONA_PLIC_PENDING_BASE,
            RATONA_PLIC_ENABLE_BASE,
            RATONA_PLIC_ENABLE_STRIDE,
            RATONA_PLIC_CONTEXT_BASE,
            RATONA_PLIC_CONTEXT_STRIDE,
            memmap[RATONA_PLIC].size);
        g_free(plic_hart_config);

        /* Try to use different PLIC instance based device type */
        if (i == 0) {
            mmio_plic = s->plic[i];
        }
    }

    /* register system main memory (actual RAM) */
    memory_region_init_ram(main_mem, NULL, "riscv_ratona_board.ram",
                           machine->ram_size, &error_fatal);
    memory_region_add_subregion(system_memory, memmap[RATONA_DRAM].base,
        main_mem);

    /* create device tree */
    create_fdt(s, memmap, machine->ram_size, machine->kernel_cmdline);

    /* boot rom */
    memory_region_init_rom(mask_rom, NULL, "riscv_ratona_board.mrom",
                           memmap[RATONA_MROM].size, &error_fatal);
    memory_region_add_subregion(system_memory, memmap[RATONA_MROM].base,
                                mask_rom);

    firmware_end_addr = riscv_find_and_load_firmware(machine, BIOS_FILENAME,
                                                     start_addr, NULL);

    if (machine->kernel_filename) {
        kernel_start_addr = riscv_calc_kernel_start_addr(machine,
                                                         firmware_end_addr);

        kernel_entry = riscv_load_kernel(machine->kernel_filename,
                                         kernel_start_addr, NULL);

        if (machine->initrd_filename) {
            hwaddr start;
            hwaddr end = riscv_load_initrd(machine->initrd_filename,
                                           machine->ram_size, kernel_entry,
                                           &start);
            qemu_fdt_setprop_cell(s->fdt, "/chosen",
                                  "linux,initrd-start", start);
            qemu_fdt_setprop_cell(s->fdt, "/chosen", "linux,initrd-end",
                                  end);
        }
    } else {
       /*
        * If dynamic firmware is used, it doesn't know where is the next mode
        * if kernel argument is not set.
        */
        kernel_entry = 0;
    }

    /* Compute the fdt load address in dram */
    fdt_load_addr = riscv_load_fdt(memmap[RATONA_DRAM].base,
                                   machine->ram_size, s->fdt);
    /* load the reset vector */
    riscv_setup_rom_reset_vec(start_addr, ratona_memmap[RATONA_MROM].base,
                              ratona_memmap[RATONA_MROM].size, kernel_entry,
                              fdt_load_addr, s->fdt);

    sifive_uart_create(system_memory, ratona_memmap[RATONA_UART0].base,
        serial_hd(0), qdev_get_gpio_in(DEVICE(mmio_plic), RATONA_UART0_IRQ));
}

static void ratona_machine_instance_init(Object *obj)
{
}

static void ratona_machine_class_init(ObjectClass *oc, void *data)
{
    MachineClass *mc = MACHINE_CLASS(oc);

    mc->desc = "RISC-V Ratona board";
    mc->init = ratona_machine_init;
    mc->max_cpus = RATONA_CPUS_MAX;
    mc->default_cpu_type = RATONA_CPU;
    mc->pci_allow_0_address = true;
    mc->possible_cpu_arch_ids = riscv_numa_possible_cpu_arch_ids;
    mc->cpu_index_to_instance_props = riscv_numa_cpu_index_to_props;
    mc->get_default_cpu_node_id = riscv_numa_get_default_cpu_node_id;
    mc->numa_mem_supported = true;
}

static const TypeInfo ratona_machine_typeinfo = {
    .name       = MACHINE_TYPE_NAME("ratona"),
    .parent     = TYPE_MACHINE,
    .class_init = ratona_machine_class_init,
    .instance_init = ratona_machine_instance_init,
    .instance_size = sizeof(RISCVRatonaState),
};

static void ratona_machine_init_register_types(void)
{
    type_register_static(&ratona_machine_typeinfo);
}

type_init(ratona_machine_init_register_types)
