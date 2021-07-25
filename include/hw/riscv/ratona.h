/*
 * QEMU RISC-V RATONA machine interface
 *
 * Copyright (c) 2017 SiFive, Inc.
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

#ifndef HW_RISCV_RATONA_H
#define HW_RISCV_RATONA_H

#include "hw/riscv/riscv_hart.h"
#include "hw/sysbus.h"
#include "hw/block/flash.h"
#include "qom/object.h"

#define RATONA_CPUS_MAX 8
#define RATONA_SOCKETS_MAX 8

#define TYPE_RISCV_RATONA_MACHINE MACHINE_TYPE_NAME("ratona")
typedef struct RISCVRatonaState RISCVRatonaState;
DECLARE_INSTANCE_CHECKER(RISCVRatonaState, RISCV_RATONA_MACHINE,
                         TYPE_RISCV_RATONA_MACHINE)

struct RISCVRatonaState {
    /*< private >*/
    MachineState parent;

    /*< public >*/
    RISCVHartArrayState soc[RATONA_SOCKETS_MAX];
    DeviceState *plic[RATONA_SOCKETS_MAX];
    PFlashCFI01 *flash[2];

    void *fdt;
    int fdt_size;
};

enum {
    RATONA_DEBUG,
    RATONA_MROM,
    RATONA_CLINT,
    RATONA_PLIC,
    RATONA_UART0,
    RATONA_DRAM
};

enum {
    RATONA_UART0_IRQ = 10,
    RATONA_RTC_IRQ = 11,
    RATONA_NDEV = 0x35 /* Arbitrary maximum number of interrupts */
};

#define RATONA_PLIC_HART_CONFIG "MS"
#define RATONA_PLIC_NUM_SOURCES 127
#define RATONA_PLIC_NUM_PRIORITIES 7
#define RATONA_PLIC_PRIORITY_BASE 0x04
#define RATONA_PLIC_PENDING_BASE 0x1000
#define RATONA_PLIC_ENABLE_BASE 0x2000
#define RATONA_PLIC_ENABLE_STRIDE 0x80
#define RATONA_PLIC_CONTEXT_BASE 0x200000
#define RATONA_PLIC_CONTEXT_STRIDE 0x1000
#define RATONA_PLIC_SIZE(__num_context) \
    (RATONA_PLIC_CONTEXT_BASE + (__num_context) * RATONA_PLIC_CONTEXT_STRIDE)

#define FDT_PCI_ADDR_CELLS    3
#define FDT_PCI_INT_CELLS     1
#define FDT_PLIC_ADDR_CELLS   0
#define FDT_PLIC_INT_CELLS    1
#define FDT_INT_MAP_WIDTH     (FDT_PCI_ADDR_CELLS + FDT_PCI_INT_CELLS + 1 + \
                               FDT_PLIC_ADDR_CELLS + FDT_PLIC_INT_CELLS)

#if defined(TARGET_RISCV32)
#define RATONA_CPU TYPE_RISCV_CPU_BASE32
#elif defined(TARGET_RISCV64)
#define RATONA_CPU TYPE_RISCV_CPU_BASE64
#endif

#endif
