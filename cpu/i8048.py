# Copyright (c) 2013 Bertrand Janin <b@janin.com>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

"""
The Intel 8048 (MCS-48) CPU was used in the following devices:

    - Magnavox Odyssey2 (Philips Videopac / G7000)
    - Korg Trident series
    - Korg Poly-61
    - Roland Jupiter-4
    - Roland ProMars
"""

from decorator import decorator


class instruction(object):

    def __init__(self, *args):
        self.instructions = args

    def __call__(self, f):
        def _instruction_wrapper(self):
            return f(self)

        # Keep a list of the handled instruction on the function itself. It
        # will be used by the instruction census function below.
        _instruction_wrapper.instructions = self.instructions
        return _instruction_wrapper


DATA_LOCATION = 0x400
DATA_SIZE = 0x800



class CPU_i8048(object):

    def __init__(self):
        # 1KB ROM (0x0000 in memory)
        self.rom = "\0" * 1024

        # 2KB Game (0x0400 in memory)
        self.data = "\0" * 2048

        # 64 bytes of RAM (XXX: where?)
        self.ram = [0] * 64

        # External RAM, by default None but it can be plugged on.
        self.external_ram = None

        # Accumulator, used by most instructions as argument or result.
        self.accumulator = 0

        # Program Counter, points to the location in the program code (ROM).
        self.program_counter = 0
        # 16-bit * 8 (program counter 12 bits + 4 DSW)
        self.program_stack = [0] * 8
        # Program Status Word.
        #  - b0, b1, b2 -> Stack Pointer
        #  - b3 -> always 1
        #  - b4 -> register bank select (BS)
        #  - b5 -> flag 0 (F0)
        #  - b6 -> auxillary carry (AC)
        #  - b7 -> carry (CY)
        self.program_status_word = 0

        # Memory Bank. This value is used during CALL and JMP, it adjusts bit
        # 11 of the Program Counter.
        self.memory_bank = 0

        # Incremented by each instructions according to the number of cycles
        # they use.
        self.cycles = 0
        # Last number of cycles during which the timer was incremented.
        self.last_timer_cycles = 0
        # Actual timer count.
        self.timer = 0
        # This triggers the use of the above variables.
        self.timer_is_enabled = False

        self.ports = [0] * 2

        # Internal map of all the bytes to functions, populated by the
        # instruction decorator.
        self._instructions = {}
        self._instruction_census()

    def run(self):
        while True:
            b = self.get_byte_from_address(self.program_counter)
            self.run_instruction(b)

    def _instruction_census(self):
        """Explore the instance to find all the instructions handlers.

        Only the objects starting with do_ are explored.

        """
        for attrname in dir(self):
            if not attrname.startswith("do_"):
                continue
            obj = getattr(self, attrname)
            instructions = getattr(obj, "instructions", None)
            if instructions:
                for byte in instructions:
                    self._instructions[byte] = obj

    def load_bios(self, filepath):
        with open(filepath, "rb") as fp:
            self.rom = fp.read()

    def load_data(self, filepath):
        with open(filepath, "rb") as fp:
            self.data = fp.read()

    def run_instruction(self, b):
        if b not in self._instructions:
            raise ValueError("Instruction not found: {} - {}"
                             .format(hex(b), bin(b)))

        func = self._instructions[b]
        func()

        if self.timer_is_enabled:
            if self.cycles - self.last_timer_cycles >= 32:
                self.timer += 1

    def set_register(self, number, value):
        """Sets the value of a register.

        Registers are the first 8 bytes of the RAM if the register bank is 0,
        they are on bytes 24-31 if register bank is 1.

        """
        register_bank_offset = self.get_register_bank() * 24
        self.ram[register_bank_offset + number] = value

    def get_register(self, number):
        """Reads the value of a register."""

        register_bank_offset = self.get_register_bank() * 24
        return self.ram[register_bank_offset + number]

    def set_register_bank(self, value):
        """Set bit 4 of the PSW."""

        if value == 0:
            self.program_status_word = self.program_status_word & 0b11110111
        elif value == 1:
            self.program_status_word = self.program_status_word | 0b00001000
        else:
            raise ValueError("Register Bank Select can only be set to 0 or 1.")

    def get_register_bank(self):
        """Get bit 4 of the PSW."""

        return (self.program_status_word & 0b00001000) >> 3

    def get_byte_from_address(self, offset):
        if offset < DATA_LOCATION:
            b = ord(self.rom[offset])
        elif offset < (DATA_LOCATION + DATA_SIZE):
            b = ord(self.data[offset - DATA_LOCATION])
        else:
            raise ValueError("Program Counter on unknown program location: {}"
                             .format(hex(offset)))

        return b

    def get_stack_pointer(self):
        """Return the first three bits of the PSW."""
        return self.program_status_word & 0b111;

    def set_stack_pointer(self, value):
        """Set the first three bits of the PSW."""
        mask = 0xFF & value
        self.program_status_word = self.program_status_word & mask

    def get_carry(self):
        """Return the first three bits of the PSW."""
        return self.program_status_word & 0b10000000;

    def set_carry(self, value):
        """Set the first three bits of the PSW."""
        if value:
            self.program_status_word |= 0b10000000
        else:
            self.program_status_word &= 0b01111111

    def push_to_stack(self):
        """Save the Program Counter and bits 4-7 of the PSW to the stack."""
        sp = self.get_stack_pointer()
        psw_bits = (0b11110000 & self.program_status_word) << 8
        stack_value = psw_bits + (self.program_counter & 0b111111111111)
        self.program_stack[sp] = stack_value
        self.set_stack_pointer(sp + 1)

    def pop_from_stack(self):
        """Get the Program Counter and bits 4-7 of the PSW from the stack."""
        sp = self.get_stack_pointer()
        stack_value = self.program_stack[sp]
        psw_bits = (0b1111000000000000 & stack_value) >> 12
        program_counter = stack_value & 0b0000111111111111
        self.set_stack_pointer(sp - 1)
        return program_counter, psw_bits

    def print_asm(self, asm, comment=None):
        """Print an assembly message from an instruction.

        This function prefixes the message with the address.

        """
        asm = "{:04X} : {}".format(self.program_counter, asm)

        if comment:
            tabs = "\t" * (4 - len(asm) / 8)
            asm += " {}; {}".format(tabs, comment)

        print(asm)


    #
    # Instruction Set
    #
    # Each instruction is responsible for incrementing the program counter and
    # increment the cycles as needed.
    #
    @instruction(0x60, 0x61)
    def do_ADD_A_atRn(self):
        """Add Data Memory Contents to Accumulator.

        The contents of the resident data memory location addressed by register
        'r' bits 0-5 are added to the accumulator. Carry is affected.

        """
        b0 = self.get_byte_from_address(self.program_counter)

        register_number = b0 & 0b00000001

        self.print_asm("ADD A,@R{}".format(register_number))

        addr = self.get_register(register_number) & 0x11111

        # Add and make sure we keep it real (8-bit)
        self.accumulator += self.ram[addr]
        self.set_carry(self.accumulator > 0xFF)
        self.accumulator &= 0xFF

        self.program_counter += 1
        self.cycles += 1

    @instruction(0x98, 0x99, 0x9A, 0x9B)
    def do_ANL_Pp_DATA(self):
        """Logical AND Port 1-2 With Immediate Mask.

        Data on port 'p' is logically ANDed with an immediate-specified mask.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        b1 = self.get_byte_from_address(self.program_counter + 1)

        port_number = (b0 & 0b00000011)
        data = b1

        self.ports[port_number] &= data

        self.print_asm("ANL P{}, #{:02X}H".format(port_number, data))

        self.program_counter += 2
        self.cycles += 2

    @instruction(0x14, 0x34, 0x54, 0x74, 0x94, 0xB4, 0xD4, 0xF4)
    def do_CALL(self):
        """Subroutine Call

        The Program Counter and PSW bits 4-7 are saved in the stack. The Stack
        Pointer (PSW bits 0-2) is updated and the program counter is pushed to
        the specified address.

        """
        self.push_to_stack()

        b0 = self.get_byte_from_address(self.program_counter)
        b1 = self.get_byte_from_address(self.program_counter + 1)

        # Strip the instruction identifier from the first byte.
        b0 = (b0 >> 5)

        addr = (self.memory_bank << 11) + (b0 << 8) + b1
        self.print_asm("CALL {:04X}H".format(addr))

        self.program_counter = addr
        self.cycles += 2

    @instruction(0x27)
    def do_CLR_A(self):
        """Clear Accumulator

        The content of the accumulator are cleared to zero.

        """
        self.print_asm("CLR A")
        self.accumulator = 0

        self.program_counter += 1
        self.cycles += 1

    @instruction(0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF)
    def do_DJNZ_Rn_ADDR(self):
        """Decrement Register and Test.

        Register 'n' is decremented and tested for zero. If the register
        contains all zeros, program control falls through to the next
        instruction. If the register contents are not zero, control jumps to
        the specified 'addres'.

        The address in this case must evaluate to 8-bits, that is, the jump
        must be to a location within the current 256-location page.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        b1 = self.get_byte_from_address(self.program_counter + 1)

        register_number = b0 & 0b00000111
        addr = b1

        # Decrement the register.
        value = self.get_register(register_number)
        self.set_register(register_number, value - 1)

        self.print_asm("DJNZ R{},ADDR8".format(register_number),
                       "value: {}".format(value - 1))

        # Test for zero.
        if value == 0:
            self.program_counter += 2
        else:
            self.program_counter &= 0xFF00
            self.program_counter |= (b1 & 0xFF)

        self.cycles += 2

    @instruction(0x08, 0x09, 0x0A, 0x0B)
    def do_IN_A_Pp(self):
        """Input Port or Data to Accumulator

        Data present on port 'p' is transferred (read) to the accumulator.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        port_number = (b0 & 0b00000011)

        self.print_asm("IN A,P{}".format(port_number),
                       "value: {}".format(self.accumulator))
        self.accumulator = self.ports[port_number]

        self.program_counter += 1
        self.cycles += 2

    @instruction(0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x11, 0x1F)
    def do_INC_Rn(self):
        """Increment Register

        As per the documentation (page 4-16), the register number is on the
        last three bits of this single-cycle instruction byte.

        """
        b0 = self.get_byte_from_address(self.program_counter)

        register_number = b0 & 0b00000111

        value = self.get_register(register_number)
        self.print_asm("INC R{}".format(register_number),
                       "value: {}+1".format(hex(value)))
        self.set_register(register_number, value + 1)

        self.program_counter += 1
        self.cycles += 1

    @instruction(0x04, 0x24, 0x44, 0x64, 0x84, 0xA4, 0xC4, 0xE4)
    def do_JMP(self):
        """Direct Jump Within 2K Block"""

        b0 = self.get_byte_from_address(self.program_counter)
        b1 = self.get_byte_from_address(self.program_counter + 1)

        # Strip the instruction identifier from the first byte.
        b0 = (b0 >> 5)

        addr = (self.memory_bank << 11) + (b0 << 8) + b1
        self.print_asm("JMP {:04X}H".format(addr))

        # XXX - Not sure I need to increment this...
        self.program_counter = addr + 1
        self.cycles += 2

    @instruction(0x23)
    def do_MOV_A_DATA(self):
        """Move Immediate Data to Accumulator

        The 8-bit value specified by 'data' is loaded in the accumulator.

        """
        data = self.get_byte_from_address(self.program_counter + 1)

        self.accumulator = data

        self.print_asm("MOV A, #{:02X}".format(data))

        self.program_counter += 2
        self.cycles += 2

    @instruction(0xF0, 0xF1)
    def do_MOV_A_atRn(self):
        """Move Data Memory Contents to Accumulator.

        The contents of the resident data memory location addressed by bits 0-5
        of register 'n' are moved to the accumulator.

        """
        b0 = self.get_byte_from_address(self.program_counter)

        register_number = b0 & 0b00000001

        self.print_asm("MOV A, @R{}".format(register_number))

        addr = self.get_register(register_number) & 0x11111
        self.accumulator = self.ram[addr]

        self.program_counter += 1
        self.cycles += 1

    @instruction(0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF)
    def do_MOV_Rn_A(self):
        """Move Accumulator Content to Register

        As per the documentation, the register number is on the last three bits
        of this single-cycle instruction byte.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        register_number = b0 & 0b00000111
        self.print_asm("MOV R{}, A".format(register_number),
                       "value: {}".format(hex(self.accumulator)))
        self.set_register(register_number, self.accumulator)

        self.program_counter += 1
        self.cycles += 1

    @instruction(0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF)
    def do_MOV_A_Rn(self):
        """Move Register Content to Accumulator

        The register number is on the last three bits of this single-cycle
        instruction byte.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        register_number = b0 & 0b00000111
        self.print_asm("MOV A, R{}".format(register_number),
                       "value: {}".format(hex(self.accumulator)))
        self.accumulator = self.get_register(register_number)

        self.program_counter += 1
        self.cycles += 1

    @instruction(0xA0, 0xA1)
    def do_MOV_atRn_A(self):
        """Move Accumulator Contents to Data Memory

        The contents of the accumulator are moved to the resident data memory
        location whose address is specified by bits 0-5 of register 'r'.
        Register 'r' contents are unaffected.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        register_number = b0 & 0b00000001

        self.print_asm("MOV @R{}, A".format(register_number),
                       "value: {:02X}H".format(self.accumulator))

        register_data = self.get_register(register_number)
        addr = register_data & 0b11111

        self.ram[addr] = self.accumulator

        self.program_counter += 1
        self.cycles += 1

    @instruction(0xB0, 0xB1)
    def do_MOV_atRn_DATA(self):
        """Move Immediate Date to Data Memory.

        The 8-bit value specified by 'data' is moved to the resident data
        memory location addressed by register 'r', bit 0-5. In clear, it's used
        to set a RAM location to a given value.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        b1 = self.get_byte_from_address(self.program_counter + 1)

        register_number = b0 & 0b00000001
        data = b1

        self.print_asm("MOV @R{}, #{:02X}H"
                       .format(register_number, self.accumulator))

        register_data = self.get_register(register_number)
        addr = register_data & 0b11111

        self.ram[addr] = data

        self.program_counter += 2
        self.cycles += 2

    @instruction(0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF)
    def do_MOV_Rn_DATA(self):
        """Move Immediate Data to Register.

        The 8-bit value specified by 'data' is moved to register 'n'.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        b1 = self.get_byte_from_address(self.program_counter + 1)

        register_number = b0 & 0b00000111
        data = b1

        self.print_asm("MOV R{}, #{:02X}H".format(register_number, data))
        self.set_register(register_number, data)

        self.program_counter += 2
        self.cycles += 2

    @instruction(0xA3)
    def do_MOVP_A_atA(self):
        """Move Current Page Data to Accumulator.

        The contents of the program memory location addressed by the
        accumulator are moved to the accumulator. Only bits 0-7 of the
        program counter are affected, limiting the program memory reference
        to the current page. The program counter is restored following this
        operation.

        For our purpose we don't need need to fsck with the program counter so
        we just simply update the accumulator.

        """
        # Ignore the first 8 bits of the program counter.
        page_addr = self.program_counter & 0xFF00

        # The doc says that if this instruction is found on location 255 (0xFF)
        # of a program memory page, @A addresses a location in the following
        # page. So accordingly, we increment the data_addr if this condition is
        # True.
        if self.program_counter & 0x00FF == 0xFF:
            data_addr = page_addr + 0x0100

        # Get the local address based on the first 8-bit of the accumulator.
        local_addr = self.accumulator & 0x000F

        # Put the accumulator in these 8 bits.
        data_addr = page_addr + local_addr

        # Get the data at this location (should be within the current page)
        data = self.get_byte_from_address(data_addr)

        self.print_asm("MOVP A, @A", "page_addr: {:04X}H + local_addr: {:04X},"
                                     " value: {:04X}H"
                                     .format(page_addr, local_addr, data))
        self.accumulator = data

        self.program_counter += 1
        self.cycles += 2

    @instruction(0x80, 0x81)
    def do_MOVX_A_atRn(self):
        """Move External-Data-Memory Contents to Accumulator

        The contents of the external data memory location addressed by register
        'n' are moved to the accumulator. Register 'n' contents are
        unaffected.

        All the MOVX instruction read/write the external RAM. If no external
        RAM is present this raises an exception.

        """
        if not self.external_ram:
            raise Exception("MOVX instruction only works with external RAM")

        b0 = self.get_byte_from_address(self.program_counter)

        register_number = b0 & 0b00000001

        data_addr = self.get_register(register_number)
        data = self.external_ram[data_addr]
        self.accumulator = data

        self.print_asm("MOVX A, @R{}".format(register_number),
                       "addr:{}, value: {}".format(hex(data_addr), hex(data)))

        self.program_counter += 1
        self.cycles += 2

    @instruction(0x90, 0x91)
    def do_MOVX_atR_A(self):
        """Move Accumulator Contents to External-Data-Memory.

        The contents of the accumulator are moved to the location addressed by
        register 'r'.

        All the MOVX instruction read/write the external RAM. If no external
        RAM is present this raises an exception.

        """
        if not self.external_ram:
            raise Exception("MOVX instruction only works with external RAM")

        b0 = self.get_byte_from_address(self.program_counter)

        register_number = b0 & 0b00000001
        data_addr = self.get_register(register_number)

        self.print_asm("MOVX @R{}, A".format(register_number),
                       "addr:{}".format(hex(data_addr)))

        self.external_ram[data_addr] = self.accumulator

        self.program_counter += 1
        self.cycles += 2

    @instruction(0x00)
    def do_NOP(self):
        """No Operation."""
        self.program_counter += 1
        self.cycles += 1

    @instruction(0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F)
    def do_ORL_A_Rn(self):
        """Logical OR Accumulator With Register Mask.

        Data in the accumulator is logically ORed with the mask contained in
        the working register 'r'.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        register_number = b0 & 0b00000001

        self.accumulator |= self.get_register(register_number)

        self.print_asm("ORL A, R{}".format(register_number))

        self.program_counter += 1
        self.cycles += 1

    @instruction(0x88, 0x89, 0x8A, 0x8B)
    def do_ORL_Pp_DATA(self):
        """Logical OR Port 1-2 With Immediate Mask.

        Data on port 'p' is logically ORed with an immediate-specified mask.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        b1 = self.get_byte_from_address(self.program_counter + 1)

        port_number = (b0 & 0b00000011)
        data = b1

        self.print_asm("ORL P{}, #{:02X}H".format(port_number, data))

        self.ports[port_number] |= data

        self.program_counter += 2
        self.cycles += 2

    @instruction(0x83)
    def do_RET(self):
        """Return Without PSW Restore.

        The stack pointer is decremented. The program counter is restored from
        the stack PSW 4-7 are not restored.

        """
        pc, psw_bits = self.pop_from_stack()

        self.print_asm("RET", "PC <- {:04X}".format(pc))

        # Nothing explicitly says we should increment the program counter
        # fetched from the stack, but if we don't, we probably just get stuck
        # in a loop. Quoting the documentation:
        #
        #     The processor updates the Program Counter by adding "1" to the
        #     counter each time it fetches an instruction, so that the Program
        #     Counter is always current (pointing to the next instruction).
        #
        # Our approach of incrementing at the end of each instruction might be
        # erroneous.
        self.program_counter = pc + 1

        self.cycles += 2

    @instruction(0xE7)
    def do_RL_A(self):
        """Rotate Left Without Carry

        The contents of the accumulator are rotated left one bit. Bit 7 is
        rotated into the bit 0 position.

        """
        right_bits = self.accumulator << 1
        left_bit = (self.accumulator & 0b10000000) >> 7

        self.accumulator = right_bits + left_bit

        self.program_counter += 1
        self.cycles += 1

    @instruction(0xE5, 0xF5)
    def do_SEL_MBk(self):
        """Select Memory Bank k

        The Memory Bank adjusts bit 11 of the Program Counter at JMP or CALL.
        With memory bank 0 (default), PC points to the 0-2047 range, with
        memory bank 1, PC points to the 2048-4095 range.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        self.memory_bank = b0 & 0b00000001
        self.print_asm("SEL MB{}".format(self.memory_bank))

        self.program_counter += 1
        self.cycles += 1

    @instruction(0xC5, 0xD5)
    def do_SEL_RBk(self):
        """Select Register Bank k

        PSW bit 4 is set to k. References to working registers 0-7 data
        memory locations 0-7 if k=0 and 24-31 if k=1. The default is 0.

        """
        b0 = self.get_byte_from_address(self.program_counter)
        self.set_register_bank(b0 & 0b00000001)
        self.print_asm("SEL RB{}".format(self.get_register_bank()))

        self.program_counter += 1
        self.cycles += 1

    @instruction(0x55)
    def do_STRT_T(self):
        """Start Timer

        Timer accumulation is initiated in the timer register. The register is
        incremented every 32 instruction cycles. The prescaler which counts the
        32 cycles is cleared but the timer register is not.

        """
        self.timer_is_enabled = True
        self.print_asm("STRT T")

        self.program_counter += 1
        self.cycles += 1

