const MEMORY_SIZE: usize = 0x1000;

// The digits are stored in a compact way, taking advantage of the fact that
// some digits' final rows are the same as other digits' initial rows.
const DIGITS_BEGIN: u16 = 0x10;
const DIGIT_OFFSETS: [u16; 16] = [41, 0, 31, 29, 43, 20, 35, 22, 37, 27, 39, 5, 16, 9, 33, 14];
#[rustfmt::skip]
const DIGIT_DATA: &[u8] = &[
    0x20, 0x60, 0x20, 0x20, 0x70, 0xe0, 0x90, 0xe0,
    0x90, 0xe0, 0x90, 0x90, 0x90, 0xe0, 0xf0, 0x80,
    0xf0, 0x80, 0x80, 0x80, 0xf0, 0x80, 0xf0, 0x10,
    0xf0, 0x40, 0x40, 0xf0, 0x90, 0xf0, 0x10, 0xf0,
    0x10, 0xf0, 0x80, 0xf0, 0x80, 0xf0, 0x90, 0xf0,
    0x90, 0xf0, 0x90, 0x90, 0x90, 0xf0, 0x10, 0x10,
];

fn default_image() -> [u8; MEMORY_SIZE] {
    let mut image = [0; MEMORY_SIZE];
    image[DIGITS_BEGIN as usize..DIGITS_BEGIN as usize + DIGIT_DATA.len()]
        .copy_from_slice(DIGIT_DATA);
    image
}

pub struct Interpreter {
    memory: [u8; MEMORY_SIZE],
    registers: [u8; 16],
    vi: u16,
    pc: u16,
    sp: u8,
    keyboard: u16,
    audio_timer: u8,
    timer: u8,
    new_key: u8, // 0xff is the default state
    //              0xfe means waiting for a key press
    //              0x1X means waiting for a release of X
    random_state: u16,
    stack: [u16; 16],
    display: [u8; 256], // display[cx + y*8]
}

impl core::fmt::Debug for Interpreter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Interpreter")?;
        write!(f, " I={:04x}", self.vi)?;
        write!(f, " pc={:04x}", self.pc)?;
        write!(f, " sp={:02x}", self.sp)?;
        write!(f, " keyboard={:08b}", self.keyboard)?;
        write!(f, " stack={:02x?}", self.stack)?;
        write!(f, ">")
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        let memory = default_image();
        Self {
            memory,
            registers: [0; 16],
            vi: 0,
            pc: 0x200,
            sp: 255,
            keyboard: 0,
            new_key: 0xff,
            audio_timer: 0,
            timer: 0,
            stack: [0; 16],
            random_state: 0,
            display: [0; 256],
        }
    }
}

#[inline(always)]
const fn n0(value: u16) -> u8 {
    (value >> 12) as u8
}

#[inline(always)]
const fn n1(value: u16) -> u8 {
    ((value >> 8) & 0xf) as u8
}

#[inline(always)]
const fn n2(value: u16) -> u8 {
    ((value >> 4) & 0xf) as u8
}

#[inline(always)]
const fn n3(value: u16) -> u8 {
    (value & 0xf) as u8
}

// Breakpoints

pub trait Breakpoints {
    fn should_break(&self, adr: u16) -> bool;
}

impl Breakpoints for u16 {
    #[inline(always)]
    fn should_break(&self, adr: u16) -> bool {
        adr == *self
    }
}

impl Breakpoints for [u16] {
    #[inline(always)]
    fn should_break(&self, adr: u16) -> bool {
        self.contains(&adr)
    }
}

impl<A, B> Breakpoints for (A, B)
where
    A: Breakpoints,
    B: Breakpoints,
{
    #[inline(always)]
    fn should_break(&self, adr: u16) -> bool {
        self.0.should_break(adr) || self.1.should_break(adr)
    }
}

impl Breakpoints for () {
    #[inline(always)]
    fn should_break(&self, _adr: u16) -> bool {
        false
    }
}

// Interpreter

impl Interpreter {
    #[inline(always)]
    pub fn pc(&self) -> u16 {
        self.pc
    }

    pub fn set_keyboard(&mut self, keyboard: u16) {
        if self.new_key & 0xf0 == 0x10 && (keyboard & (1u16 << (self.new_key & 0xf)) == 0) {
            self.new_key &= 0xf;
        }

        let new_pressed = keyboard & !self.keyboard;
        if new_pressed != 0 && self.new_key == 0xfe {
            let p = new_pressed.ilog2() as u8;
            self.new_key = 0x10 | p;
        }

        self.keyboard = keyboard;
    }

    pub fn set_seed(&mut self, seed: u16) {
        self.random_state = seed;
    }

    #[inline(always)]
    fn advance_random(&mut self) -> u8 {
        self.random_state = self.random_state.wrapping_mul(5).wrapping_add(69);
        ((self.random_state >> 6) & 0xff) as u8
    }

    #[inline(always)]
    pub fn tick(&mut self) {
        self.timer = self.timer.saturating_sub(1);
        self.audio_timer = self.audio_timer.saturating_sub(1);
    }

    #[inline(always)]
    pub fn is_playing_sound(&self) -> bool {
        self.audio_timer > 0
    }

    #[inline(always)]
    pub fn display_snapshot(&self) -> [u8; 256] {
        self.display
    }

    #[inline(always)]
    fn reg(&self, at: u8) -> u8 {
        // SAFETY: the size of registers is 16
        unsafe { *self.registers.get_unchecked((at & 0xf) as usize) }
    }

    #[inline(always)]
    fn set_reg(&mut self, at: u8, value: u8) {
        // SAFETY: the size of registers is 16
        unsafe { *self.registers.get_unchecked_mut((at & 0xf) as usize) = value }
    }

    pub fn load_rom(&mut self, rom: &[u8]) {
        let start_pos = 0x200;
        if rom.len() >= self.memory.len() - start_pos {
            // TODO: proper error reporting
            panic!("ROM too large");
        }
        self.memory[0x200..0x200 + rom.len()].copy_from_slice(rom);
    }

    pub fn execute_bounded(&mut self, limit: u64, bp: impl Breakpoints) -> u64 {
        let pc_limit = (self.memory.len() - 1) as u16;
        for i in 0..limit {
            if self.pc >= pc_limit {
                // TODO: proper error reporting
                panic!("pc too large")
            }

            // SAFETY: we just checked the bounds
            let instr_high = unsafe { *self.memory.get_unchecked(self.pc as usize) };
            let instr_low = unsafe { *self.memory.get_unchecked(self.pc as usize + 1) };
            let instr = ((instr_high as u16) << 8) + (instr_low as u16);

            let old_pc = self.pc;
            self.pc = self.exec_instruction(instr);
            if self.pc == old_pc {
                return i;
            }

            if bp.should_break(self.pc) {
                return i + 1;
            }
        }
        limit
    }

    #[inline(always)]  // Load-bearing inline: 2x overall speedup on 1dcell_benchmark
    pub fn exec_instruction(&mut self, instr: u16) -> u16 {
        // For reference, see https://github.com/mattmikolay/chip-8/wiki/CHIP‐8-Instruction-Set

        match n0(instr) {
            0 => {
                // 0NNN: Execute machine language subroutine at address NNN

                if instr == 0x00e0 {
                    // 00E0: clear display
                    self.display.fill(0);

                } else if instr == 0x00ee {
                    if self.sp >= 16 {
                        // TODO: proper error reporting
                        todo!("Stack underflow at pc={}", self.pc)
                    }
                    // SAFETY: just checked the bounds
                    let ret_adr = unsafe {self.stack.get_unchecked_mut(self.sp as usize)}.wrapping_add(2);
                    self.sp = self.sp.wrapping_sub(1);
                    return ret_adr;

                } else {
                    let syscall = instr & 0xfff;
                    if syscall & 0x0f00 == 0x0f00 {
                        println!();
                        println!("{0:02x} {1:?}", syscall & 0xff, &self);
                        self.core_dump();
                        println!();
                    } else {
                        // TODO: proper error reporting
                        todo!("Unknown syscall={syscall:04x}")
                    }
                }
            }
            1 => {
                return instr & 0xfff;
            }
            2 => {
                let addr = instr & 0xfff;
                self.sp = self.sp.wrapping_add(1);
                if self.sp >= 16 {
                    // TODO: proper error reporting
                    todo!("Stack overflow at CALL {addr}")
                }
                // SAFETY: just checked the bounds
                unsafe { *self.stack.get_unchecked_mut(self.sp as usize) = self.pc };
                return addr;
            }
            3 => {
                if self.reg(n1(instr)) == (instr & 0xff) as u8 {
                    return self.pc.wrapping_add(4)
                }
            }
            4 => {
                if self.reg(n1(instr)) != (instr & 0xff) as u8 {
                    return self.pc.wrapping_add(4)
                }
            }
            5 => {
                if n3(instr) != 0 {
                    // TODO: proper error reporting
                    todo!("Invalid instruction {:04x} at pc={:04x}", instr, self.pc)
                }
                if self.reg(n1(instr)) == self.reg(n2(instr)) {
                    return self.pc.wrapping_add(4)
                }
            }
            6 => {
                self.set_reg(n1(instr), (instr & 0xff) as u8);
            }
            7 => {
                let adr = n1(instr);
                let new_value = self.reg(adr).wrapping_add((instr & 0xff) as u8);
                self.set_reg(adr, new_value);
            }
            8 => {
                let vx = n1(instr);
                let x = self.reg(vx);
                let y = self.reg(n2(instr));
                match n3(instr) {
                    0 => self.set_reg(vx, y),
                    1 => self.set_reg(vx, x | y),
                    2 => self.set_reg(vx, x & y),
                    3 => self.set_reg(vx, x ^ y),
                    4 => {
                        let (new_x, carry) = x.overflowing_add(y);
                        self.set_reg(vx, new_x);
                        self.set_reg(15, carry as u8);
                    },
                    5 => {
                        let (new_x, borrow) = x.overflowing_sub(y);
                        self.set_reg(vx, new_x);
                        self.set_reg(15, (!borrow) as u8);
                    },
                    6 => {
                        self.set_reg(vx, x >> 1);
                        self.set_reg(15, (x & 1 != 0) as u8);
                    },
                    7 => {
                        let (new_x, borrow) = y.overflowing_sub(x);
                        self.set_reg(vx, new_x);
                        self.set_reg(15, (!borrow) as u8);
                    },
                    0xE => {
                        self.set_reg(vx, x << 1);
                        self.set_reg(15, (x & 0b10000000 != 0) as u8);
                    },
                    _ => {
                        // TODO: proper error reporting
                        todo!("Invalid instruction {:04x} at pc={:04x}", instr, self.pc)
                    }

                }
            }
            9 => {
                if n3(instr) != 0 {
                    // TODO: proper error reporting
                    todo!("Invalid instruction {:04x} at pc={:04x}", instr, self.pc)
                }
                if self.reg(n1(instr)) != self.reg(n2(instr)) {
                    return self.pc.wrapping_add(4)
                }
            }
            0xA => {
                self.vi = instr & 0xfff;
            }
            0xB => {
                return (instr & 0xfff).wrapping_add(self.reg(0) as u16);
            }
            0xC => {
                let random = self.advance_random();
                self.set_reg(n1(instr), random & ((instr & 0xff) as u8));
            }
            0xD => {
                let x = self.reg(n1(instr)) as usize % 64;
                let y = self.reg(n2(instr)) as usize % 32;
                let sprite_height = (n3(instr) as usize).min((32 + 1) - y);

                let xcell = x >> 3;
                let xbit = (x & 0b0000_0111) as u8;

                let mut idx = self.vi as usize;
                let mut vf = 0u8;
                for j in y..(y + sprite_height) {
                    idx %= MEMORY_SIZE;

                    // SAFETY: just moduloed the index
                    let row = unsafe { self.memory.get_unchecked(idx) };

                    let left_mask = row >> xbit;
                    // SAFETY: we did all the bounds checking above
                    let ptr = unsafe { self.display.get_unchecked_mut(xcell + j*8)};
                    if (*ptr) & left_mask != 0 {
                        vf = 1;
                    }
                    *ptr ^= left_mask;

                    if xcell != 7 && xbit != 0 {
                        let right_mask = row.wrapping_shl((8 - xbit) as u32);
                        // SAFETY: we did all the bounds checking above
                        let ptr = unsafe { self.display.get_unchecked_mut(xcell + 1 + j*8)};
                        if (*ptr) & right_mask != 0 {
                            vf = 1;
                        }
                        *ptr ^= right_mask;
                    }
                    idx += 1;
                }
                self.set_reg(15, vf);
            }
            0xE => {
                match (instr & 0xff) as u8 {
                    0x9e => {
                        let reg = self.reg(n1(instr)) & 0xf;
                        if self.keyboard & (1 << reg) != 0 {
                            return self.pc + 4
                        }
                    }
                    0xa1 => {
                        let reg = self.reg(n1(instr)) & 0xf;
                        if self.keyboard & (1 << reg) == 0 {
                            return self.pc + 4
                        }
                    }
                    _ => {
                        // TODO: proper error reporting
                        todo!("Invalid instruction {:04x} at pc={:04x}", instr, self.pc)
                    }
                }
            }
            _ /* F */ => {
                match (instr & 0xff) as u8 {
                    0x07 => {
                        self.set_reg(n1(instr), self.timer);
                    },
                    0x0a => {
                        if self.new_key & 0xf0 != 0 {
                            if self.new_key == 0xff {
                                self.new_key = 0xfe;
                            }
                            return self.pc
                        }
                        let reg = n1(instr);
                        self.set_reg(reg, self.new_key);
                        self.new_key = 0xff;
                    },
                    0x15 => {
                        self.timer = self.reg(n1(instr));
                    },
                    0x18 => {
                        self.audio_timer = self.reg(n1(instr));
                    },
                    0x1e => {
                        self.vi = self.vi.wrapping_add(self.reg(n1(instr)) as u16);
                    },
                    0x29 => {
                        self.vi = DIGIT_OFFSETS[(self.reg(n1(instr)) & 0x0f) as usize] + DIGITS_BEGIN;
                    }
                    0x33 => {
                        let val = self.reg(n1(instr));
                        let vi = self.vi as usize;
                        let idx0 = vi % MEMORY_SIZE;
                        let idx1 = (vi + 1) % MEMORY_SIZE;
                        let idx2 = (vi + 2) % MEMORY_SIZE;

                        // SAFETY: just did the bounds check
                        unsafe {
                            *self.memory.get_unchecked_mut(idx0) = val / 100;
                            *self.memory.get_unchecked_mut(idx1) = (val % 100) / 10;
                            *self.memory.get_unchecked_mut(idx2) = val % 10;
                        };
                    }
                    0x55 => {
                        // Saving registers
                        let bound = n1(instr) as usize;
                        for i in 0..(bound + 1) {
                            self.memory[(self.vi + i as u16) as usize % MEMORY_SIZE] = self.registers[i];
                        }
                        self.vi = self.vi + bound as u16 + 1;
                    }
                    0x65 => {
                        // Loading registers
                        let bound = n1(instr) as usize;

                        for i in 0..(bound + 1) {
                            self.registers[i] = self.memory[(self.vi + i as u16) as usize % MEMORY_SIZE];
                        }
                        self.vi = self.vi + bound as u16 + 1;
                    }
                    _ => {
                        // TODO: proper error reporting
                        todo!("Invalid instruction {:04x} at pc={:04x}", instr, self.pc)
                    }
                }
            }
        }

        self.pc.wrapping_add(2)
    }

    pub fn debug_screen(&self) {
        debug_screen(&self.display);
    }

    pub fn core_dump(&self) {
        core_dump(&self.memory)
    }
}

fn core_dump(memory: &[u8; MEMORY_SIZE]) {
    let row_size = 32;
    println!("BEGIN CORE DUMP");
    let mut was_all_zeros = false;
    for j in 0..(memory.len() / row_size) {
        let row = &memory[j * row_size..(j + 1) * row_size];
        let all_zeros = row.iter().all(|x| *x == 0);
        if all_zeros && !was_all_zeros {
            println!("        ...");
        }
        was_all_zeros = all_zeros;
        if !all_zeros {
            println!("  {:04x} {:02x?}", j * row_size, row);
        }
    }
    println!("END CORE DUMP");
}

fn debug_screen(display: &[u8; 256]) {
    fn to_chars(b: u8) -> String {
        let mut rv = String::new();
        for i in (0..8).rev() {
            rv += if b & (1 << i) == 0 { "  " } else { "██" };
        }
        rv
    }

    let mut lines = Vec::with_capacity(32);
    for j in 0..32 {
        let mut line = String::with_capacity(64);
        for i in 0..8 {
            line += &to_chars(display[i + j * 8]);
        }
        assert!(line.len() >= 64);
        lines.push(line);
    }

    println!("{}", lines.join("\n"))
}
