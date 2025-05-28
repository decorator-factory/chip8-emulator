use ::emu_backend::Interpreter;
use std::time::Instant;

fn main() {
    // This binary runs a benchmarking ROM
    let mut vm = Interpreter::default();

    let bs: &[u8] = std::hint::black_box(include_bytes!("../../../roms/1dcell_benchmark.ch8"));

    vm.load_rom(bs);
    let start = Instant::now();
    let count = vm.execute_bounded(2_000_000_000, ());
    let dt = start.elapsed();
    vm.debug_screen();

    let mips = (count as f64 / dt.as_secs_f64()) / 1_000_000.;

    println!("pc={} count={} mips={}", vm.pc(), count, mips);
}
