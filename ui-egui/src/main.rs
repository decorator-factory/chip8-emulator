use std::{
    sync::mpsc,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

use egui::{self, Color32, Pos2, Rect, Sense, StrokeKind, Vec2};

#[derive(Debug, Clone)]
struct EmuUI {
    display: [u8; 256],
    instructions: u64,
}

#[derive(Debug, Clone)]
enum Update {
    Keyboard(u16),
    CoreDump,
}

struct MapSource<S, F>(S, F);

impl<S, F> Iterator for MapSource<S, F>
where
    S: Iterator<Item = f32>,
    F: Fn(f32) -> f32,
{
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(&self.1)
    }
}

impl<S, F> rodio::Source for MapSource<S, F>
where
    S: rodio::Source + Iterator<Item = f32>,
    F: Fn(f32) -> f32,
{
    fn current_frame_len(&self) -> Option<usize> {
        self.0.current_frame_len()
    }

    fn channels(&self) -> u16 {
        self.0.channels()
    }

    fn sample_rate(&self) -> u32 {
        self.0.sample_rate()
    }

    fn total_duration(&self) -> Option<Duration> {
        self.0.total_duration()
    }
}

fn make_source() -> impl rodio::Source<Item = f32> + Send + 'static {
    let sine = rodio::source::SineWave::new(120.);
    MapSource(sine, |x: f32| {
        if x > 0.5 {
            0.1
        } else if x < -0.5 {
            -1.0
        } else {
            x
        }
    })
}

fn main() {
    env_logger::init();  // Log to stderr (if you run with `RUST_LOG=debug`).
    std::thread::scope(|s| {
        let (display_tx, display_rx) = mpsc::sync_channel(1);
        let (upd_tx, upd_rx) = mpsc::sync_channel(1);
        let f = || run_emulator(display_tx, upd_rx);
        s.spawn(f);
        run_gui(display_rx, upd_tx);
    });
}

fn run_emulator(display_tx: mpsc::SyncSender<EmuUI>, upd_rx: mpsc::Receiver<Update>) {
    let mut vm = emu_backend::Interpreter::default();
    let program: &[u8] = include_bytes!("../../roms/superpong.ch8");
    vm.load_rom(program);
    vm.set_seed(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_or(42, |x| {
                x.as_millis()
                    .to_le_bytes()
                    .into_iter()
                    .fold(0u16, |a, b| (a << 2) | (a ^ (b as u16)))
            }),
    );

    let mut count = 0u64;
    let (_stream, output_handle) = rodio::OutputStream::try_default().unwrap();
    let sink = rodio::Sink::try_new(&output_handle).unwrap();
    sink.append(make_source());
    sink.pause();

    loop {
        let local_time = Instant::now();

        vm.tick();
        count += vm.execute_bounded(10_000, ());

        if let Err(mpsc::TrySendError::Disconnected(_)) = display_tx.try_send(EmuUI {
            display: vm.display_snapshot(),
            instructions: count,
        }) {
            break;
        };
        if vm.is_playing_sound() {
            sink.play();
        } else {
            sink.pause();
        }
        std::thread::sleep(Duration::from_micros(16667).saturating_sub(local_time.elapsed()));
        match upd_rx.try_recv() {
            Ok(Update::Keyboard(keys)) => vm.set_keyboard(keys),
            Ok(Update::CoreDump) => {
                println!("{vm:?}");
                vm.core_dump();
            }
            Err(mpsc::TryRecvError::Empty) => {}
            Err(mpsc::TryRecvError::Disconnected) => break,
        }
    }
    vm.core_dump();
    println!("Done; {vm:?}");
}

fn run_gui(display_rx: mpsc::Receiver<EmuUI>, upd_tx: mpsc::SyncSender<Update>) {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_min_inner_size((300., 200.)),
        ..Default::default()
    };

    eframe::run_native(
        "CHIP-8",
        options,
        Box::new(|cc| {
            // This gives us image support:
            egui_extras::install_image_loaders(&cc.egui_ctx);

            Ok(Box::new(MyApp::new(display_rx, upd_tx)))
        }),
    )
    .unwrap();
}

struct MyApp {
    emu: EmuUI,
    keyboard_state: u16,
    display_rx: mpsc::Receiver<EmuUI>,
    upd_tx: mpsc::SyncSender<Update>,
}

impl MyApp {
    fn new(display_rx: mpsc::Receiver<EmuUI>, upd_tx: mpsc::SyncSender<Update>) -> Self {
        Self {
            emu: EmuUI {
                display: [0; 256],
                instructions: 0,
            },
            keyboard_state: 0,
            display_rx,
            upd_tx,
        }
    }
}

pub fn chip_ui(ui: &mut egui::Ui, bytes: &[u8; 256], pixel_scale: f32) -> egui::Response {
    let (rect, resp) = ui.allocate_exact_size(
        Vec2::new(64. * pixel_scale + 2., 32. * pixel_scale + 2.),
        Sense::hover(),
    );

    if ui.is_rect_visible(rect) {
        let painter = ui.painter_at(rect);
        let start = rect.min + Vec2::new(1., 1.);

        painter.rect_filled(rect, 0.0, Color32::BLACK);
        let pixel_color = Color32::from_rgb(200, 200, 255);
        let pixel = Rect::from_min_max(Pos2::new(0., 0.), Pos2::new(pixel_scale, pixel_scale));

        for j in 0..32 {
            for i in 0..64 {
                let byte = unsafe { bytes.get_unchecked(j * 8 + (i / 8)) };
                if byte & (1 << (7 - (i & 0b00000111))) != 0 {
                    painter.rect_filled(
                        pixel.translate(Vec2::new(
                            i as f32 * pixel_scale + start.x,
                            j as f32 * pixel_scale + start.y,
                        )),
                        0.0,
                        pixel_color,
                    );
                }
            }
        }
        painter.rect_stroke(rect, 0.0, (1., pixel_color), StrokeKind::Inside);
    }

    resp
}

fn key_to_pos(key: &egui::Key) -> Option<u8> {
    match key {
        egui::Key::Num1 => Some(0x1),
        egui::Key::Num2 => Some(0x2),
        egui::Key::Num3 => Some(0x3),
        egui::Key::Num4 => Some(0xC),
        egui::Key::Q => Some(0x4),
        egui::Key::W => Some(0x5),
        egui::Key::E => Some(0x6),
        egui::Key::R => Some(0xD),
        egui::Key::A => Some(0x7),
        egui::Key::S => Some(0x8),
        egui::Key::D => Some(0x9),
        egui::Key::F => Some(0xE),
        egui::Key::Z => Some(0xA),
        egui::Key::X => Some(0x0),
        egui::Key::C => Some(0xB),
        egui::Key::V => Some(0xF),
        _ => None,
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let (pressed, released) = ctx.input(|i| {
            let mut pressed = 0u16;
            let mut released = 0u16;
            for e in i.events.iter() {
                if let egui::Event::Key {
                    physical_key: Some(k),
                    pressed: is_pressed,
                    ..
                } = e
                {
                    if let Some(pos) = key_to_pos(k) {
                        if *is_pressed {
                            pressed |= 1 << pos;
                        } else {
                            released |= 1 << pos;
                        }
                    }

                    if matches!(k, egui::Key::Y) && *is_pressed {
                        let _ = self.upd_tx.try_send(Update::CoreDump);
                    }
                }
            }
            (pressed, released)
        });
        let new_kb = (self.keyboard_state | pressed) & !released;
        if new_kb != self.keyboard_state {
            self.keyboard_state = new_kb;
            if let Err(mpsc::TrySendError::Disconnected(_)) =
                self.upd_tx.try_send(Update::Keyboard(new_kb))
            {
                todo!("Disconnected...")
            };
        }

        match self.display_rx.try_recv() {
            Ok(emu_ui) => {
                self.emu = emu_ui;
                ctx.request_repaint();
            }
            Err(mpsc::TryRecvError::Empty) => {}
            Err(mpsc::TryRecvError::Disconnected) => {
                todo!("Disconnected...");
            }
        };

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.vertical(|ui| {
                ui.heading(format!("CHIP-8, {} instructions", self.emu.instructions));
                let available_size = ui.available_size();

                let xscale = ((available_size.x - 2.) / 64.).floor();
                let yscale = ((available_size.y - 2.) / 32.).floor();

                chip_ui(ui, &self.emu.display, xscale.min(yscale));
            })
        });
    }
}
