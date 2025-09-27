
#[cfg(feature = "using_egui")]
use rusty_logic::ui_main;

fn main() {
    #[cfg(feature = "using_egui")]
    ui_main();
    #[cfg(not(feature = "using_egui"))]
    println!("Enable `using_egui` feature to use GUI");
}