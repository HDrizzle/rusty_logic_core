//! Toplevel UI, not circuit specific

use crate::{prelude::*, resource_interface};
use eframe::{egui::{self, containers::Popup, response::Response, scroll_area::ScrollBarVisibility, Align2, Button, PopupCloseBehavior, Pos2, RectAlign, ScrollArea, Ui}};
use egui_commonmark::{CommonMarkCache, CommonMarkViewer};
use std::rc::Rc;
#[cfg(feature = "kicad_scrolling")]
use mouse_rs;

#[cfg(feature = "using_filesystem")]
struct NewCircuitWindow {
	lib_name: String,
	name: String,
	file_name: String
}

#[cfg(feature = "using_filesystem")]
impl NewCircuitWindow {
	pub fn new() -> Self {
		Self {
			lib_name: DEFAULT_CIRCUIT_LIB.to_owned(),
			name: String::new(),
			file_name: String::new()
		}
	}
	pub fn clear(&mut self) {
		self.lib_name = DEFAULT_CIRCUIT_LIB.to_owned();
		self.name = String::new();
		self.file_name = String::new();
	}
	/// Returns: (Whether to close, Option<New circuit>)
	pub fn show(&mut self, ui: &mut Ui, circuit_libs_ordered: &Vec<String>) -> (bool, Option<LogicCircuit>) {
		let mut error_opt: Option<String> = None;
		let mut out: (bool, Option<LogicCircuit>) = (false, None);
		ui.label("Create New Circuit");
		ui.horizontal(|ui| {
			ui.label("Name: ");
			ui.text_edit_singleline(&mut self.name);
		});
		ui.horizontal(|ui| {
			ui.label("Save file name: ");
			ui.text_edit_singleline(&mut self.file_name);
			ui.label(".json");
		});
		// Check
		for char in self.file_name.chars() {
			if !REASONABLE_FILENAME_CHARS.contains(char) {
				error_opt = Some(format!("File name contains disallowed char \"{}\"", char));
			}
		}
		ui.horizontal(|ui| {
			ui.label("Circuit library: ");
			egui::ComboBox::new("New circuit library select", "")
				.selected_text(&self.lib_name)
				.show_ui(ui, |ui| {
					for lib_option in circuit_libs_ordered {
						ui.selectable_value(&mut self.lib_name, lib_option.clone(), lib_option);
					}
				});
		});
		ui.horizontal(|ui| {
			let create_button = Button::new("Create Circuit");
			if ui.add_enabled(error_opt.is_none(), create_button).clicked() {
				out.0 = true;
				out.1 = Some(LogicCircuit::new_mostly_default(self.name.clone(), self.file_name.clone(), true, self.lib_name.clone()));
			}
			if ui.button("Cancel").clicked() {
				out.0 = true;
			}
		});
		if let Some(error) = error_opt {
			ui.colored_label(u8_3_to_color32([255, 0, 0]), error);
		}
		out
	}
}

pub struct App {
	styles: Rc<Styles>,
	circuit_tabs: Vec<LogicCircuitToplevelView>,
	/// 0 is for the home tab, so indexing for `circuit_tabs` starts at 1
	current_tab_index: usize,
	load_circuit_err_opt: Option<String>,
	readme_file: String,
	/// Vec<(Library, File)>
	circuits_ordered: Vec<(String, String)>,
	circuit_libs_ordered: Vec<String>,
	#[cfg(feature = "using_filesystem")]
	new_circuit_window: NewCircuitWindow,
	#[cfg(feature = "using_filesystem")]
	showing_new_circuit_popup: bool
}

impl App {
	pub fn new() -> Self {
		// Load styles
		let styles: Styles = match Styles::load() {
			Ok(styles) => styles,
			Err(e) => {
				println!("Could not load styles: {}, resorting to default", e);
				Styles::default()
			}
		};
		let mut out = Self {
			styles: Rc::new(styles),
			circuit_tabs: Vec::new(),//vec![LogicCircuitToplevelView::new(create_simple_circuit(), false)],
			current_tab_index: 0,
			load_circuit_err_opt: None,
			readme_file: resource_interface::load_file_with_better_error("README.md").unwrap(),
			circuits_ordered: Vec::new(),
			circuit_libs_ordered: Vec::new(),
			#[cfg(feature = "using_filesystem")]
			new_circuit_window: NewCircuitWindow::new(),
			showing_new_circuit_popup: false
		};
		out.load_circuit_names_and_libs().unwrap();
		out
	}
	fn load_circuit_names_and_libs(&mut self) -> Result<(), String> {
		self.circuits_ordered = resource_interface::list_all_circuit_files()?;
		self.circuit_libs_ordered = resource_interface::load_circuit_libraries()?.1;
		Ok(())
	}
	fn load_circuit_tab(&mut self, file_name: &str, lib_name: &str) -> Result<LogicCircuit, String> {
		resource_interface::load_circuit(file_name, false, true, IntV2(0, 0), FourWayDir::default(), String::new(), lib_name.to_owned(), false)
	}
	fn new_circuit_tab(&mut self, file_name: &str, lib_name: &str) {
		match self.load_circuit_tab(file_name, lib_name) {
			Ok(circuit) => {
				// RIP Joe Sullivan, You did a lot for Haley
				self.circuit_tabs.push(LogicCircuitToplevelView::new(circuit, true, &self.styles));
				self.current_tab_index = self.circuit_tabs.len();// Not an OBOE
			},
			Err(e) => {
				self.load_circuit_err_opt = Some(e);
			}
		}
	}
	fn reload_tab(&mut self, tab_index: usize) {
		let actual_tab_index = tab_index - 1;
		let circuit = &self.circuit_tabs[actual_tab_index].circuit;
		match self.load_circuit_tab(&circuit.save_name.clone(), &circuit.lib_name.clone()) {
			Ok(new_circuit) => {
				self.circuit_tabs[actual_tab_index] = LogicCircuitToplevelView::new(new_circuit, true, &self.styles);
			},
			Err(e) => {
				self.load_circuit_err_opt = Some(e);
			}
		}
	}
}

impl eframe::App for App {
	fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
		let circuit_names: Vec<String> = self.circuit_tabs.iter().map(|toplevel| toplevel.circuit.type_name.clone()).collect();
		let mut i_to_delete_opt: Option<usize> = None;
		egui::CentralPanel::default().show(ctx, |ui: &mut Ui| {
			// This function by default is only run upon user interaction, so copied this from https://users.rust-lang.org/t/issues-while-writing-a-clock-with-egui/102752
			ui.ctx().request_repaint();
			ScrollArea::horizontal().scroll_bar_visibility(ScrollBarVisibility::AlwaysHidden).show(ui, |ui| {
				ui.horizontal(|ui| {
					if ui.add_enabled(self.current_tab_index != 0, Button::new("Home")).clicked() {
						if self.current_tab_index != 0 {
							self.current_tab_index = 0;
						}
					}
					for (i, circuit_name) in circuit_names.iter().enumerate() {
						let name_for_ui: &str = match self.circuit_tabs[i].saved {
							true => circuit_name,
							false => &format!("{} *", circuit_name)
						};
						let is_current_tab: bool = self.current_tab_index == i + 1;
						if ui.add_enabled(!is_current_tab, Button::new(name_for_ui)).clicked() {
							self.current_tab_index = i + 1;
						}
						// Tab close button
						if is_current_tab {
							if ui.button("x").clicked() {
								i_to_delete_opt = Some(i);
							}
						}
					}
					let new_button_response = ui.button("+");
					if new_button_response.clicked() {
						self.load_circuit_err_opt = None;
						self.load_circuit_names_and_libs().unwrap();
					}
					Popup::menu(&new_button_response).close_behavior(PopupCloseBehavior::CloseOnClickOutside).align(RectAlign::RIGHT_START).align_alternatives(&[RectAlign::LEFT_START]).show(|ui| {
						if ui.button("New Circuit...").clicked() {
							self.showing_new_circuit_popup = true;
							self.new_circuit_window.clear();
						}
						ui.menu_button("Load Circuit", |ui| {
							if self.circuits_ordered.len() == 0 {
								ui.label(format!("No circuit files found across all libraries"));
							}
							ScrollArea::vertical().show(ui, |ui| {
								let mut load_info_opt: Option<(String, String)> = None;
								for (lib_name, file_name) in &self.circuits_ordered {
									if ui.selectable_label(false, &format!("{}/{}", lib_name, file_name)).clicked() {
										load_info_opt = Some((lib_name.clone(), file_name.clone()));
									}
								}
								if let Some(load_info) = load_info_opt {
									self.new_circuit_tab(&load_info.1, &load_info.0);
								}
								if let Some(load_error) = &self.load_circuit_err_opt {
									ui.colored_label(u8_3_to_color32([255, 0, 0]), format!("Loading error: {}", load_error));
								}
							});
						});
					});
				});
			});
			let response_for_popups: Response = if self.current_tab_index == 0 {// Home tab
				ui.vertical(|ui| {
					ScrollArea::vertical().show(ui, |ui| {
						CommonMarkViewer::new().show(ui, &mut CommonMarkCache::default(), &self.readme_file);
					});
				}).response
			}
			else {
				let circuit_toplevel: &mut LogicCircuitToplevelView = &mut self.circuit_tabs[self.current_tab_index - 1];
				#[allow(unused)]
				let (new_mouse_pos_opt, new_circuit_tab_opt, response, reload_tab): (Option<Pos2>, Option<(String, String)>, Response, bool) = circuit_toplevel.draw(ui, Rc::clone(&self.styles), ctx.screen_rect().min);// TODO: Get actual window top-left position
				#[cfg(feature = "kicad_scrolling")]
				if let Some(new_pos) = new_mouse_pos_opt {
					let mouse = mouse_rs::Mouse::new();
					mouse.move_to(new_pos.x as i32, new_pos.y as i32).unwrap();
				}
				if let Some(new_circuit_tab) = new_circuit_tab_opt {
					self.new_circuit_tab(&new_circuit_tab.0, &new_circuit_tab.1);
				}
				if reload_tab {
					self.reload_tab(self.current_tab_index);
				}
				response
			};
			if self.showing_new_circuit_popup {
				Popup::from_response(&response_for_popups).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
					let (close, new_circuit_opt) = self.new_circuit_window.show(ui, &self.circuit_libs_ordered);
					if close {
						self.showing_new_circuit_popup = false;
						if let Some(new_circuit) = new_circuit_opt {
							self.circuit_tabs.push(LogicCircuitToplevelView::new(new_circuit, false, &self.styles));
							self.current_tab_index = self.circuit_tabs.len();// Not an OBOE
						}
					}
				});
			}
		});
		if let Some(i_to_delete) = i_to_delete_opt {
			self.circuit_tabs.remove(i_to_delete);
			if self.current_tab_index >= i_to_delete + 1 {
				self.current_tab_index -= 1;
			}
		}
	}
}