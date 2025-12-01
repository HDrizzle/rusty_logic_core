//! Only UI stuff

use crate::{builtin_components, prelude::*, resource_interface, simulator::{AncestryStack, GraphicSelectableItemRef, SelectionState, TimingDiagram, TimingTiagramRunningState, TimingDiagramTimestamp, Tool}};
use eframe::{egui::{self, containers::Popup, scroll_area::ScrollBarVisibility, text::LayoutJob, Align2, Button, Color32, DragValue, FontFamily, FontId, Frame, Galley, Painter, PopupCloseBehavior, Pos2, Rect, RectAlign, ScrollArea, Sense, Shape, Stroke, StrokeKind, TextEdit, TextFormat, Ui, Vec2, Window, response::Response, Key, KeyboardShortcut, Modifiers, PointerButton}, emath, epaint::{PathStroke, TextShape}};
use egui_commonmark::{CommonMarkCache, CommonMarkViewer};
use nalgebra::ComplexField;
use serde::{Serialize, Deserialize};
use serde_json;
use std::{collections::HashSet, f32::consts::TAU, ops::{AddAssign, DerefMut, RangeInclusive, SubAssign}, rc::Rc, sync::Arc, time::Instant};
#[cfg(feature = "kicad_scrolling")]
use mouse_rs;

#[derive(Debug, Clone, PartialEq)]
pub enum SelectProperty {
	BitWidth(u16),
	PositionX(i32),
	PositionY(i32),
	GlobalConnectionState(Vec<Option<bool>>),
	Direction(FourWayDir),
	DisplayCircuitAsBlock(bool),
	ClockEnabled(bool),
	ClockFreq(f32),
	ClockState(bool),
	Name(String),
	FixedSourceState(bool),
	/// Address size, Max
	AddressWidth(u8, u8),
	MemoryProperties(builtin_components::MemoryPropertiesUI),
	EncoderOrDecoder(bool),
	/// Whether to reload, optional error message
	ReloadCircuit(bool, Option<String>),
	SplitterSplits(Vec<u16>),
	/// Bus name, Whether to group, Whether to list in forward order
	BusLayout(String, bool, bool),
	/// Namee of pin (ex: "Output Enable"), whether it is enabled
	HasPin(String, bool)
}

impl SelectProperty {
	pub fn ui_name(&self) -> String {
		match self {
			Self::BitWidth(_) => "Bit Width".to_owned(),
			Self::PositionX(_) => "X".to_owned(),
			Self::PositionY(_) => "Y".to_owned(),
			Self::GlobalConnectionState(_) => "I/O State".to_owned(),
			Self::Direction(_) => "Direction".to_owned(),
			Self::DisplayCircuitAsBlock(_) => "Display circuit as block".to_owned(),
			Self::ClockEnabled(_) => "Clock Enable".to_owned(),
			Self::ClockFreq(_) => "Clock Frequency".to_owned(),
			Self::ClockState(_) => "Clock State".to_owned(),
			Self::Name(_) => "Name".to_owned(),
			Self::FixedSourceState(_) => "State".to_owned(),
			Self::AddressWidth(_, _) => "Address Width".to_owned(),
			Self::MemoryProperties(_) => "Memory Properties".to_owned(),
			Self::EncoderOrDecoder(_) => "Encoder/Decoder".to_owned(),
			Self::ReloadCircuit(_, _) => "Reload circuit".to_owned(),
			Self::SplitterSplits(_) => "Splits".to_owned(),
			Self::BusLayout(name, _, _) => format!("{} bus config", name),
			Self::HasPin(name, _) => format!("Has {} pin", name)
		}
	}
	/// Add this property to a list on the UI
	/// Returns: Whether to update the property and/or recalculate circuit connections
	pub fn add_to_ui(&mut self, ui: &mut Ui) -> bool {
		match self {
			Self::BitWidth(n) => {
				ui_drag_value_with_arrows(ui, n, Some((1, 256)))
			},
			Self::PositionX(x) => {
				ui_drag_value_with_arrows(ui, x, None)
			},
			Self::PositionY(y) => {
				ui_drag_value_with_arrows(ui, y, None)
			},
			Self::GlobalConnectionState(state_opts) => {
				let mut return_update_all = false;
				let mut return_update_single = false;
				let mut all_drive: Option<bool> = state_opts[0];
				ui.vertical(|ui| {
					Popup::menu(&ui.button("I/O State")).align(RectAlign::RIGHT_START).show(|ui| {
						if ui.button("All 0").clicked() {
							all_drive = Some(false);
							return_update_all = true;
						}
						if ui.button("All 1").clicked() {
							all_drive = Some(true);
							return_update_all = true;
						}
						if ui.button("Floating").clicked() {
							all_drive = None;
							return_update_all = true;
						}
					});
					for opt in state_opts {
						if return_update_all {
							*opt = all_drive;
						}
						if ui.add_enabled(opt.is_some(), Button::new(match opt.unwrap_or(false) {true => "1", false => "0"})).clicked() {
							if let Some(state) = opt {
								*state = !*state;
								return_update_single = true;
							}
						}
					}
				});
				return_update_all || return_update_single
			},
			Self::Direction(dir) => {
				if ui.button("↶").clicked() {
					*dir = dir.turn_ccw();
					return true;
				}
				if ui.button("↷").clicked() {
					*dir = dir.turn_cw();
					return true;
				}
				return false;
			},
			Self::DisplayCircuitAsBlock(block) => {
				ui.checkbox(block, "").changed()
			},
			Self::ClockEnabled(enable) => {
				ui.checkbox(enable, match enable {
					true => "Enabled",
					false => "Disabled"
				}).changed()
			},
			Self::ClockFreq(freq) => {
				ui.add(DragValue::new(freq).range(RangeInclusive::new(0.01, 1000000.0)).clamp_existing_to_range(false)).changed()
			},
			Self::ClockState(state) => {
				if ui.button(match state {
					true => "1",
					false => "0"
				}).clicked() {
					*state = !(*state);
					true
				}
				else {
					false
				}
			}
			Self::Name(name) => {
				ui.text_edit_singleline(name).changed()
			},
			Self::FixedSourceState(state) => {
				if ui.button(match state {
					true => "1",
					false => "0"
				}).clicked() {
					*state = !(*state);
					true
				}
				else {
					false
				}
			},
			Self::AddressWidth(n, max) => {
				ui_drag_value_with_arrows(ui, n, Some((1, *max)))
			}
			Self::MemoryProperties(props) => {
				props.show_ui(ui)
			}
			Self::EncoderOrDecoder(encoder) => {
				if ui.button(match encoder {
					true => "Encoder",
					false => "Decoder"
				}).clicked() {
					*encoder = !*encoder;
					true
				}
				else {
					false
				}
			},
			Self::ReloadCircuit(reload, err_opt) => {
				*reload = ui.button("Reload").clicked();
				if let Some(err) = err_opt {
					ui.colored_label(u8_3_to_color32([255, 0, 0]), err);
				}
				*reload
			}
			Self::SplitterSplits(splits) => {
				let mut changed = false;
				ui.vertical(|ui| {
					let mut total_bw = 0;
					for s in &mut *splits {
						ui.horizontal(|ui| {
							changed |= ui_drag_value_with_arrows(ui, s, Some((1, 256)));
						});
						total_bw += *s;
					}
					ui.horizontal(|ui| {
						if ui.button("-").clicked() {
							splits.pop();
							changed = true;
						}
						if ui.button("+").clicked() {
							splits.push(1);
							changed = true;
						}
					});
					ui.label(format!("Total BW: {}", total_bw));
					if total_bw > 256 {
						ui.colored_label(u8_3_to_color32([255, 0, 0]), "Bit width cannot exceed 256");
					}
				});
				changed
			},
			Self::BusLayout(_, single_pin, forward) => {
				let mut changed = ui.checkbox(single_pin, "Single graphic pin").changed();
				if !*single_pin {
					if ui.button("Reverse").clicked() {
						*forward = !*forward;
						changed = true;
					}
				}
				changed
			}
			Self::HasPin(_, state) => {
				ui.checkbox(state, "").changed()
			}
		}
	}
}

#[derive(Debug, Serialize, Deserialize)]
pub enum CopiedGraphicItem {
	Component(EnumAllLogicDevices),
	Wire((IntV2, FourWayDir, u32)),
	/// (Position, Direction, Name, Show name, Bit width)
	ExternalConnection(IntV2, FourWayDir, String, bool, u16),
	Splitter(SplitterSave),
	GraphicLabel(GraphicLabelSave),
	Probe(ProbeSave)
}

/// This is what will be serialized as JSON and put onto the clipboard
#[derive(Debug, Serialize, Deserialize)]
pub struct CopiedItemSet {
	pub items: Vec<CopiedGraphicItem>,
	/// So that everything will be shown at the same displacement wrt the mouse when paste is hit
	pub bb_center: IntV2
}

impl CopiedItemSet {
	pub fn new(
		items: Vec<CopiedGraphicItem>,
		bb_center: IntV2
	) -> Self {
		Self {
			items,
			bb_center
		}
	}
}

#[derive(Clone)]
pub struct EguiDrawInterface {
	pub data: DrawData,
	pub painter: Rc<Painter>
}

impl EguiDrawInterface {
	pub fn new(
		data: DrawData,
		painter: Rc<Painter>
	) -> Self {
		Self {
			data,
			painter
		}
	}
}

impl DrawInterface for EguiDrawInterface {
	fn get_draw_data(&self) -> &DrawData {
		&self.data
	}
	fn draw_polyline(&self, points: Vec<V2>, stroke: [u8; 3]) {
		let draw_data = self.get_draw_data();
		let px_points = points.iter().map(|p| v2_to_emath_pos2(draw_data.grid_to_px(*p))).collect();
		self.painter.add(Shape::line(px_points, PathStroke::new(draw_data.grid_size * draw_data.styles.line_size_grid, u8_3_to_color32(stroke))));
	}
	fn draw_rect(&self, start_grid: V2, end_grid: V2, inside_stroke: [u8; 4], border_stroke: [u8; 3]) {
		let draw_data = self.get_draw_data();
		let (start, end) = merge_points_to_bb_reversed_y(vec![draw_data.grid_to_px(start_grid), draw_data.grid_to_px(end_grid)]);
		let px_rectified_bb = Rect{min: Pos2::new(start.x, end.y), max: Pos2::new(end.x, start.y)};// After Y is flipped, Y needs to be swapped so that the bb is correct in pixel coordinates, Gemini helped find this problem
		self.painter.rect_filled(px_rectified_bb, 0, u8_4_to_color32(inside_stroke));
		self.painter.rect_stroke(px_rectified_bb, 0, Stroke::new(draw_data.grid_size * draw_data.styles.line_size_grid, u8_3_to_color32(border_stroke)), StrokeKind::Outside);
	}
	fn draw_circle(&self, center: V2, radius: f32, stroke: [u8; 3]) {
		let draw_data = self.get_draw_data();
		self.painter.circle_stroke(v2_to_emath_pos2(draw_data.grid_to_px(center)), radius * draw_data.grid_size, Stroke::new(draw_data.grid_size * draw_data.styles.line_size_grid, u8_3_to_color32(stroke)));
	}
	fn draw_circle_filled(&self, center: V2, radius: f32, stroke: [u8; 3]) {
		let draw_data = self.get_draw_data();
		self.painter.circle_filled(v2_to_emath_pos2(draw_data.grid_to_px(center)), radius * draw_data.grid_size, u8_3_to_color32(stroke));
	}
	/// Egui doen't have an arc feature so I will use a polyline :(
	/// The end angle must be a larger number then the start angle
	fn draw_arc(&self, center_grid: V2, radius_grid: f32, start_deg: f32, end_deg: f32, stroke: [u8; 3]) {
		let mut polyline = Vec::<V2>::new();
		let seg_size: f32 = 5.0;
		let n_segs = ((end_deg - start_deg) / seg_size) as usize;
		for i in 0..n_segs+1 {
			let angle_rad = ((i as f32 * seg_size) + start_deg) * TAU / 360.0;
			polyline.push(V2::new(angle_rad.cos() * radius_grid, angle_rad.sin() * radius_grid) + center_grid);
		}
		self.draw_polyline(polyline, stroke);
	}
	fn text(&self, text: &str, pos: V2, align: GenericAlign2, color: [u8; 3], size_grid: f32, vertical: bool) {
		let draw_data = self.get_draw_data();
		let color32 = u8_3_to_color32(color);
		let pos_px = draw_data.grid_to_px(pos);
		let font_id = FontId::new(draw_data.grid_size * size_grid, FontFamily::Monospace);
		if vertical {
			let galley = self.painter.fonts::<Arc<Galley>>(|fonts| {
				let mut job: LayoutJob = LayoutJob::default();
				job.append(text, 0.0, TextFormat::simple(font_id, color32));
				fonts.layout_job(job)
			});
			let y_offet = if align == GenericAlign2::CENTER_BOTTOM {
				galley.size().x
			}
			else {
				0.0
			};
			let mut shape = TextShape::new(v2_to_emath_pos2(pos_px + V2::new(galley.size().y / 2.0, -y_offet)), galley, color32);
			shape.angle = std::f32::consts::FRAC_PI_2;
			self.painter.add(shape);
		}
		else {
			self.painter.text(v2_to_emath_pos2(pos_px), align.to_egui(), text, font_id, color32);
		}
	}
	/// gets text size in grid without actually drawing it
	fn text_size(&self, text: &str, size_grid: f32) -> V2 {
		let grid_size = self.get_grid_size();
		let font_id = FontId::new(grid_size * size_grid, FontFamily::Monospace);
		let galley = self.painter.fonts::<Arc<Galley>>(|fonts| {
			let mut job: LayoutJob = LayoutJob::default();
			job.append(text, 0.0, TextFormat::simple(font_id, Color32::default()));
			fonts.layout_job(job)
		});
		emath_vec2_to_v2(galley.size()) / grid_size
	}
	fn add_grid_pos_and_direction(&self, offset_unrotated: IntV2, dir_: FourWayDir) -> Box<dyn DrawInterface> {
		Box::new(Self {
			data: self.data.add_grid_pos_and_direction(offset_unrotated, dir_),
			painter: Rc::clone(&self.painter)
		})
	}
}

impl LogicCircuit {
	/// Returns: Whether to recompute the circuit connections
	pub fn toplevel_ui_interact<'a, F: Fn(Pos2) -> V2>(&mut self, response: Response, context: &egui::Context, /*draw: &dyn DrawInterface<'a>,*/ mut input_state: egui::InputState, grid_size: f32, mouse_pos2_to_grid: F) -> bool {
		let mut return_recompute_connections = false;
		let mut new_tool_opt = Option::<Tool>::None;
		let mut recompute_pin_block_positions = false;
		let mut new_highlighted_net = Option::<Option<u64>>::None;
		let mouse_pos_grid_opt: Option<V2> = match response.hover_pos() {
			Some(pos_px) => Some(mouse_pos2_to_grid(pos_px)),
			None => None
		};
		match self.tool.borrow_mut().deref_mut() {
			Tool::Select{selected_graphics, selected_graphics_state} => {
				match selected_graphics_state {
					SelectionState::Fixed => {
						if response.drag_started_by(PointerButton::Primary) {
							let begining_mouse_pos_grid: V2 = mouse_pos_grid_opt.expect("Hover pos should work when dragging");
							*selected_graphics_state =  SelectionState::Dragging(begining_mouse_pos_grid, emath_vec2_to_v2(response.drag_delta()) / grid_size);
							for item_ref in selected_graphics.iter() {
								self.run_function_on_graphic_item_mut(item_ref.clone(), |graphic_item| {
									graphic_item.start_dragging(begining_mouse_pos_grid);
								});
							}
						}
						if response.clicked() {
							// Find if command/ctrl is being held down
							let multi_select_key: bool = input_state.key_down(Key::A);// TODO: Command / Control
							// Find what was clicked (if anything)
							match self.was_anything_clicked(mouse_pos2_to_grid(response.interact_pointer_pos().expect("Interact pointer pos should work when clicked")), multi_select_key) {
								Some(new_selected_item) => match multi_select_key {
									true => match selected_graphics.contains(&new_selected_item) {
										true => {
											selected_graphics.remove(&new_selected_item);
										},
										false => {
											selected_graphics.insert(new_selected_item);
										}
									},
									false => {
										selected_graphics.clear();
										selected_graphics.insert(new_selected_item);
									}
								},
								None => {
									if !multi_select_key {
										*selected_graphics = HashSet::new();
									}
								}
							}
							//println!("Currently selected items: {:?}", &selected_graphics);
						}
						// Cmd-A for select-all
						if input_state.consume_shortcut(&KeyboardShortcut::new(Modifiers::COMMAND, Key::A)) {
							*selected_graphics = HashSet::from_iter(self.get_all_graphics_references());
						}
						// W for Wire
						if input_state.consume_key(Modifiers::NONE, Key::W) {
							new_tool_opt = Some(Tool::PlaceWire{perp_pairs: vec![]});
						}
					},
					SelectionState::Dragging(start_grid, delta_grid) => {
						let delta_grid_backwards_y = emath_vec2_to_v2(response.drag_delta()) / grid_size;
						*delta_grid += if cfg!(feature = "reverse_y") {
							v2_reverse_y(delta_grid_backwards_y)
						} else {
							delta_grid_backwards_y
						};
						match selected_graphics.len() {
							0 => {// Drag a rectangle
								let select_bb: (V2, V2) = merge_points_to_bb(vec![*start_grid, *start_grid + *delta_grid]);
								if response.drag_stopped_by(PointerButton::Primary) {
									// Find all items that have BBs intersected by the rectangle and select them
									selected_graphics.clear();
									for item_ref in self.get_all_graphics_references() {
										if self.run_function_on_graphic_item(item_ref.clone(), |graphic_item| -> bool {
											bbs_overlap(graphic_item.bounding_box(V2::zeros()), select_bb)
										}).unwrap_or_else(|| false) {
											selected_graphics.insert(item_ref.clone());
										}
									}
									// Back to fixed selection
									*selected_graphics_state = SelectionState::Fixed
								}
							},
							_ => {// Move stuff
								for item_ref in selected_graphics.iter() {
									self.run_function_on_graphic_item_mut(item_ref.clone(), |graphic_item| {
										graphic_item.dragging_to(*start_grid + *delta_grid);
									});
								}
								if response.drag_stopped_by(PointerButton::Primary) {
									for item_ref in selected_graphics.iter() {
										self.run_function_on_graphic_item_mut(item_ref.clone(), |graphic_item| {
											graphic_item.stop_dragging(*start_grid + *delta_grid);
										});
									}
									*selected_graphics_state = SelectionState::Fixed;
									return_recompute_connections = true;
								}
							}
						}
					},
					SelectionState::FollowingMouse(mouse_pos) => {
						if let Some(new_mouse_pos) = mouse_pos_grid_opt {// In case the mouse goes off the edge or something idk
							*mouse_pos = new_mouse_pos;
						}
						for item_ref in selected_graphics.iter() {
							self.run_function_on_graphic_item_mut(item_ref.clone(), |item_box| {item_box.get_ui_data_mut().position = round_v2_to_intv2(*mouse_pos) + item_box.get_ui_data().position_before_dragging;});
						}
						if response.clicked_by(PointerButton::Primary) {
							return_recompute_connections = true;
							new_tool_opt = Some(Tool::default());
						}
					}
				}
				// Copy and Paste will use a plain-text JSON array of instances of the `CopiedGraphicItem` enum
				// Copy
				if input_state.consume_shortcut(&KeyboardShortcut::new(Modifiers::NONE, Key::C)) {// TODO
					let mut copied_items = Vec::<CopiedGraphicItem>::new();
					// Get combined BB center
					let bb_opt = self.selected_bb(selected_graphics);
					// If there is a BB then at least one selected item to copy, otherwise do nothing
					if let Some(bb) = bb_opt {
						let bb_center: V2 = (bb.1 + bb.0) / 2.0;
						//let bb_int = (IntV2(bb_float.0.x as i32, bb_float.0.y as i32), IntV2(bb_float.1.x as i32, bb_float.1.y as i32));
						for item_ref in selected_graphics.iter() {
							if let Some(copied_item) = self.copy_graphic_item(item_ref.clone()) {
								copied_items.push(copied_item);
							}
						}
						let item_set = CopiedItemSet::new(copied_items, round_v2_to_intv2(bb_center));
						let raw_string = serde_json::to_string(&item_set).unwrap();
						context.copy_text(raw_string);
					}
				}
				// Vaste
				if input_state.consume_shortcut(&KeyboardShortcut::new(Modifiers::NONE, Key::V)) {// TODO
					// Attempt to decode from clipboard
					let mut clipboard = arboard::Clipboard::new().unwrap();
					let string_raw = clipboard.get_text().unwrap();
					match serde_json::from_str::<CopiedItemSet>(&string_raw) {
						Ok(item_set) => {
							let pasted_selected_graphics = self.paste(item_set);
							new_tool_opt = Some(Tool::Select{selected_graphics: HashSet::from_iter(pasted_selected_graphics.into_iter()), selected_graphics_state: SelectionState::FollowingMouse(mouse_pos_grid_opt.unwrap_or_default())});
						},
						Err(_) => {}
					}
					recompute_pin_block_positions = true;
				}
				// Delete
				if input_state.consume_key(Modifiers::NONE, Key::Backspace) || input_state.consume_key(Modifiers::NONE, Key::Delete) {
					for item_ref in selected_graphics.iter() {
						self.remove_graphic_item(item_ref);
					}
					recompute_pin_block_positions = true;
					return_recompute_connections = true;
					*selected_graphics = HashSet::new();
				}
				// Rotate w/ arrow keys
				if input_state.consume_key(Modifiers::NONE, Key::ArrowLeft) {
					self.rotate_selection(selected_graphics, false);
					return_recompute_connections = true;
				}
				if input_state.consume_key(Modifiers::NONE, Key::ArrowRight) {
					self.rotate_selection(selected_graphics, true);
					return_recompute_connections = true;
				}
				// Flip w/ arrow keys
				if input_state.consume_key(Modifiers::COMMAND, Key::ArrowLeft) || input_state.consume_key(Modifiers::COMMAND, Key::ArrowRight) {
					self.flip_selection(selected_graphics, true);
					return_recompute_connections = true;
				}
				if input_state.consume_key(Modifiers::COMMAND, Key::ArrowDown) || input_state.consume_key(Modifiers::COMMAND, Key::ArrowUp) {
					self.flip_selection(selected_graphics, false);
					return_recompute_connections = true;
				}
			},
			Tool::HighlightNet => {
				if response.clicked_by(PointerButton::Primary) {
					if let Some(mouse_grid) = mouse_pos_grid_opt {
						let net_opts: Vec<Option<u64>> = self.is_connection_point(round_v2_to_intv2(mouse_grid)).1;
						new_highlighted_net = Some(None);
						for net_opt in net_opts {
							if let Some(test_net) = net_opt {
								new_highlighted_net = Some(Some(test_net));
							}
						}
					}
				}
				if input_state.consume_key(Modifiers::NONE, Key::Escape) {
					new_tool_opt = Some(Tool::default());
				}
			},
			Tool::PlaceWire{perp_pairs} => {
				if let Some(mouse_pos_grid) = mouse_pos_grid_opt {
					let mouse_pos_grid_rounded: IntV2 = round_v2_to_intv2(mouse_pos_grid);
					let n_pairs = perp_pairs.len();
					// Wire has been started
					if n_pairs >= 1 {
						let latest_pair: &mut (IntV2, FourWayDir) = &mut perp_pairs[n_pairs - 1];
						// Check if perp pair is perfectly horiz or vert
						let v: IntV2 = mouse_pos_grid_rounded - latest_pair.0;
						if let Some(new_dir) = v.is_along_axis() {
							latest_pair.1 = new_dir;
						}
						if response.clicked_by(PointerButton::Primary) {
							// First, check if this is a wire termination point
							let (is_term_point, _, _) = self.is_connection_point(mouse_pos_grid_rounded);
							if is_term_point {
								// End wire
								self.add_wire_geometry(perp_pairs.clone(), mouse_pos_grid_rounded);
								return_recompute_connections = true;
								new_tool_opt = Some(Tool::default());
							}
							else {
								perp_pairs.push((
									mouse_pos_grid_rounded,
									FourWayDir::E
								));
							}
						}
					}
					// Wire not started
					else {
						if response.clicked_by(PointerButton::Primary) {
							perp_pairs.push((
								mouse_pos_grid_rounded,
								FourWayDir::E
							));
						}
					}
				}
				if input_state.consume_key(Modifiers::NONE, Key::Escape) {
					new_tool_opt = Some(Tool::default());
				}
			}
		}
		if let Some(new_tool) = new_tool_opt {
			*self.tool.borrow_mut() = new_tool;
		}
		if recompute_pin_block_positions {
			self.update_pin_block_positions();
		}
		if let Some(net_opt) = new_highlighted_net {
			self.highlighted_net_opt = net_opt;
		}
		return_recompute_connections
	}
	/// Inspired by the timing diagram on Wavedrom.com/editor.html
	pub fn show_timing_diagram_ui(&self, ui: &mut Ui, styles: Rc<Styles>, timing: &mut TimingDiagram) {
		ui.horizontal(|ui| {
			if ui.button("Clear").clicked() {
				timing.clear();
			}
			Popup::menu(&ui.button(format!("Mode: {}", timing.running.to_str()))).show(|ui| {
				if ui.button(TimingTiagramRunningState::Off.to_str()).clicked() {
					timing.set_running_state(TimingTiagramRunningState::Off);
				}
				if ui.button(TimingTiagramRunningState::Clk.to_str()).clicked() {
					timing.set_running_state(TimingTiagramRunningState::Clk);
				}
				if ui.button(TimingTiagramRunningState::AnyChange.to_str()).clicked() {
					timing.set_running_state(TimingTiagramRunningState::AnyChange);
				}
				if ui.button(TimingTiagramRunningState::RealTime.to_str()).clicked() {
					timing.set_running_state(TimingTiagramRunningState::RealTime);
				}
			});
			if !timing.running.uses_real_time() {
				ui.checkbox(&mut timing.show_sim_steps, "Show propagation steps");
			}
		});
		ScrollArea::vertical().show(ui, |ui| {
			ui.horizontal(|ui| {
				let mut amplitude: f32 = 8.5;
				let mut vert_spacing: f32 = 25.0;
				// Labels
				let vert_widget_extra_spacing = ui.style().spacing.item_spacing.y;
				ui.vertical(|ui| {
					ui.add_space(8.0);
					let height = ui.label("CLK").rect.height() + vert_widget_extra_spacing;
					if height > vert_spacing {
						let r = height / vert_spacing;
						vert_spacing /= r;
						amplitude /= r;
					}
					ui.add_space(vert_spacing - height);
					let probes = self.probes.borrow();
					for (i, (probe_id, _)) in timing.signal_groups.iter().enumerate() {
						if i == 0 {
							continue;
						}
						let height = ui.label(&probes.get(probe_id).unwrap().name).rect.height() + vert_widget_extra_spacing;
						ui.add_space(vert_spacing - height);
					}
				});
				// Signals
				if timing.n_samples > 0 {
					ScrollArea::horizontal().stick_to_right(true).show(ui, |ui| {
						Frame::canvas(ui.style()).show::<()>(ui, |ui| {
							let canvas_size = Vec2::new(timing.convert_timestamp_to_x_value(&*styles, timing.timing_diagram_end()) + 4.0, timing.signal_groups.len() as f32 * vert_spacing);
							let (response, painter) = ui.allocate_painter(canvas_size, Sense::empty());
							let logic_state_to_graph_y_and_color = |state: LogicState| -> (f32, [u8; 3]) {
								match state {
									LogicState::Floating => (0.0, styles.color_wire_floating),
									LogicState::Contested => (0.0, styles.color_wire_contested),
									LogicState::Driven(bit) => match bit {
										true => (amplitude, styles.color_foreground),
										false => (-amplitude, styles.color_foreground)
									}
								}
							};
							let graph_pos_to_canvas_pos = |graph_x: f32, graph_y: f32, group_i: usize| -> Pos2 {
								Pos2::new(graph_x + response.rect.left() + 2.0, (-graph_y) + (group_i as f32 + 0.5)*vert_spacing + response.rect.top())
							};
							// New propagation event vertical marker lines
							if !timing.running.uses_real_time() && timing.show_sim_steps && timing.signal_groups.len() > 0 {
								for i in 0_u32..(timing.propagation_steps.len() as u32) {
									let x = timing.convert_timestamp_to_x_value(&*styles, TimingDiagramTimestamp::PropagationAndSimStep(i, 0));
									painter.line_segment(
										[
											graph_pos_to_canvas_pos(x, logic_state_to_graph_y_and_color(LogicState::Driven(true)).0, 0),
											graph_pos_to_canvas_pos(x, logic_state_to_graph_y_and_color(LogicState::Driven(false)).0, timing.signal_groups.len() - 1)],
										Stroke::new(0.7, u8_3_to_color32([128, 128, 128]))
									);
								}
							}
							// Iterate signal groups, each line on the timing diagram
							for (group_i, (_, signal_group)) in timing.signal_groups.iter().enumerate() {
								let mut prev_x: f32 = 0.0;
								let (mut prev_y, color) = logic_state_to_graph_y_and_color(LogicState::Floating);
								let mut prev_stroke = Stroke::new(1.0, u8_3_to_color32(color));
								if signal_group.len() == 0 {
									panic!("Cannot have empty signal group")
								} else if signal_group.len() == 1 {// Single bit, code here can be simplified
									let bit_line = &signal_group[0];
									let mut prev_sample: LogicState = if bit_line.len() == 0 {
										LogicState::Floating
									}
									else {
										bit_line[0].1
									};
									for (i, (timestamp, state)) in bit_line.iter().enumerate() {
										let x: f32 = timing.convert_timestamp_to_x_value(&*styles, *timestamp);
										let (y, color) = logic_state_to_graph_y_and_color(*state);
										let stroke = Stroke::new(1.0, u8_3_to_color32(color));
										if i == 0 {
											// Save numbers
											prev_y = y;
											prev_stroke = stroke;
										}
										else {
											// Vertical connection line if states are different
											if *state != prev_sample {
												painter.line_segment([graph_pos_to_canvas_pos(x, prev_y, group_i), graph_pos_to_canvas_pos(x, y, group_i)], stroke);
											}
										}
										// Horizontal line
										painter.line_segment([graph_pos_to_canvas_pos(prev_x, prev_y, group_i), graph_pos_to_canvas_pos(x, prev_y, group_i)], prev_stroke);
										// Last sample until the end
										if *timestamp < timing.current_timestamp && i + 1 == bit_line.len() {
											let last_x: f32 = timing.convert_timestamp_to_x_value(&*styles, timing.timing_diagram_end());
											let (last_y, last_color) = logic_state_to_graph_y_and_color(*state);
											let last_stroke = Stroke::new(1.0, u8_3_to_color32(last_color));
											// Vertical
											painter.line_segment([graph_pos_to_canvas_pos(x, prev_y, group_i), graph_pos_to_canvas_pos(x, last_y, group_i)], last_stroke);
											// Horizontal
											painter.line_segment([graph_pos_to_canvas_pos(x, last_y, group_i), graph_pos_to_canvas_pos(last_x, last_y, group_i)], last_stroke);
										}
										// Save numbers
										prev_sample = *state;
										prev_x = x;
										prev_y = y;
										prev_stroke = stroke;
									}
								}
								else {// Multiple bits, complicated
									// Current index of each bit line, they won't all be updated the same amount so each needs its own index
									let mut bit_indices: Vec<usize> = (0..signal_group.len()).map(|_| 0).collect();// Splat 0
									// Get first recorded state of each bit
									let mut prev_sample: Vec<LogicState> = signal_group.iter().map(|bit_line| if bit_line.len() == 0 {
										LogicState::Floating
									}
									else {
										bit_line[0].1
									}).collect();
									// Returns: ((N lower, N upper), valid, is contested)
									let get_n_and_whether_valid_from_sample = |sample: &Vec<LogicState>| -> ((u128, u128), bool, bool) {
										let mut valid = true;
										let mut contested = false;
										let mut curr_n: (u128, u128) = (0, 0);
										for (i, state) in sample.iter().enumerate() {
											if state.is_valid() {
												if state.to_bool() {
													if i < 128 {
														curr_n.0 += 1 << i;
													}
													else {
														curr_n.1 += 1 << (i - 128);
													}
												}
											}
											else {
												valid = false;
												contested |= state.is_contested();
											}
										}
										(curr_n, valid, contested)
									};
									// (Binary lower, Binary upper)
									let (mut prev_n_opt, mut prev_sample_contested): (Option<(u128, u128)>, bool) = {
										let (n, valid, contested) = get_n_and_whether_valid_from_sample(&prev_sample);
										(
											match valid {
												true => Some(n),
												false => None
											},
											contested
										)
									};
									// I Love you Haley
									// ((Binary lower, Binary upper), X pos of center)
									let mut bus_labels = Vec::<((u128, u128), f32)>::new();
									let mut latest_bus_label_start: f32 = 0.0;
									let mut end = false;
									// Draw graphics
									let mut draw_bus_segment = |curr_n: (u128, u128), valid: bool, contested: bool, x: f32, prev_x: f32, end: bool| {
										let ((y_low, _), (y_high, _)) = (logic_state_to_graph_y_and_color(LogicState::Driven(false)), logic_state_to_graph_y_and_color(LogicState::Driven(true)));
										let center_pt = graph_pos_to_canvas_pos(x + styles.timing_diagram_bus_half_change_px, (y_high+y_low)/2.0, group_i);
										let stroke_normal = Stroke::new(1.0, u8_3_to_color32(styles.color_foreground));
										let mut both_valid_diff = false;
										let mut diags_from_prev_segment = false;
										// Diagonals from prev segment
										if let Some(prev_n) = prev_n_opt {
											both_valid_diff = (prev_n != curr_n) && valid;
											if both_valid_diff || !valid {
												diags_from_prev_segment = true;
												painter.line_segment(
													[
														graph_pos_to_canvas_pos(x, y_high, group_i),
														center_pt
													],
													stroke_normal
												);
												painter.line_segment(
													[
														center_pt,
														graph_pos_to_canvas_pos(x, y_low, group_i),
													],
													stroke_normal
												);
												// Is it weird for guys to name their dicks?
											}
											if diags_from_prev_segment || end {
												// End current bus
												bus_labels.push((prev_n, (latest_bus_label_start + x)/2.0));
											}
										}
										// Campus is looking really pretty in the fall
										// Diagonals to this segment
										let diags_to_this_segment = both_valid_diff || (prev_n_opt.is_none() && valid);
										if diags_to_this_segment {
											painter.line_segment(
												[
													graph_pos_to_canvas_pos(x + styles.timing_diagram_bus_half_change_px*2.0, y_high, group_i),
													center_pt
												],
												stroke_normal
											);
											painter.line_segment(
												[
													center_pt,
													graph_pos_to_canvas_pos(x + styles.timing_diagram_bus_half_change_px*2.0, y_low, group_i),
												],
												stroke_normal
											);
											latest_bus_label_start = x;
										}
										// horiz line(s)
										if prev_n_opt.is_some() {
											let start_x = /*(sample_i_f32+DIAGONAL_HALF_WIDTH*2.0)*wavelength;*/match diags_to_this_segment {
												true => prev_x + styles.timing_diagram_bus_half_change_px*2.0,
												false => prev_x
											};
											painter.line_segment(
												[
													graph_pos_to_canvas_pos(start_x, y_high, group_i),
													graph_pos_to_canvas_pos(x, y_high, group_i)
												],
												stroke_normal
											);
											painter.line_segment(
												[
													graph_pos_to_canvas_pos(start_x, y_low, group_i),
													graph_pos_to_canvas_pos(x, y_low, group_i)
												],
												stroke_normal
											);
										}
										else {
											let start_x = match diags_from_prev_segment {
												true => prev_x + styles.timing_diagram_bus_half_change_px,
												false => prev_x
											};
											let color: [u8; 3] = if prev_sample_contested {
												styles.color_wire_contested
											}
											else {
												styles.color_wire_floating
											};
											let (y_mid, _) = logic_state_to_graph_y_and_color(LogicState::Floating);
											painter.line_segment(
												[
													graph_pos_to_canvas_pos(start_x, y_mid, group_i),
													graph_pos_to_canvas_pos(x, y_mid, group_i)
												],
												Stroke::new(1.0, u8_3_to_color32(color))
											);
										}
										if valid {
											prev_n_opt = Some(curr_n);
										}
										else {
											prev_n_opt = None;
										}
										prev_sample_contested = contested;
									};
									// Iterate signal samples
									while !end {
										// Find first bit to change
										let mut current_sample: Vec<LogicState> = prev_sample.clone();
										let mut new_timestamp = timing.current_timestamp;
										// Fixed by Gemini
										let next_timestamps: Vec<TimingDiagramTimestamp> = signal_group.iter().enumerate().map(
											|(bit_line_i, bit_line)| {
												let current_idx = bit_indices[bit_line_i];
												if bit_line.is_empty() || current_idx + 1 >= bit_line.len() {
													// If empty or at the last recorded sample, use the diagram's end time
													timing.timing_diagram_end() 
												}
												else {
													// Use the timestamp of the NEXT sample
													bit_line[current_idx + 1].0
												}
											}
										).collect();
										// Get time of next bit change, closest next change
										for timestamp in &next_timestamps {
											if timing.compare_timestamps_for_display(*timestamp, new_timestamp).is_le() {
												new_timestamp = *timestamp;
											}
										}
										// Fixed by Gemini
										// Find bit lines to step forward, ones that changed at `new_timestamp`
										let mut bit_lines_to_advance = Vec::<usize>::new();
										for (bit_line_i, timestamp) in next_timestamps.iter().enumerate() {
											// Check if this bit line's NEXT change time is equal to the earliest change time
											if *timestamp <= new_timestamp { 
												bit_lines_to_advance.push(bit_line_i);
											}
										}
										// Update current sample and increment index for the bit lines that advance to the new timestamp
										for bit_line_i in bit_lines_to_advance {
											let current_idx = bit_indices[bit_line_i];
											// We only update the current state if it hasn't already reached the last sample
											if current_idx + 1 < signal_group[bit_line_i].len() {
												// Increment the index
												bit_indices[bit_line_i] += 1;
												// Update current sample using the newly incremented index
												current_sample[bit_line_i] = signal_group[bit_line_i][bit_indices[bit_line_i]].1;
											}
											// If the index was already at the last sample, current_sample is not updated
										}
										// End condition: Check if all `bit_indices` are endmaxxing
										let mut bit_idices_endmaxxing: usize = 0;
										for (bit_line_i, bit_i) in bit_indices.iter().enumerate() {
											if *bit_i + 1 == signal_group[bit_line_i].len() {
												bit_idices_endmaxxing += 1;
											}
										}
										end |= bit_idices_endmaxxing == signal_group.len();
										let x: f32 = timing.convert_timestamp_to_x_value(&*styles, new_timestamp);
										assert!(current_sample.len() > 0, "Signal group must have at least one bit");
										// Compile binary number, quit if any states are floating or contested
										let (curr_n, valid, contested): ((u128, u128), bool, bool) = get_n_and_whether_valid_from_sample(&current_sample);
										draw_bus_segment(curr_n, valid, contested, x, prev_x, false);
										if end {
											let last_x = timing.convert_timestamp_to_x_value(&*styles, timing.timing_diagram_end());
											let last_sample: Vec<LogicState> = signal_group.iter().map(|bit_line| match bit_line.last() {
												Some(t) => t.1,
												None => LogicState::Floating
											}).collect();
											let (last_n, last_valid, last_contested): ((u128, u128), bool, bool) = get_n_and_whether_valid_from_sample(&last_sample);
											draw_bus_segment(last_n, last_valid, last_contested, last_x, x, true);
										}
										prev_sample = current_sample;
										prev_x = x;
									}
									// Bus labels
									for (n_256, label_center_x) in bus_labels {
										let mut text = format!("{:X}", n_256.0);
										if n_256.1 != 0 {
											text += &format!("{:X}", n_256.1);
										}
										let font_id = FontId::new(amplitude*1.5, FontFamily::Monospace);
										painter.text(
											graph_pos_to_canvas_pos(label_center_x, logic_state_to_graph_y_and_color(LogicState::Floating).0, group_i),
											Align2::CENTER_CENTER,
											text,
											font_id,
											u8_3_to_color32(styles.text_color)
										);
									}
								}
							}
						});
					});
				}
			});
		});
	}
	pub fn paste(&self, item_set: CopiedItemSet) -> Vec<GraphicSelectableItemRef> {
		let mut out = Vec::<GraphicSelectableItemRef>::new();
		for pasted_item in item_set.items {
			out.push(match pasted_item {
				CopiedGraphicItem::Component(comp_save) => self.insert_component(&comp_save),
				CopiedGraphicItem::ExternalConnection(pos, dir, name, show_name, bit_width) => {
					GraphicSelectableItemRef::Pin(self.insert_graphic_pin(pos, dir, name, show_name, bit_width))
				},
				CopiedGraphicItem::Wire((pos, dir, len)) => {
					let wire_ids = self.add_wire_geometry(vec![(pos, dir)], pos + dir.to_unit_int().mult(len as i32));
					GraphicSelectableItemRef::Wire(wire_ids[0])// There shoud be exactly one
				},
				CopiedGraphicItem::Splitter(save) => self.insert_splitter(save),
				CopiedGraphicItem::GraphicLabel(save) => self.insert_label(save),
				CopiedGraphicItem::Probe(save) => self.insert_probe(save)
			});
		}
		// Set each item's pre-drag position to the difference from the BB center it it's position
		for item_ref in &out {
			self.run_function_on_graphic_item_mut(item_ref.clone(), |item_box| {item_box.get_ui_data_mut().position_before_dragging = item_box.get_ui_data().position - item_set.bb_center;});
		}
		out
	}
	fn selected_bb(&self, selected_graphics: &HashSet<GraphicSelectableItemRef>) -> Option<(V2, V2)> {
		let mut bb_opt = Option::<(V2, V2)>::None;
		for item_ref in selected_graphics.iter() {
			if let Some(bb_float) = self.run_function_on_graphic_item::<(V2, V2)>(item_ref.clone(), |item_box| item_box.bounding_box(V2::zeros())) {
				bb_opt = Some(match bb_opt.clone() {
					Some(bb) => {
						merge_points_to_bb(vec![bb_float.0, bb_float.1, bb.0, bb.1])
					}
					None => bb_float
				});
			}
		}
		bb_opt
	}
	pub fn rotate_selection(&self, selected_graphics: &HashSet<GraphicSelectableItemRef>, cw: bool) {
		// Only do anything of there's at least one thing selected
		if let Some(bb) = self.selected_bb(selected_graphics) {
			let bb_center: IntV2 = round_v2_to_intv2((bb.1 + bb.0) / 2.0);
			for item_ref in selected_graphics.iter() {
				self.run_function_on_graphic_item_mut(item_ref.clone(), |item_box| {
					let ui_data: &mut UIData = item_box.get_ui_data_mut();
					let rotate_dir: FourWayDir = if cw {
						ui_data.direction = ui_data.direction.turn_cw();
						FourWayDir::S
					}
					else {
						ui_data.direction = ui_data.direction.turn_ccw();
						FourWayDir::N
					};
					ui_data.position = rotate_dir.rotate_intv2(ui_data.position - bb_center) + bb_center;
					ui_data.position_before_dragging = rotate_dir.rotate_intv2(ui_data.position_before_dragging);
				});
			}
		}
	}
	/// Flips anything with direction W or E if horiz, otherwise anything with N or S
	pub fn flip_selection(&self, selected_graphics: &HashSet<GraphicSelectableItemRef>, horiz: bool) {
		// Only do anything of there's at least one thing selected
		if let Some(bb) = self.selected_bb(selected_graphics) {
			let bb_center: IntV2 = round_v2_to_intv2((bb.1 + bb.0) / 2.0);
			for item_ref in selected_graphics.iter() {
				self.run_function_on_graphic_item_mut(item_ref.clone(), |item_box| {
					let ui_data: &mut UIData = item_box.get_ui_data_mut();
					// Fixed by Gemini
					if horiz {
						// Check if the item has a horizontal direction to flip
						if ui_data.direction == FourWayDir::E || ui_data.direction == FourWayDir::W {
							// Flip the item's direction (e.g., East becomes West)
							ui_data.direction = ui_data.direction.opposite_direction();
						}
						// Reflect the item's absolute position across the center's X-coordinate
						ui_data.position.0 = 2 * bb_center.0 - ui_data.position.0;
						// Reflect the relative dragging offset by negating its X-component
						ui_data.position_before_dragging.0 = -ui_data.position_before_dragging.0;
					} else { // Vertical flip
						// Check if the item has a vertical direction to flip
						if ui_data.direction == FourWayDir::S || ui_data.direction == FourWayDir::N {
							// Flip the item's direction (e.g., North becomes South)
							ui_data.direction = ui_data.direction.opposite_direction();
						}
						// Reflect the item's absolute position across the center's Y-coordinate
						ui_data.position.1 = 2 * bb_center.1 - ui_data.position.1;
						// Reflect the relative dragging offset by negating its Y-component
						ui_data.position_before_dragging.1 = -ui_data.position_before_dragging.1;
					}
				});
			}
		}
	}
	pub fn recompute_internals_bb(&mut self) {
		let mut bb_opt = Option::<(V2, V2)>::None;
		for item_ref in self.get_all_graphics_references() {
			if let GraphicSelectableItemRef::Pin(_) = &item_ref {
				continue;
			}
			self.run_function_on_graphic_item(item_ref.clone(), |item_box| {
				let new_bb = item_box.bounding_box(V2::zeros());
				if let Some(bb) = &mut bb_opt {
					*bb = merge_points_to_bb(vec![bb.0, bb.1, new_bb.0, new_bb.1]);
				}
				else {
					bb_opt = Some(new_bb);
				}
			});
		}
		if let Some(bb) = bb_opt {
			self.circuit_internals_bb = bb;
		}
		else {
			self.circuit_internals_bb = (V2::zeros(), V2::zeros())
		}
	}
	fn was_anything_clicked<'a>(&self, grid_pos: V2, multi_select_key: bool) -> Option<GraphicSelectableItemRef> {
		let mut selected_opt = Option::<GraphicSelectableItemRef>::None;
		for ref_ in self.get_all_graphics_references() {
			self.run_function_on_graphic_item_mut(ref_.clone(), |graphic_item| {
				if graphic_item.is_click_hit(grid_pos, V2::zeros()) {
					if multi_select_key {
						selected_opt = Some(ref_.clone());
					}
					else if !graphic_item.accept_click(graphic_item.get_ui_data().parent_pos_to_local_coords_float(grid_pos)) {
						selected_opt = Some(ref_.clone());
					}
				}
			});
		}
		selected_opt
	}
	fn copy_graphic_item(&self, ref_: GraphicSelectableItemRef) -> Option<CopiedGraphicItem> {
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => {
				let components_ref = self.components.borrow();
				let comp_rc = components_ref.get(&comp_id)?;
				let comp_borrow = comp_rc.borrow();
				Some(comp_borrow.copy())
			}
			GraphicSelectableItemRef::Wire(wire_id) => {
				let wires_ref = self.wires.borrow();
				let wire_rc = wires_ref.get(&wire_id)?;
				let wire_borrow = wire_rc.borrow();
				Some(wire_borrow.copy())
			}
			GraphicSelectableItemRef::Pin(pin_id) => {
				let pins_ref = self.generic_device.graphic_pins.borrow();
				let pin = pins_ref.get(&pin_id)?;
				Some(pin.copy())
			},
			GraphicSelectableItemRef::Splitter(splitter_id) => {
				let splitters = self.splitters.borrow();
				let splitter = splitters.get(&splitter_id)?;
				Some(splitter.copy())
			},
			GraphicSelectableItemRef::GraphicLabel(label_id) => {
				let labels = self.labels.borrow();
				let label = labels.get(&label_id)?;
				Some(label.copy())
			},
			GraphicSelectableItemRef::Probe(probe_id) => {
				let probes = self.probes.borrow();
				let probe = probes.get(&probe_id)?;
				Some(probe.copy())
			}
		}
	}
}

pub struct LogicCircuitToplevelView {
	/// The top-level circuit of this view, its pins are rendered as interactive I/O pins
	circuit: LogicCircuit,
	/// Location of center of screen with respect to the grid, it is this way so that it will not have to adjusted when the grid is zoomed in/out
	screen_center_wrt_grid: V2,
	/// Pixels per grid increment
	grid_size: f32,
	logic_loop_error: bool,
	showing_component_popup: bool,
	showing_block_edit_popup: bool,
	component_search_text: String,
	all_logic_devices_search: Vec<EnumAllLogicDevices>,
	saved: bool,
	recompute_conns_next_frame: bool,
	/// 1 Less than actual number of times the compute function was called because the last call doesn't change anything and ends the loop
	frame_compute_cycles: usize,
	showing_flatten_opoup: bool,
	flatten_error: Option<String>,
	timing: TimingDiagram,
}

impl LogicCircuitToplevelView {
	pub fn new(circuit: LogicCircuit, saved: bool, styles: &Styles) -> Self {
		let timing_probe_order = circuit.timing_probe_order.clone();
		let mut out = Self {
			circuit,
			screen_center_wrt_grid: V2::zeros(),
			grid_size: styles.default_grid_size,
			logic_loop_error: false,
			showing_component_popup: false,
			showing_block_edit_popup: false,
			component_search_text: String::new(),
			all_logic_devices_search: Vec::new(),
			saved,
			recompute_conns_next_frame: false,
			frame_compute_cycles: 0,
			showing_flatten_opoup: false,
			flatten_error: None,
			timing: TimingDiagram::from_probe_id_list(&timing_probe_order)
		};
		out.circuit.update_probe_net_connections_and_timing(&mut out.timing);
		// Done
		out
	}
	/// Returns: (Optional position to set the mouse to, Optional new circuit tab to open)
	pub fn draw(&mut self, ui: &mut Ui, styles: Rc<Styles>, #[allow(unused)]screen_top_left: Pos2) -> (Option<Pos2>, Option<String>) {
		#[allow(unused_mut)]
		let mut return_new_mouse_pos = Option::<Pos2>::None;
		let mut return_new_circuit_tab = Option::<String>::None;
		let canvas_size: Vec2 = ui.available_size_before_wrap();
		let anything_in_focus: bool = ui.memory(|memory| memory.focused().is_some());
		let inner_response = Frame::canvas(ui.style()).show::<(Vec2, V2)>(ui, |ui| {
			// First, detect user unput
			let input_state = ui.ctx().input(|i| i.clone());
			let (response, painter) = ui.allocate_painter(canvas_size, Sense::all());
			let rect_center: V2 = emath_pos2_to_v2(response.rect.center());
			let draw_info = EguiDrawInterface::new(
				DrawData::new(
					self.screen_center_wrt_grid,
					self.grid_size,
					IntV2(0, 0),
					FourWayDir::default(),
					Rc::clone(&styles),
					rect_center,
					emath_vec2_to_v2(canvas_size),
					response.hover_pos().map(|v| emath_pos2_to_v2(v))
				),
				Rc::new(painter)
			);
			// Scrolling
			let scroll = input_state.raw_scroll_delta.y;
			if scroll != 0.0 {
				// Set mouse position to center of screen and move grid offset along with it, inspired by KiCad
				#[cfg(feature = "kicad_scrolling")]
				if let Some(og_mouse_pos) = response.hover_pos() {
					// Make sure its rounded
					let shift_grid: IntV2 = round_v2_to_intv2(emath_vec2_to_v2(response.rect.center() - og_mouse_pos) / self.grid_size);
					return_new_mouse_pos = Some(response.rect.center() + screen_top_left.to_vec2());
					self.screen_center_wrt_grid -= V2::new(shift_grid.0 as f32, -shift_grid.1 as f32);
				};
				// Scroll
				let scale = styles.grid_scale_factor.powf(scroll);
				let new_grid_unclamped = self.grid_size.scale(scale);
				#[cfg(not(feature = "kicad_scrolling"))]
				let old_grid_size = self.grid_size;
				self.grid_size = if new_grid_unclamped < styles.max_grid_size {
					if new_grid_unclamped > styles.min_grid_size {
						new_grid_unclamped
					}
					else {
						styles.min_grid_size
					}
				}
				else {
					styles.max_grid_size
				};
				#[cfg(not(feature = "kicad_scrolling"))]
				if let Some(og_mouse_pos) = response.hover_pos() {
					let shift_px = emath_vec2_to_v2(response.rect.center() - og_mouse_pos);
					let shift_grid: V2 = (shift_px / self.grid_size) - (shift_px / old_grid_size);
					self.screen_center_wrt_grid += V2::new(shift_grid.x, -shift_grid.y);
				};
			}
			//let recompute_connections: bool = self.circuit.toplevel_ui_interact(response, ui.ctx(), &draw_info, input_state);
			if !anything_in_focus {
				self.recompute_conns_next_frame |= self.circuit.toplevel_ui_interact(
					response,
					ui.ctx(),
					input_state,
					self.grid_size,
					|pos_px: Pos2| -> V2 {
						DrawData::mouse_pos2_to_grid_unattached(V2::new(pos_px.x, pos_px.y), FourWayDir::default(), rect_center, IntV2(0, 0), self.screen_center_wrt_grid, emath_vec2_to_v2(canvas_size), self.grid_size)
					}
				);
			}
			// Reconnect wires and thingies if anything was changed
			if self.recompute_conns_next_frame {
				self.saved = false;
				self.circuit.check_wire_geometry_and_connections(Some(&mut self.timing));
			}
			// Update
			self.logic_loop_error = self.propagate_until_stable(CIRCUIT_MAX_COMPUTE_CYCLES);
			self.recompute_conns_next_frame = false;
			// graphics help from https://github.com/emilk/egui/blob/main/crates/egui_demo_lib/src/demo/painting.rs
			// Right side toolbar
			self.circuit.tool.borrow().tool_select_ui(&draw_info);
			// Draw circuit
			self.circuit.draw(&(Box::new(draw_info) as Box<dyn DrawInterface>));
			(canvas_size, rect_center)
		});
		// Top: general controls
		Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::LEFT_TOP, child: Align2::LEFT_TOP}).id("top-left controls".into()).show(|ui| {
			if ui.button("Save").clicked() {
				self.circuit.save_circuit().unwrap();
				self.saved = true;
			}
			if self.circuit.tool.borrow().tool_select_allowed() {
				if ui.button("+ Component / Subcircuit").clicked() {
					// Update component search list
					self.all_logic_devices_search = builtin_components::list_all_basic_components();
					for file_path in resource_interface::list_all_circuit_files().unwrap() {
						self.all_logic_devices_search.push(EnumAllLogicDevices::SubCircuit(file_path, false, IntV2(0, 0), FourWayDir::default(), String::new()));
					}
					self.showing_component_popup = true;
				}
				if ui.button("+ I/O Pin").clicked() {
					let graphic_pin_id = self.circuit.insert_graphic_pin(IntV2(0, 0), FourWayDir::default(), String::new(), true, 1);
					self.circuit.set_graphic_item_following_mouse(GraphicSelectableItemRef::Pin(graphic_pin_id));
					self.circuit.update_pin_block_positions();
				}
				if ui.button("+ Splitter").clicked() {
					self.circuit.set_graphic_item_following_mouse(self.circuit.insert_splitter(Splitter::new().save()));
				}
				if ui.button("+ Label").clicked() {
					self.circuit.set_graphic_item_following_mouse(self.circuit.insert_label(GraphicLabel::new().save()));
				}
				if ui.button("+ Probe").clicked() {
					self.circuit.set_graphic_item_following_mouse(self.circuit.insert_probe(Probe::default().save()));
				}
			}
			// Compute cycles text
			match self.frame_compute_cycles == CIRCUIT_MAX_COMPUTE_CYCLES {
				true => {ui.colored_label(u8_3_to_color32([255, 0, 0]), format!("Compute cycles: {}", self.frame_compute_cycles));}
				false => {ui.label(format!("Compute cycles: {}", self.frame_compute_cycles));}
			}
			// Bit width errors
			let n_bit_widths: usize = self.circuit.bit_width_errors.len();
			if n_bit_widths > 0 {
				ui.colored_label(u8_3_to_color32([255, 0, 0]), format!("{} Incompatible bit width islands", n_bit_widths));
			}
			// Circuit settings (clock speed, etc)
			ui.collapsing("Circuit Settings", |ui| {
				ui.horizontal(|ui| {
					ui.label("Name");
					ui.add(TextEdit::singleline(&mut self.circuit.type_name).desired_width(100.0));
				});
				ui.horizontal(|ui| {
					let mut fixed_cycles_enabled = self.circuit.fixed_sub_cycles_opt.is_some();
					ui.checkbox(&mut fixed_cycles_enabled, "Fixed sub cycles");
					if fixed_cycles_enabled {
						if let Some(cycles) = &mut self.circuit.fixed_sub_cycles_opt {
							let mut cycles_i32 = *cycles as i32;
							ui_drag_value_with_arrows(ui, &mut cycles_i32, Some((1, 1000)));
							*cycles = cycles_i32 as usize;
						}
						else {
							self.circuit.fixed_sub_cycles_opt = Some(1);
						}
					}
					else {
						self.circuit.fixed_sub_cycles_opt = None;
					}
				});
				// Clock
				ui.label("Clock");
				let mut clock = self.circuit.clock.borrow_mut();
				ui.horizontal(|ui| {
					ui.checkbox(&mut clock.enabled, "Enable");
					ui.label("Freq:");
					ui.add(DragValue::new(&mut clock.freq).range(RangeInclusive::new(0.01, 1000000.0)).clamp_existing_to_range(false));
					ui.label("State:");
					if ui.button(match &mut clock.state {
						true => "1",
						false => "0"
					}).clicked() {
						clock.state = !clock.state;
					}
				});
				if ui.button("Edit block layout...").clicked() {
					self.showing_block_edit_popup = true;
				}
				if ui.button("Flatten circuit...").clicked() {
					self.showing_flatten_opoup = true;
					self.flatten_error = None;
				}
			});
			// Active selection features
			if let Tool::Select{selected_graphics, selected_graphics_state: _} = &*self.circuit.tool.borrow() {
				if !selected_graphics.is_empty() {
					// Properties list
					ui.separator();
					ui.label("Properties");
					// All different variants of `SelectProperty`
					// Vec<(Property, whether they are all the same, Set of selected items to update when property is edited)>
					let mut unique_properties = Vec::<(SelectProperty, bool, HashSet<GraphicSelectableItemRef>)>::new();
					for graphic_handle in selected_graphics.iter() {
						if let Some(new_properties) = self.circuit.run_function_on_graphic_item(graphic_handle.clone(), |item_box| item_box.get_properties()) {
							for property in new_properties {
								// Check if property enum variant is already included in `unique_properties`
								let mut variant_included = false;// Optional index of `unique_properties`
								for (prop_test, are_all_same, graphic_item_set) in unique_properties.iter_mut() {
									if prop_test.ui_name() == property.ui_name() {
										variant_included = true;
										if *prop_test != property {
											*are_all_same = false;
										}
										graphic_item_set.insert(graphic_handle.clone());
									}
								}
								if !variant_included {
									unique_properties.push((
										property,
										true,
										HashSet::from_iter(vec![graphic_handle.clone()].into_iter())
									));
								}
							}
						}
					}
					// Display them
					for (prop, _, graphic_item_set) in unique_properties.iter_mut() {
						ui.horizontal(|ui| {
							ui.label(format!("{}:", prop.ui_name()));
							if prop.add_to_ui(ui) {
								self.saved = false;
								self.recompute_conns_next_frame = true;
								for graphic_handle in graphic_item_set.iter() {
									self.circuit.run_function_on_graphic_item_mut(graphic_handle.clone(), |item_box| {item_box.set_property(prop.clone());});
								}
							}
						});
					}
				}
			}
		});
		if self.showing_component_popup {
			Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
				ui.horizontal(|ui| {
					ui.label("Add Component / Sub-circuit");
					if ui.button("Cancel").clicked() {
						self.showing_component_popup = false;
					}
				});
				ui.text_edit_singleline(&mut self.component_search_text).request_focus();
				ScrollArea::vertical().show(ui, |ui| {
					for device_save in &self.all_logic_devices_search {
						if device_save.type_name().to_ascii_lowercase().contains(&self.component_search_text.to_ascii_lowercase()) {
							if ui.button(device_save.type_name()).clicked() {
								self.showing_component_popup = false;
								let handle = self.circuit.insert_component(device_save);
								*self.circuit.tool.borrow_mut() = Tool::Select{
									selected_graphics: HashSet::from_iter(vec![handle].into_iter()),
									selected_graphics_state: SelectionState::FollowingMouse(V2::zeros())
								};
							}
						}
					}
				});
			});
		}
		else if self.showing_block_edit_popup {
			Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
				self.edit_block_layout(ui, styles, inner_response.inner.0);
			});
		}
		else if self.showing_flatten_opoup {
			Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
				ui.label("Recursively flatten sub circuits");
				ui.separator();
				ui.label(format!("This feature will create a copy of this circuit but all the wires and components of sub-circuits extracted into the \"top layer\". Sub-circuits with fixed compute cycles will not be extracted but flattened versions of them will still be created. The intended use-case of this is to make a circuit faster to simulate. The flattened copy file size will be approximately the sum of all sub-circuits plus the toplevel wires and components. Running this function might create large files in the {} directory.", resource_interface::CIRCUITS_DIR));
				ui.horizontal(|ui| {
					if ui.button("Flatten Circuit").clicked() {
						match self.circuit.flatten(true) {
							Ok(device_save) => if let EnumAllLogicDevices::SubCircuit(save_path, _, _, _, _) = device_save {
								return_new_circuit_tab = Some(save_path);
								self.showing_flatten_opoup = false;
							}
							else {
								panic!("Circuit flatten() should return the EnumAllLogicDevices::SubCircuit variant of EnumAllLogicDevices");
							},
							Err(e) => {
								self.flatten_error = Some(e)
							}
						}
					}
					if ui.button("Cancel").clicked() {
						self.showing_flatten_opoup = false;
					}
				});
				if let Some(flatten_error) = &self.flatten_error {
					ui.colored_label(u8_3_to_color32([255, 0, 0]), flatten_error);
				}
			});
		}
		else {
			/*Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
				ScrollArea::both().show(ui, self.)
			});*/
			Window::new("Timing Diagram").anchor(Align2::RIGHT_TOP, Vec2::new(0.0, inner_response.response.rect.top())).collapsible(true).resizable(true).show(ui.ctx(), |ui| self.circuit.show_timing_diagram_ui(ui, styles, &mut self.timing));
		}
		(return_new_mouse_pos, return_new_circuit_tab)
	}
	/// Runs `compute_step()` repeatedly on the circuit until there are no changes, there must be a limit because there are circuits (ex. NOT gate connected to itself) where this would otherwise never end
	pub fn propagate_until_stable(&mut self, propagation_limit: usize) -> bool {
		let start_t = Instant::now();
		loop {
			let mut count: usize = 0;
			while count < propagation_limit {
				if !self.circuit.compute_immutable(&AncestryStack::new(), 0, count == 0) {
					/*if count > 0 {
						self.circuit.update_timing_diagram(&mut self.circuit.propagation_done.borrow_mut(), &mut self.timing, count == 0);
					}*/
					self.circuit.update_timing_diagram(&mut self.circuit.propagation_done.borrow_mut(), &mut self.timing, count == 0);
					self.frame_compute_cycles = count;
					return false;
				}
				self.circuit.update_timing_diagram(&mut self.circuit.propagation_done.borrow_mut(), &mut self.timing, count == 0);
				count += 1;
			}
			self.frame_compute_cycles = count;
			// Keep going unless the clock isn't gonna change or the UI frame time limit is reached
			if (count < propagation_limit && self.circuit.clock.borrow().would_update().is_none()) || (Instant::now() - start_t).as_secs_f32() >= UI_MAX_FRAME_SIMULATION_TIME {
				break;
			}
		}
		return true;
	}
	fn edit_block_layout(&mut self, ui: &mut Ui, styles: Rc<Styles>, maine_frame_size: Vec2) {
		let inner_response = Frame::canvas(ui.style()).show(ui, |ui| {
			//let canvas_size = ui.available_size_before_wrap();
			let (response, painter) = ui.allocate_painter(maine_frame_size, Sense::all());
			let draw_info = EguiDrawInterface::new(
				DrawData::new(
					self.screen_center_wrt_grid,
					self.grid_size,
					IntV2(0, 0),
					FourWayDir::default(),
					styles,
					emath_pos2_to_v2(response.rect.center()),
					emath_vec2_to_v2(maine_frame_size),
					None
				),
				Rc::new(painter)
			);
			self.circuit.draw_as_block(&(Box::new(draw_info) as Box<dyn DrawInterface>), true);
		});
		//inner_response.response.ctx.input_mut(|input| input.events.clear());
		Popup::from_response(&inner_response.response).open(true).align(RectAlign{parent: Align2::LEFT_TOP, child: Align2::LEFT_TOP}).id("block edit controls".into()).show(|ui| {
			ScrollArea::vertical().scroll_bar_visibility(ScrollBarVisibility::AlwaysHidden).show(ui, |ui| {
				let mut pins = self.circuit.generic_device.graphic_pins.borrow_mut();
				for (pin_id, pin) in pins.iter_mut() {
					let pin_config: &mut (IntV2, FourWayDir, bool) = self.circuit.block_pin_positions.get_mut(pin_id).unwrap();
					ui.horizontal(|ui| {
						ui.add(TextEdit::singleline(&mut pin.name).desired_width(50.0));
						ui.checkbox(&mut pin.show_name, "");
						ui.separator();
						ui.label("X:");
						if ui.button("<").clicked() {
							pin_config.0.0 -= 1;
						}
						ui.add(DragValue::new(&mut pin_config.0.0));
						if ui.button(">").clicked() {
							pin_config.0.0 += 1;
						}
						ui.separator();
						ui.label("Y:");
						if ui.button("<").clicked() {
							pin_config.0.1 -= 1;
						}
						ui.add(DragValue::new(&mut pin_config.0.1));
						if ui.button(">").clicked() {
							pin_config.0.1 += 1;
						}
						ui.separator();
						ui.label("Dir:");
						if ui.button("↶").clicked() {
							pin_config.1 = pin_config.1.turn_ccw();
						}
						if ui.button("↷").clicked() {
							pin_config.1 = pin_config.1.turn_cw();
						}
					});
				}
				ui.horizontal(|ui| {
					let bb_float: (V2, V2) = self.circuit.generic_device.ui_data.local_bb;
					let mut bb: (IntV2, IntV2) = (round_v2_to_intv2(bb_float.0), round_v2_to_intv2(bb_float.1));
					ui.label("Box dimensions:");
					ui.separator();
					ui.label("X min:");
					ui.add(DragValue::new(&mut bb.0.0));
					ui.label("X max:");
					ui.add(DragValue::new(&mut bb.1.0));
					ui.label("Y min:");
					ui.add(DragValue::new(&mut bb.0.1));
					ui.label("Y max:");
					ui.add(DragValue::new(&mut bb.1.1));
					self.circuit.generic_device.ui_data.local_bb = (bb.0.to_v2(), bb.1.to_v2());
				});
			});
			if ui.button("Done").clicked() {
				self.showing_block_edit_popup = false;
			}
		});
	}
}


pub struct App {
	styles: Rc<Styles>,
	circuit_tabs: Vec<LogicCircuitToplevelView>,
	current_tab_index: usize,
	new_circuit_name: String,
	new_circuit_path: String,
	load_circuit_err_opt: Option<String>,
	readme_file: String
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
		Self {
			styles: Rc::new(styles),
			circuit_tabs: Vec::new(),//vec![LogicCircuitToplevelView::new(create_simple_circuit(), false)],
			current_tab_index: 0,
			new_circuit_name: String::new(),
			new_circuit_path: String::new(),
			load_circuit_err_opt: None,
			readme_file: resource_interface::load_file_with_better_error("README.md").unwrap()
		}
	}
	fn load_circuit_tab(&mut self, file_path: &str) {
		match resource_interface::load_circuit(file_path, false, true, IntV2(0, 0), FourWayDir::default(), String::new()) {
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
}

impl eframe::App for App {
	fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
		let circuit_names: Vec<String> = self.circuit_tabs.iter().map(|toplevel| toplevel.circuit.type_name.clone()).collect();
		egui::CentralPanel::default().show(ctx, |ui: &mut Ui| {
			// This function by default is only run upon user interaction, so copied this from https://users.rust-lang.org/t/issues-while-writing-a-clock-with-egui/102752
			ui.ctx().request_repaint();
			ScrollArea::horizontal().scroll_bar_visibility(ScrollBarVisibility::AlwaysHidden).show(ui, |ui| {
				ui.horizontal(|ui| {
					if ui.button("Home").clicked() {
						if self.current_tab_index != 0 {
							self.current_tab_index = 0;
						}
					}
					for (i, circuit_name) in circuit_names.iter().enumerate() {
						let name_for_ui: &str = match self.circuit_tabs[i].saved {
							true => circuit_name,
							false => &format!("{} *", circuit_name)
						};
						if ui.button(name_for_ui).clicked() {
							if i + 1 != self.current_tab_index {
								self.current_tab_index = i + 1;
							}
						}
					}
					let new_button_response = ui.button("+");
					if new_button_response.clicked() {
						self.new_circuit_name = String::new();
						self.new_circuit_path = String::new();
						self.load_circuit_err_opt = None;
					}
					Popup::menu(&new_button_response).close_behavior(PopupCloseBehavior::CloseOnClickOutside).align(RectAlign::RIGHT_START).align_alternatives(&[RectAlign::LEFT_START]).show(|ui| {
						ui.menu_button("New Circuit", |ui| {
							ui.horizontal(|ui| {
								ui.label("Name: ");
								ui.text_edit_singleline(&mut self.new_circuit_name);
							});
							ui.horizontal(|ui| {
								ui.label("Save file path: ");
								ui.text_edit_singleline(&mut self.new_circuit_path);
								ui.label(".json");
							});
							if ui.button("Create Circuit").clicked() {
								self.circuit_tabs.push(LogicCircuitToplevelView::new(LogicCircuit::new_mostly_default(self.new_circuit_name.clone(), self.new_circuit_path.clone(), true), false, &self.styles));
								self.current_tab_index = self.circuit_tabs.len();// Not an OBOE
							}
						});
						ui.menu_button("Load Circuit", |ui| {
							let files_list = resource_interface::list_all_circuit_files().unwrap();
							if files_list.len() == 0 {
								ui.label(format!("No circuit files found in {}", resource_interface::CIRCUITS_DIR));
							}
							ScrollArea::vertical().show(ui, |ui| {
								for file_path in files_list {
									if ui.selectable_label(false, &file_path).clicked() {
										self.load_circuit_tab(&file_path);
									}
								}
							});
							if let Some(load_error) = &self.load_circuit_err_opt {
								ui.label(format!("Loading error: {}", load_error));
							}
						});
					});
				});
			});
			if self.current_tab_index == 0 {// Home tab
				ui.vertical(|ui| {
					ScrollArea::vertical().show(ui, |ui| {
						CommonMarkViewer::new().show(ui, &mut CommonMarkCache::default(), &self.readme_file);
					});
				});
			}
			else {
				let circuit_toplevel: &mut LogicCircuitToplevelView = &mut self.circuit_tabs[self.current_tab_index - 1];
				#[allow(unused)]
				let (new_mouse_pos_opt, new_circuit_tab_opt): (Option<Pos2>, Option<String>) = circuit_toplevel.draw(ui, Rc::clone(&self.styles), ctx.screen_rect().min);// TODO: Get actual window top-left position
				#[cfg(feature = "kicad_scrolling")]
				if let Some(new_pos) = new_mouse_pos_opt {
					let mouse = mouse_rs::Mouse::new();
					mouse.move_to(new_pos.x as i32, new_pos.y as i32).unwrap();
				}
				if let Some(new_circuit_tab) = new_circuit_tab_opt {
					self.load_circuit_tab(&new_circuit_tab);
				}
			}
		});
	}
}

fn ui_drag_value_with_arrows<N: emath::Numeric + SubAssign + AddAssign + From<u8> + Into<i32>>(ui: &mut Ui, value: &mut N, range_opt: Option<(N, N)>) -> bool {
	let mut down_enable = true;
	let mut up_enable = true;
	let value_copy = *value;
	if let Some(range) = range_opt {
		if value_copy <= range.0 {
			down_enable = false;
		}
		if value_copy >= range.1 {
			up_enable = false;
		}
	}
	if ui.add_enabled(down_enable, Button::new("<")).clicked() {
		*value -= 1_u8.into();
		return true;
	}
	let mut drag_value = DragValue::new(value);
	if let Some(range) = range_opt {
		drag_value = drag_value.range(RangeInclusive::new(range.0, range.1));
		drag_value = drag_value.clamp_existing_to_range(true);
	}
	if ui.add(drag_value).changed() {
		return true;
	}
	if ui.add_enabled(up_enable, Button::new(">")).clicked() {
		*value += 1_u8.into();
		return true;
	}
	false
}