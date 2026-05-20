//! Only UI stuff

use crate::{builtin_components, prelude::*, resource_interface, simulator::GraphicSelectableItemRef};
use eframe::{egui::{self, containers::Popup, response::Response, scroll_area::ScrollBarVisibility, text::LayoutJob, Align2, Button, Color32, DragValue, FontFamily, FontId, Frame, Galley, Key, KeyboardShortcut, Modifiers, Painter, PointerButton, Pos2, Rect, RectAlign, ScrollArea, Sense, Shape, Stroke, StrokeKind, TextEdit, TextFormat, Ui, Vec2, Window}, emath, epaint::{PathStroke, TextShape}};
use nalgebra::ComplexField;
use serde::{Serialize, Deserialize};
use serde_json;
use std::{cell::RefCell, collections::{HashSet, HashMap}, f32::consts::TAU, ops::{AddAssign, RangeInclusive, SubAssign}, rc::Rc, sync::Arc, time::Instant};
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
	HasPin(String, bool),
	PxGridSize(u8),
	IncludeInTimingDiagram(bool)
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
			Self::HasPin(name, _) => format!("Has {} pin", name),
			Self::PxGridSize(_) => "Px Size Grid".to_owned(),
			Self::IncludeInTimingDiagram(_) => "Include in timing diagram".to_owned()
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
			},
			Self::PxGridSize(px_size) => {
				ui_drag_value_with_arrows(ui, px_size, Some((1, 255)))
			},
			Self::IncludeInTimingDiagram(state) => {
				ui.checkbox(state, "").changed()
			}
		}
	}
}

#[derive(Debug, Serialize, Deserialize)]
pub enum CopiedGraphicItem {
	/// Usual circuit save handle, state config
	Component(EnumAllLogicDevices, Option<ComponentInstanceConfig>),
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
	pub fn toplevel_ui_interact<'a, F: Fn(Pos2) -> V2>(&mut self, response: Response, context: &egui::Context, /*draw: &dyn DrawInterface<'a>,*/ mut input_state: egui::InputState, grid_size: f32, mouse_pos2_to_grid: F, tool: &mut Tool) -> bool {
		let mut return_recompute_connections = false;
		let mut new_tool_opt = Option::<Tool>::None;
		let mut recompute_pin_block_positions = false;
		let mut new_highlighted_net = Option::<Option<u64>>::None;
		let mouse_pos_grid_opt: Option<V2> = match response.hover_pos() {
			Some(pos_px) => Some(mouse_pos2_to_grid(pos_px)),
			None => None
		};
		match tool {
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
			*tool = new_tool;
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
				let amplitude: f32 = 8.5;
				let vert_spacing: f32 = 25.0;
				// Includes actual signal groups and blank spaces for headers
				// After an item corresponding to a header, there will be one extra item indicating whether that header is being displayed
				let mut shown_signal_group_indices = Vec::<bool>::new();
				let mut shown_group_spaces: usize = 0;// Excludes header open flags
				// Recursively generate tree UI and flatten vec of signal groups
				ui.vertical(|ui| {
					Self::show_timing_diagram_ui_recursive(
						ui,
						&timing.tree,
						&*self.probes.borrow(),
						&*self.components.borrow(),
						&styles,
						amplitude,
						vert_spacing,
						&mut shown_signal_group_indices,
						&mut shown_group_spaces
					);
				});
				// Signals
				if timing.n_samples > 0 {
					ScrollArea::horizontal().stick_to_right(true).show(ui, |ui| {
						Frame::canvas(ui.style()).show::<()>(ui, |ui| {
							let canvas_size = Vec2::new(timing.convert_timestamp_to_x_value(&*styles, timing.timing_diagram_end()) + 4.0, shown_group_spaces as f32 * vert_spacing);
							let (response, painter) = ui.allocate_painter(canvas_size, Sense::empty());
							let graph_pos_to_canvas_pos = |graph_x: f32, graph_y: f32, group_i: usize| -> Pos2 {
								Pos2::new(graph_x + response.rect.left() + 2.0, (-graph_y) + (group_i as f32 + 0.5)*vert_spacing + response.rect.top())
							};
							// New propagation event vertical marker lines
							if let TimingDiagramTimestamp::PropagationAndSimStep(_, _) = &timing.current_timestamp {
								if timing.tree.len() > 0 {
									for i in 0_u32..(timing.propagation_steps.len() as u32) {
										let x = timing.convert_timestamp_to_x_value(&*styles, TimingDiagramTimestamp::PropagationAndSimStep(i, 0));
										painter.line_segment(
											[
												graph_pos_to_canvas_pos(x, amplitude, 0),
												graph_pos_to_canvas_pos(x, -amplitude, shown_group_spaces - 1)],
											Stroke::new(0.7, u8_3_to_color32([128, 128, 128]))
										);
									}
								}
							}
							// Iterate through tree
							let mut queue = Vec::<&TimingDiagramTreeNode>::new();
							let mut group_i_total: usize = 0;// For indexing into `shown_signal_group_indices`
							let mut group_i_graphic: usize = 0;// For vertical placement
							queue.extend_from_slice(timing.tree.iter().rev().collect::<Vec<&TimingDiagramTreeNode>>().as_slice());
							while !queue.is_empty() {
								let node = queue.pop().unwrap();
								if shown_signal_group_indices[group_i_total] {
									match node {
										TimingDiagramTreeNode::Leaf(_, signal_group) => {
											timing.show_signal_group(signal_group, graph_pos_to_canvas_pos, &styles, amplitude, &painter, group_i_graphic);
										},
										TimingDiagramTreeNode::Branch(_, sub_tree) => {
											// Test if long enough for sub tree to have contents
											// Check branch open flag
											group_i_total += 1;
											if shown_signal_group_indices[group_i_total] {
												queue.extend_from_slice(sub_tree.iter().rev().collect::<Vec<&TimingDiagramTreeNode>>().as_slice());
											}
											else {
												// Otherwise skip past hidden sub tree elements
												//group_i_total += sub_tree.len()
											}
										}
									}
									group_i_graphic += 1;
								}
								group_i_total += 1;
							}
						});
					});
				}
			});
		});
	}
	fn show_timing_diagram_ui_recursive(
		ui: &mut Ui,
		tree: &Vec<TimingDiagramTreeNode>,
		probes: &HashMap<u64, Probe>,
		components: &HashMap<u64, RefCell<Box<dyn LogicDevice>>>,
		styles: &Rc<Styles>,
		amplitude: f32,
		vert_spacing: f32,
		shown_signal_group_indices: &mut Vec<bool>,
		shown_group_spaces: &mut usize
	) {
		ui.add_space(8.0);
		let vert_widget_extra_spacing = ui.style().spacing.item_spacing.y;
		for node in tree {
			match node {
				TimingDiagramTreeNode::Leaf(source, _) => {
					let height = ui.label(match source {
						TimingDiagramSignalGroupSource::Clk => "CLK",
						TimingDiagramSignalGroupSource::Probe(probe_id) => &(probes.get(probe_id).unwrap().name)
					}).rect.height() + vert_widget_extra_spacing;
					ui.add_space(vert_spacing - height);
					shown_signal_group_indices.push(true);
					*shown_group_spaces += 1;
				},
				TimingDiagramTreeNode::Branch(comp_id, sub_tree) => {
					shown_signal_group_indices.push(true);// Always leave room for header
					*shown_group_spaces += 1;
					let open_flag_index = shown_signal_group_indices.len();
					let comp_cell = components.get(comp_id).unwrap();
					let comp = comp_cell.borrow();
					let mut header_open = false;
					ui.collapsing(&comp.get_generic().name, |ui_inner: &mut Ui| {
						header_open = true;
						let circuit = comp.get_circuit();
						LogicCircuit::show_timing_diagram_ui_recursive(ui_inner, sub_tree, &*circuit.probes.borrow(), &*circuit.components.borrow(), &styles, amplitude, vert_spacing, shown_signal_group_indices, shown_group_spaces);
					});
					// If closed then put one false to flag the branch as closed
					shown_signal_group_indices.insert(open_flag_index, header_open);
					if !header_open {
						ui.add_space(4.0);
					}
				}
			}
		}
	}
	pub fn paste(&self, item_set: CopiedItemSet) -> Vec<GraphicSelectableItemRef> {
		let mut out = Vec::<GraphicSelectableItemRef>::new();
		for pasted_item in item_set.items {
			out.push(match pasted_item {
				CopiedGraphicItem::Component(comp_save, inst_config_opt) => self.insert_component(&comp_save, inst_config_opt.as_ref()),
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

struct MoveCircuitPopup {
	old_file_name: String,
	old_lib: String,
	new_file_name: String,
	new_lib: String,
	circuit_libs_ordered: Vec<String>,
	move_error_opt: Option<String>
}

impl MoveCircuitPopup {
	pub fn new(new_file_name: String, new_lib: String) -> Self {
		Self {
			old_file_name: new_file_name.clone(),
			old_lib: new_lib.clone(),
			new_file_name,
			new_lib,
			circuit_libs_ordered: resource_interface::load_circuit_libraries().unwrap().1,
			move_error_opt: None
		}
	}
	/// Shows UI, meant to be in a popup
	/// Returns: (Whether to close the popup, Whether to save the circuit, Optional<(New lib, New name)>)
	pub fn show(&mut self, ui: &mut Ui, saved: bool) -> (bool, bool, Option<(String, String)>) {
		let mut out = (false, false, None);
		let mut transient_error_opt: Option<String> = None;
		ui.label("Move circuit");
		ui.separator();
		ui.label("WARNING: This feature will edit any circuits in which this circuit is used as a subcircuit, so if there are any tabs open for those circuits then saving them afterwards will corrupt them.");
		if !saved {
			transient_error_opt = Some("The circuit must be saved first".to_owned());
		}
		ui.horizontal(|ui| {
			ui.label("Save file name: ");
			ui.text_edit_singleline(&mut self.new_file_name);
			ui.label(".json");
		});
		// Check
		for char in self.new_file_name.chars() {
			if !REASONABLE_FILENAME_CHARS.contains(char) {
				transient_error_opt = Some(format!("File name contains disallowed char \"{}\"", char));
			}
		}
		ui.horizontal(|ui| {
			ui.label("Circuit library: ");
			egui::ComboBox::new("New circuit library select", "")
				.selected_text(&self.new_lib)
				.show_ui(ui, |ui| {
					for lib_option in &self.circuit_libs_ordered {
						ui.selectable_value(&mut self.new_lib, lib_option.clone(), lib_option);
					}
				});
		});
		if let Some(error) = &transient_error_opt {
			ui.colored_label(u8_3_to_color32([255, 0, 0]), error);
		}
		ui.horizontal(|ui| {
			if ui.add_enabled(transient_error_opt.is_none(), Button::new("Move")).clicked() {
				let move_res = resource_interface::move_circuit(&self.old_lib, &self.old_file_name, &self.new_lib, &self.new_file_name);
				match move_res {
					Ok(()) => {
						out.0 = true;
						out.2 = Some((self.new_lib.clone(), self.new_file_name.clone()));
					},
					Err(e) => {
						self.move_error_opt = Some(e);
					}
				}
			}
			if ui.add_enabled(!saved, Button::new("Save")).clicked() {
				out.1 = true;
			}
			if ui.button("Cancel").clicked() {
				out.0 = true;
			}
		});
		if let Some(error) = &self.move_error_opt {
			ui.colored_label(u8_3_to_color32([255, 0, 0]), error);
		}
		out
	}
}



#[derive(Debug, Default, Clone)]
pub enum SelectionState {
	/// Just there
	#[default]
	Fixed,
	/// Being dragged by mouse, keeps track of where it started (wrt grid). If there aren't any selected items, then use this to drag a rectangle to select stuff
	/// (
	/// 	Start,
	/// 	Delta,
	/// 	Vector of wires being dragged: (
	/// 		Wire ID,
	/// 		Whether start is selected,
	/// 		Initial position of either start or end based on field 1
	/// 	)
	/// )
	Dragging(V2, V2),
	/// After Paste operation, the pasted stuff will remain selected and following the mouse until a left click
	/// (Current or most recent mouse pos)
	FollowingMouse(V2)
}

#[derive(Debug, Clone)]
pub enum Tool {
	Select {
		selected_graphics: HashSet<GraphicSelectableItemRef>,
		selected_graphics_state: SelectionState
	},
	HighlightNet,
	/// Wire initially horizontal/vertical rules:
	/// When the firt perpindicular pair is placed it defauts to Horizontal (horiz), meaning the first segment is horizontal and then a vertical one from the end of that to the mouse
	/// Whenever the most recent pair is completely horiz or vert, its initial direction is set to that direction
	PlaceWire {
		/// Each of these represents two perpindicular segments (if the difference between this one's position and the next one's position is perfectly horizontal or vertical then one of the straight segments will just be length zero)
		/// [(Starting pos, Initial direction to get to next position)]
		/// When the actual wires are created, consecutive segments in the same direction will be combined
		perp_pairs: Vec<(IntV2, FourWayDir)>
	}
}

impl Tool {
	/// The tool buttons side bar cannot always be enabled, for example when the component placement ui is active
	pub fn tool_select_allowed(&self) -> bool {
		match &self {
			Self::Select{selected_graphics: _, selected_graphics_state} => match selected_graphics_state {
				SelectionState::Fixed => true,
				SelectionState::Dragging(_, _) => false,
				SelectionState::FollowingMouse(_) => false
			},
			Self::HighlightNet => false,
			Self::PlaceWire{perp_pairs: _} => false,
		}
	}
	pub fn tool_select_ui(&self, _draw: &dyn DrawInterface) {
		// TODO
	}
}

impl Default for Tool {
	fn default() -> Self {
		Self::Select{selected_graphics: HashSet::new(), selected_graphics_state: SelectionState::default()}
	}
}

pub struct LogicCircuitToplevelView {
	/// The top-level circuit of this view, its pins are rendered as interactive I/O pins
	pub circuit: LogicCircuit,
	/// Location of center of screen with respect to the grid, it is this way so that it will not have to adjusted when the grid is zoomed in/out
	screen_center_wrt_grid: V2,
	/// Pixels per grid increment
	grid_size: f32,
	logic_loop_error: bool,
	showing_component_popup: bool,
	showing_block_edit_popup: bool,
	component_search_text: String,
	all_logic_devices_search: Vec<EnumAllLogicDevices>,
	pub saved: bool,
	recompute_conns_next_frame: bool,
	/// 1 Less than actual number of times the compute function was called because the last call doesn't change anything and ends the loop
	frame_compute_cycles: usize,
	showing_flatten_opoup: bool,
	flatten_error: Option<String>,
	/// Not owned by the circuit itself because its only used for the toplevel circuit and would use more memory if subcircuits had it
	timing: TimingDiagram,
	move_popup_opt: Option<MoveCircuitPopup>,
	tool: Tool
}

impl LogicCircuitToplevelView {
	pub fn new(circuit: LogicCircuit, saved: bool, styles: &Styles) -> Self {
		let timing_diagram_order = circuit.build_timing_diagram_tree();
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
			recompute_conns_next_frame: true,
			frame_compute_cycles: 0,
			showing_flatten_opoup: false,
			flatten_error: None,
			timing: TimingDiagram::new(timing_diagram_order),
			move_popup_opt: None,
			tool: Tool::default()
		};
		out.circuit.update_probe_net_connections_and_timing(&mut out.timing.tree);
		// Done
		out
	}
	/// Returns: (Optional position to set the mouse to, Optional new circuit tab to open, Whether to reload tab)
	pub fn draw(&mut self, ui: &mut Ui, styles: Rc<Styles>, #[allow(unused)]screen_top_left: Pos2) -> (Option<Pos2>, Option<(String, String)>, Response, bool) {
		#[allow(unused_mut)]
		let mut return_new_mouse_pos = Option::<Pos2>::None;
		let mut return_new_circuit_tab = Option::<(String, String)>::None;
		let mut return_reload = false;
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
			if scroll != 0.0 && response.contains_pointer() {
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
					},
					&mut self.tool
				);
			}
			// Reconnect wires and thingies if anything was changed
			if self.recompute_conns_next_frame {
				self.saved = false;
				self.circuit.check_wire_geometry_and_connections(Some(&mut self.timing.tree));
			}
			// Update
			self.logic_loop_error = self.propagate_until_stable(CIRCUIT_MAX_COMPUTE_CYCLES, self.recompute_conns_next_frame);
			self.recompute_conns_next_frame = false;
			// graphics help from https://github.com/emilk/egui/blob/main/crates/egui_demo_lib/src/demo/painting.rs
			// Right side toolbar
			self.tool.tool_select_ui(&draw_info);
			// Draw circuit
			self.circuit.draw_toplevel(&(Box::new(draw_info) as Box<dyn DrawInterface>), &mut self.tool);
			(canvas_size, rect_center)
		});
		// Top: general controls
		Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::LEFT_TOP, child: Align2::LEFT_TOP}).id("top-left controls".into()).show(|ui| {
			if ui.button("Save").clicked() {
				self.circuit.save_circuit_toplevel(Some(&self.timing)).unwrap();
				self.saved = true;
			}
			if self.tool.tool_select_allowed() {
				if ui.button("+ Component / Subcircuit").clicked() {
					// Update component search list
					self.all_logic_devices_search = builtin_components::list_all_basic_components();
					for (lib_name, file_name) in resource_interface::list_all_circuit_files().unwrap() {
						self.all_logic_devices_search.push(EnumAllLogicDevices::SubCircuit(file_name, false, IntV2(0, 0), FourWayDir::default(), String::new(), lib_name, true));
					}
					self.showing_component_popup = true;
				}
				if ui.button("+ I/O Pin").clicked() {
					let graphic_pin_id = self.circuit.insert_graphic_pin(IntV2(0, 0), FourWayDir::default(), String::new(), true, 1);
					self.circuit.set_graphic_item_following_mouse(GraphicSelectableItemRef::Pin(graphic_pin_id), &mut self.tool);
					self.circuit.update_pin_block_positions();
				}
				if ui.button("+ Splitter").clicked() {
					self.circuit.set_graphic_item_following_mouse(self.circuit.insert_splitter(Splitter::new().save()), &mut self.tool);
				}
				if ui.button("+ Label").clicked() {
					self.circuit.set_graphic_item_following_mouse(self.circuit.insert_label(GraphicLabel::new().save()), &mut self.tool);
				}
				if ui.button("+ Probe").clicked() {
					self.circuit.set_graphic_item_following_mouse(self.circuit.insert_probe(Probe::default().save()), &mut self.tool);
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
					ui.label("Name:");
					ui.add(TextEdit::singleline(&mut self.circuit.type_name).desired_width(100.0));
				});
				ui.horizontal(|ui| {
					ui.label("File name:");
					ui.code(format!("{}.json", &self.circuit.save_name))
				});
				ui.label(format!("Library: {}", &self.circuit.lib_name));
				if ui.button("Change library or filename...").clicked() {
					self.move_popup_opt = Some(MoveCircuitPopup::new(self.circuit.save_name.clone(), self.circuit.lib_name.clone()));
				}
				ui.checkbox(&mut self.circuit.save_instance_config, "Save state (increases file size)");
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
			if let Tool::Select{selected_graphics, selected_graphics_state: _} = &self.tool {
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
								let handle = self.circuit.insert_component(device_save, None);
								self.tool = Tool::Select{
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
				ui.label(format!("This feature will create a copy of this circuit but with all the wires and components of sub-circuits extracted into the \"top layer\". Sub-circuits with fixed compute cycles will not be extracted but flattened versions of them will still be created. The intended use-case of this is to make a circuit faster to simulate. The flattened copy file size will be approximately the sum of all sub-circuits plus the toplevel wires and components. Running this function might create large files."));
				ui.horizontal(|ui| {
					if ui.button("Flatten Circuit").clicked() {
						match self.circuit.flatten(true) {
							Ok(device_save) => if let EnumAllLogicDevices::SubCircuit(save_path, _, _, _, _, lib_name, _) = device_save {
								return_new_circuit_tab = Some((save_path, lib_name));
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
		else if self.move_popup_opt.is_some() {
			Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
				let (close, save, reload_opt) = self.move_popup_opt.as_mut().unwrap().show(ui, self.saved);
				if close {
					self.move_popup_opt = None;
				}
				if save {
					self.circuit.save_circuit_toplevel(Some(&self.timing)).unwrap();
					self.saved = true;
				}
				if let Some(reload_data) = reload_opt {
					return_reload = true;
					// Set these so when this tab is reloaded it uses the correct info to find the circuit at the new location
					self.circuit.lib_name = reload_data.0;
					self.circuit.save_name = reload_data.1;
				}
			});
		}
		else {
			/*Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
				ScrollArea::both().show(ui, self.)
			});*/
			Window::new("Timing Diagram").anchor(Align2::RIGHT_TOP, Vec2::new(0.0, inner_response.response.rect.top())).collapsible(true).resizable(true).show(ui.ctx(), |ui| self.circuit.show_timing_diagram_ui(ui, styles, &mut self.timing));
		}
		(return_new_mouse_pos, return_new_circuit_tab, inner_response.response, return_reload)
	}
	/// Runs `compute_step()` repeatedly on the circuit until there are no changes, there must be a limit because there are circuits (ex. NOT gate connected to itself) where this would otherwise never end
	pub fn propagate_until_stable(&mut self, propagation_limit: usize, update_all: bool) -> bool {
		let start_t = Instant::now();
		loop {
			let mut count: usize = 0;
			let mut comp_update_tree = Vec::<ComponentUpdateTreeNode>::new();
			while count < propagation_limit {
				if !self.circuit.compute_toplevel(count == 0, &mut comp_update_tree, update_all) {
					/*if count > 0 {
						self.circuit.update_timing_diagram(&mut self.circuit.propagation_done.borrow_mut(), &mut self.timing, count == 0);
					}*/
					self.circuit.update_timing_diagram_toplevel(&mut self.circuit.propagation_done.borrow_mut(), &mut self.timing, count == 0);
					self.frame_compute_cycles = count;
					return false;
				}
				self.circuit.update_timing_diagram_toplevel(&mut self.circuit.propagation_done.borrow_mut(), &mut self.timing, count == 0);
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