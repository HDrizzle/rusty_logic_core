//! Timing diagram graphics & logic

use std::{cmp::{Ordering, PartialOrd}, default::Default, fmt::Debug, rc::Rc};
use eframe::{egui::{Align2, FontFamily, FontId, Painter, Pos2, Stroke}};
use crate::prelude::*;
use web_time::Instant;

/// A "Signal Group" is one graphical section of the timing diagram, can be one logical wire or a bus
#[derive(Debug, Clone)]
pub enum TimingDiagramSignalGroupSource {
	Clk,
	/// Probe ID
	Probe(u64)
}

#[derive(Debug, Clone)]
pub enum TimingDiagramTreeNode {
	/// (Component ID of sub circuit, It's list of tree elements)
	/// The toplevel timing diagram will collect all timing data, the sub circuit won't do anything
	Branch(u64, Vec<TimingDiagramTreeNode>),
	/// Actual signal group
	Leaf(TimingDiagramSignalGroupSource, Vec<Vec<(TimingDiagramTimestamp, LogicState)>>)
}

#[derive(Debug, Clone)]
pub struct TimingDiagram {
	/// List of signal groups and corresponding probe IDs (CLK probe ID is ignored and set to 0), each signal group contains list of signals (one signal per bit width of probe), each signal contains list of samples
	pub tree: Vec<TimingDiagramTreeNode>,
	pub n_samples: usize,
	pub running: TimingTiagramRunningState,
	pub current_timestamp: TimingDiagramTimestamp,
	pub real_start_time: Instant,
	/// Cumulative Number of steps BEFORE each propagation event (clk edges, etc)
	/// Should always have at least one element, first one is zero
	pub propagation_steps: Vec<u64>,
	pub show_sim_steps: bool,
	pub current_event_started_by_clock: bool
}

impl TimingDiagram {
	pub fn new(tree: Vec<TimingDiagramTreeNode>) -> Self {
		Self {
			tree,
			n_samples: 0,
			running: TimingTiagramRunningState::Off,
			current_timestamp: TimingDiagramTimestamp::default(),
			real_start_time: Instant::now(),
			propagation_steps: vec![0],
			show_sim_steps: false,
			current_event_started_by_clock: false
		}
	}
	pub fn update_timestamp(&mut self, first_propagation_step: bool) {
		match &mut self.current_timestamp {
			TimingDiagramTimestamp::Real(ts) => {
				*ts = Instant::now();// TS is PMO
			},
			TimingDiagramTimestamp::PropagationAndSimStep(event_count, sim_step) => {
				if first_propagation_step {
					self.propagation_steps.push((*sim_step as u64) + self.propagation_steps.last().unwrap());
					*event_count += 1;
					*sim_step = 0;
				}
				else {
					*sim_step += 1;
				}
			}
		}
	}
	pub fn timestamp_zero(&self) -> TimingDiagramTimestamp {
		match &self.current_timestamp {
			&TimingDiagramTimestamp::Real(_) => TimingDiagramTimestamp::Real(self.real_start_time),
			&TimingDiagramTimestamp::PropagationAndSimStep(_, _) => TimingDiagramTimestamp::PropagationAndSimStep(0, 0)
		}
	}
	// Not an instance method so that caller can recurse into the tree. Recursive `&mut self` functions will have borrowing issues
	pub fn push_state_if_different(current_timestamp: &TimingDiagramTimestamp, signal_group: &mut Vec<Vec<(TimingDiagramTimestamp, LogicState)>>, bit_i: usize, state: LogicState) {
		let bit_line = &mut signal_group[bit_i];
		if bit_line.len() == 0 {// If first sample put there anyway
			bit_line.push((*current_timestamp, state));
		}// Otherwise only if different from previous sample
		else if bit_line[bit_line.len() - 1].1 != state {
			bit_line.push((*current_timestamp, state));
		}
	}
	pub fn clear(&mut self) {
		// Clear recording in tree, using DFS
		let mut tree_stack = Vec::<&mut Vec<TimingDiagramTreeNode>>::new();
		tree_stack.push(&mut self.tree);
		while !tree_stack.is_empty() {
			let nodes: &mut Vec<TimingDiagramTreeNode> = tree_stack.pop().expect("Shouldn't be empty");
			for node in nodes {
				match node {
					TimingDiagramTreeNode::Leaf(_, signal_group) => {
						for bit_line in signal_group.iter_mut() {
							bit_line.clear();
						}
					},
					TimingDiagramTreeNode::Branch(_, branch_nodes) => {tree_stack.push(branch_nodes);}
				}
			}
		}
		// Counts & timing
		self.n_samples = 0;
		self.real_start_time = Instant::now();
		match &mut self.current_timestamp {
			TimingDiagramTimestamp::Real(ts) => {
				*ts = Instant::now();// TS is PMO
			},
			TimingDiagramTimestamp::PropagationAndSimStep(event_count, sim_step) => {
				*event_count = 0;
				*sim_step = 0;
			}
		}
		self.propagation_steps.clear();
		// Number of simulation steps BEFORE current event step
		self.propagation_steps.push(0);
	}
	pub fn convert_timestamp_to_x_value(&self, styles: &Styles, other_timestamp: TimingDiagramTimestamp) -> f32 {
		const ERROR: &str = "convert_timestamp_to_x_value called on wrong variant of `TimingDiagramTimestamp`";
		match self.current_timestamp {
			TimingDiagramTimestamp::Real(_) => {
				if let TimingDiagramTimestamp::Real(other_instant) = other_timestamp {
					styles.timing_diagram_real_time_resolution_px * (other_instant - self.real_start_time).as_secs_f32()
				}
				else {
					panic!("{}", ERROR);
				}
			},
			TimingDiagramTimestamp::PropagationAndSimStep(_, _) => {
				if let TimingDiagramTimestamp::PropagationAndSimStep(other_prop, other_sim_step) = other_timestamp {
					let mut out = (other_prop as f32 - 1.0) * styles.timing_diagram_event_resolution_px;
					if self.show_sim_steps {
						let event_index: usize = if other_prop as usize >= self.propagation_steps.len() {
							self.propagation_steps.len() - 1
						}
						else {
							other_prop as usize
						};
						out += (self.propagation_steps[event_index] + (other_sim_step as u64)) as f32 * styles.timing_diagram_prop_step_resolution_px;
					}
					out
				}
				else {
					panic!("{}", ERROR);
				}
			}
		}
	}
	pub fn set_running_state(&mut self, new_state: TimingTiagramRunningState) {
		if new_state == TimingTiagramRunningState::Off {
			self.running = new_state;
			return;
		}
		let current_ts_real = self.current_timestamp.is_real_time();
		if current_ts_real && new_state.uses_incremental_time() {
			self.current_timestamp = TimingDiagramTimestamp::PropagationAndSimStep(0, 0);
			self.running = new_state;
			self.clear();
		}
		if (!current_ts_real) && new_state.uses_real_time() {
			self.current_timestamp = TimingDiagramTimestamp::Real(Instant::now());
			self.running = new_state;
			self.clear();
		}
		self.running = new_state;
	}
	pub fn timing_diagram_end(&self) -> TimingDiagramTimestamp {
		match self.current_timestamp {
			TimingDiagramTimestamp::Real(_) => self.current_timestamp,
			TimingDiagramTimestamp::PropagationAndSimStep(event_count, prop_count) => TimingDiagramTimestamp::PropagationAndSimStep(event_count + 1, prop_count)
		}
	}
	/// Very similar to the timestamps own comparison imlementation, EXCEPT when not showing substeps, substeps are ignored and discrete timestamps with the same event count are considered equal
	pub fn compare_timestamps_for_display(&self, ts0: TimingDiagramTimestamp, other: TimingDiagramTimestamp) -> Ordering {
		match ts0 {
			TimingDiagramTimestamp::Real(this_instant) => {
				if let TimingDiagramTimestamp::Real(other_instant) = &other {
					this_instant.partial_cmp(other_instant).unwrap()
				}
				else {
					panic!("Comparing different variants of `TimingDiagramTimestamp`");
				}
			},
			TimingDiagramTimestamp::PropagationAndSimStep(this_prop, this_sim_step) => {
				if let TimingDiagramTimestamp::PropagationAndSimStep(other_prop, other_sim_step) = other {
					if this_prop < other_prop {
						Ordering::Less
					}
					else if this_prop > other_prop {
						Ordering::Greater
					}
					else if self.show_sim_steps {// ==
						this_sim_step.partial_cmp(&other_sim_step).unwrap()
					} else {
						Ordering::Equal
					}
				}
				else {
					panic!("Comparing different variants of `TimingDiagramTimestamp`");
				}
			}
		}
	}
	/// Corresponds to a single trace on the timing diagram (single bit or bus state)
	pub fn show_signal_group(
		&self,
		signal_group: &Vec<Vec<(TimingDiagramTimestamp, LogicState)>>,
		graph_pos_to_canvas_pos: impl Fn(f32, f32, usize) -> Pos2,
		styles: &Rc<Styles>,
		amplitude: f32,
		painter: &Painter,
		group_i: usize
	) {
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
				let x: f32 = self.convert_timestamp_to_x_value(&*styles, *timestamp);
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
				if *timestamp < self.current_timestamp && i + 1 == bit_line.len() {
					let last_x: f32 = self.convert_timestamp_to_x_value(&*styles, self.timing_diagram_end());
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
				let mut new_timestamp = self.current_timestamp;
				// Fixed by Gemini
				let next_timestamps: Vec<TimingDiagramTimestamp> = signal_group.iter().enumerate().map(
					|(bit_line_i, bit_line)| {
						let current_idx = bit_indices[bit_line_i];
						if bit_line.is_empty() || current_idx + 1 >= bit_line.len() {
							// If empty or at the last recorded sample, use the diagram's end time
							self.timing_diagram_end() 
						}
						else {
							// Use the timestamp of the NEXT sample
							bit_line[current_idx + 1].0
						}
					}
				).collect();
				// Get time of next bit change, closest next change
				for timestamp in &next_timestamps {
					if self.compare_timestamps_for_display(*timestamp, new_timestamp).is_le() {
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
				let x: f32 = self.convert_timestamp_to_x_value(&*styles, new_timestamp);
				assert!(current_sample.len() > 0, "Signal group must have at least one bit");
				// Compile binary number, quit if any states are floating or contested
				let (curr_n, valid, contested): ((u128, u128), bool, bool) = get_n_and_whether_valid_from_sample(&current_sample);
				draw_bus_segment(curr_n, valid, contested, x, prev_x, false);
				if end {
					let last_x = self.convert_timestamp_to_x_value(&*styles, self.timing_diagram_end());
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
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TimingDiagramTimestamp {
	/// Real time
	Real(Instant),
	/// (Event count, sim step)
	PropagationAndSimStep(u32, u32)
}

impl TimingDiagramTimestamp {
	fn is_real_time(&self) -> bool {
		match self {
			Self::Real(_) => true,
			Self::PropagationAndSimStep(_, _) => false
		}
	}
}

impl Default for TimingDiagramTimestamp {
	fn default() -> Self {
		Self::PropagationAndSimStep(0, 0)
	}
}

impl PartialOrd for TimingDiagramTimestamp {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		match self {
			Self::Real(this_instant) => {
				if let Self::Real(other_instant) = other {
					this_instant.partial_cmp(other_instant)
				}
				else {
					panic!("Comparing different variants of `TimingDiagramTimestamp`");
				}
			},
			Self::PropagationAndSimStep(this_prop, this_sim_step) => {
				if let Self::PropagationAndSimStep(other_prop, other_sim_step) = other {
					if this_prop < other_prop {
						Some(Ordering::Less)
					}
					else if this_prop > other_prop {
						Some(Ordering::Greater)
					}
					else {// ==
						this_sim_step.partial_cmp(other_sim_step)
					}
				}
				else {
					panic!("Comparing different variants of `TimingDiagramTimestamp`");
				}
			}
		}
	}
}

/// Manual control or whenever the clock is running
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub enum TimingTiagramRunningState {
	/// Never updates
	Off,
	/// Only on clock edges
	#[default]
	Clk,
	/// Whenever anything changes
	AnyChange,
	/// Always, uses TimingDiagramTimestamp::Real
	RealTime
}

impl TimingTiagramRunningState {
	pub fn to_str(&self) -> &'static str {
		match &self {
			Self::Off => "Off",
			Self::Clk => "CLK Only",
			Self::AnyChange => "Any Change",
			Self::RealTime => "Real Time"
		}
	}
	/// What variant of `TimingDiagramTimestamp` this uses
	pub fn uses_real_time(&self) -> bool {
		match &self {
			Self::Off => false,
			Self::Clk => false,
			Self::AnyChange => false,
			Self::RealTime => true
		}
	}
	/// What variant of `TimingDiagramTimestamp` this uses
	pub fn uses_incremental_time(&self) -> bool {
		match &self {
			Self::Off => false,
			Self::Clk => true,
			Self::AnyChange => true,
			Self::RealTime => false
		}
	}
}