//! Heavily based off of the logic simulation I wrote in TS for use w/ MotionCanvas, found at https://github.com/HDrizzle/stack_machine/blob/main/presentation/src/logic_sim.tsx

use std::{cell::RefCell, rc::Rc, collections::{HashMap, HashSet, VecDeque}, default::Default, fmt::Debug, fs, ops::{Deref, DerefMut}, time::{Duration, Instant}};
use serde::{Deserialize, Serialize};
use crate::{prelude::*, resource_interface};
use resource_interface::LogicCircuitSave;
use eframe::egui::{self, response::Response, Key, KeyboardShortcut, Modifiers, PointerButton};
use arboard;

fn logic_device_to_graphic_item(x: &dyn LogicDevice) -> &dyn GraphicSelectableItem {
	x
}

fn logic_device_to_graphic_item_mut(x: &mut dyn LogicDevice) -> &mut dyn GraphicSelectableItem {
	x
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum LogicState {
	Driven(bool),
	Floating,
	Contested
}

impl LogicState {
	pub fn value(&self) -> Option<bool> {
		match self {
			Self::Driven(state) => Some(*state),
			Self::Floating => None,
			Self::Contested => None
		}
	}
	pub fn is_valid(&self) -> bool {
		if let Self::Driven(_test) = self {
			true
		}
		else {
			false
		}
	}
	pub fn is_contested(&self) -> bool {
		if let Self::Contested = self {
			true
		}
		else {
			false
		}
	}
	pub fn is_floating(&self) -> bool {
		if let Self::Floating = self {
			true
		}
		else {
			false
		}
	}
	/// WARNING! Not neccessarily the same as real-world, this method is only here because logic gates will have to work w/ something, even if their inputs are floating or contested
	pub fn to_bool(&self) -> bool {
		match &self {
			Self::Driven(b) => *b,
			Self::Floating => false,
			Self::Contested => false
		}
	}
	pub fn to_bool_opt(&self) -> Option<bool> {
		match &self {
			Self::Driven(b) => Some(*b),
			Self::Floating => None,
			Self::Contested => None
		}
	}
}

impl Default for LogicState {
	fn default() -> Self {
		Self::Floating
	}
}

impl From<bool> for LogicState {
	fn from(value: bool) -> Self {
		Self::Driven(value)
	}
}

impl From<Option<bool>> for LogicState {
	fn from(value_opt: Option<bool>) -> Self {
		match value_opt {
			Some(value) => Self::Driven(value),
			None => Self::Floating
		}
	}
}

/// If two wires are connected, what will their combined state be?
pub fn merge_logic_states(a: LogicState, b: LogicState) -> LogicState {
	if a.is_valid() || b.is_valid() {// Both driven normally
		if a.is_valid() && b.is_valid() {
			if a.value().expect("This shouldn't happen") == b.value().expect("This shouldn't happen") {
				LogicState::Driven(a.value().expect("This shouldn't happen"))
			}
			else {
				LogicState::Contested
			}
		}
		else {// One of them is driven normally, the other is either contested or floating
			let (valid, invalid): (LogicState, LogicState) = if a.is_valid() {
				(a, b)
			}
			else {
				(b, a)
			};
			if invalid.is_contested() {
				LogicState::Contested
			}
			else {// Other one is floating
				valid
			}
		}
	}
	else {
		if a.is_contested() || b.is_contested() {
			LogicState::Contested
		}
		else {// Both floating
			LogicState::Floating
		}
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubCircuitPath(Vec<String>);

impl SubCircuitPath {
	pub fn to_string(&self) -> String {
		self.0.join("/") + "/"
	}
}

// Not just something that is connected, but something that is setting the voltage either high or low
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum LogicDriveSource {
	/// Set by the UI or the Clock or whatever
	Global,
	/// Connection to something outside this circuit
	ExternalConnection(GenericQuery<LogicConnectionPin>),
	/// A set of things connected together
	/// This also depends on context, for example the nets one either side of a sub-circuit pin would be referenced within different "namespaces"
	Net(GenericQuery<LogicNet>),
	/// Output of a basic logic gate, the actual transistors are not gonna be simulated
	ComponentInternal(ComponentPinReference),
	/// The clock source of a given circuit/sub-circuit
	Clock(SubCircuitPath)
}

impl LogicDriveSource {
	/// Basic component or global interface, cannot resolve any deeper
	pub fn is_final_source(&self) -> bool {
		match &self {
			Self::Global => true,
			Self::ExternalConnection(_) => false,
			Self::Net(_) => false,
			Self::ComponentInternal(_) => true,
			Self::Clock(_) => true
		}
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GlobalSourceReference {
	/// Pin of top level circuit, which always takes its inputs from Global
	Global(String),
	/// Output from basic logic component
	/// 0. Vec of strings, each one a sub-circuit of the last
	/// 1. Component reference within the previously given circuit
	ComponentInternal(SubCircuitPath, ComponentPinReference),
	/// The clock source of a given circuit/sub-circuit
	Clock(SubCircuitPath)
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LogicNet {
	connections: Vec<CircuitWidePinReference>,
	pub sources: Vec<GlobalSourceReference>,
	pub state: LogicState
}

impl LogicNet {
	pub fn new(
		connections: Vec<CircuitWidePinReference>
	) -> Self {
		Self {
			connections,
			sources: Vec::new(),
			state: LogicState::Floating
		}
	}
	/// Goes "down the rabbit hole" and finds everything that this net is connected to, and only takes logic states from global inputs and basic component outputs
	/// Pretty sure it can't get stuck in a recursive loop
	fn resolve_sources(&self, self_ancestors: &AncestryStack, self_id: u64, caller_history: &Vec<(AncestryStack, u64)>) -> Vec<(GlobalSourceReference, LogicState)> {
		// Keep history of recursion to ignore nets that have already been reached
		for (caller_ancestry, caller_id) in caller_history {
			if caller_ancestry == self_ancestors && *caller_id == self_id {
				return Vec::new();
			}
		}
		let mut new_caller_history = caller_history.clone();
		new_caller_history.push((self_ancestors.clone(), self_id));
		let mut out = Vec::<(GlobalSourceReference, LogicState)>::new();
		for connection in &self.connections {
			match self_ancestors.parent() {
				Some(circuit) => match connection {
					CircuitWidePinReference::ComponentPin(component_pin_ref) => match circuit.components.borrow().get(&component_pin_ref.component_id) {
						Some(component_cell) => match component_cell.borrow().get_pins_cell().borrow().get(&component_pin_ref.pin_id) {
							Some(pin) => {
								// Check what the internal source is
								if let Some(source) = &pin.borrow().internal_source {
									match source {
										// Check if pin is internally driven (by a sub-circuit)
										LogicConnectionPinInternalSource::Net(child_circuit_net_id) => {
											let component = component_cell.borrow();
											let circuit = component.get_circuit();
											match circuit.nets.borrow().get(&child_circuit_net_id) {
												Some(child_net) => out.append(&mut child_net.borrow().resolve_sources(&self_ancestors.push(circuit), *child_circuit_net_id, &new_caller_history)),
												None => panic!("Internal connection in circuit \"{}\" references net {:?} inside sub-circuit \"{}\", the net does not exist", circuit.get_generic().unique_name, &child_circuit_net_id, component_cell.borrow().get_generic().unique_name)
											};
										},
										// Check if pin is driven by a regular component
										LogicConnectionPinInternalSource::ComponentInternal => {
											out.push((GlobalSourceReference::ComponentInternal(self_ancestors.to_sub_circuit_path(), component_pin_ref.clone()), pin.borrow().internal_state));
										}
									}
								}
							},
							None => panic!("Net references internal pin {} on component \"{}\" circuit \"{}\", which doesn't exist on that component", component_pin_ref.pin_id, component_cell.borrow().get_generic().unique_name, circuit.get_generic().unique_name)
						},
						None => panic!("Net references internal pin on component {} circuit \"{}\", which doesn't exist in the circuit", component_pin_ref.component_id, circuit.get_generic().unique_name)
					},
					CircuitWidePinReference::ExternalConnection(ext_conn_id) => match circuit.get_pins_cell().borrow().get(ext_conn_id) {
						Some(pin) => {
							// Check what the external source is
							if let Some(source) = &pin.borrow().external_source {
								match source {
									// Check if external pin is connected to net on other side
									LogicConnectionPinExternalSource::Net(parent_circuit_net_id) => {
										// External pin is connected to a net in a circuit that contains the circuit that this net is a part of
										// Check that this net's "grandparent" is a circuit and not toplevel
										match self_ancestors.grandparent() {
											Some(parent_circuit) => match parent_circuit.nets.borrow().get(parent_circuit_net_id) {
												Some(parent_circuit_net) => out.append(&mut parent_circuit_net.borrow().resolve_sources(&self_ancestors.trim(), *parent_circuit_net_id, &new_caller_history)),
												None => panic!("External connection is referencing a net ({}) which does not exist in this circuit's parent", parent_circuit_net_id)
											},
											None => panic!("External connection is connected to a net, but the parent of this circuit is toplevel, this shouldn't happen")
										}
									}
									// Check if it is connected to a global pin
									LogicConnectionPinExternalSource::Global => {
										out.push((GlobalSourceReference::Global(ext_conn_id.to_owned()), pin.borrow().external_state));
									}
								}
							}
						},
						None => panic!("Net references external connection {:?} which is invalid", connection)
					}
				},
				None => panic!("Net cannot have a toplevel wrapper as its parent")
			}
		}
		// Done
		out
	}
	pub fn update_state(&self, ancestors: &AncestryStack, self_id: u64) -> (LogicState, Vec<GlobalSourceReference>) {
		let sources_raw = self.resolve_sources(ancestors, self_id, &Vec::new());
		let mut sources = Vec::<GlobalSourceReference>::new();
		let mut new_state = LogicState::Floating;
		// Go through and remove any logic states that are floating
		for (source, state) in sources_raw {
			if !state.is_floating() {
				sources.push(source);
				new_state = merge_logic_states(new_state, state);
			}
		}
		// Done
		(new_state, sources)
	}
	/// Makes sure component connection is or isn't included in this net
	pub fn edit_component_connection(&mut self, include: bool, comp_id: u64, pin_id: &str) {
		let mut index_to_remove_opt = Option::<usize>::None;
		for (conn_i, conn) in self.connections.iter().enumerate() {
			match conn {
				CircuitWidePinReference::ExternalConnection(_) => {},
				CircuitWidePinReference::ComponentPin(test_comp_pin_ref) => {
					if test_comp_pin_ref.component_id == comp_id && test_comp_pin_ref.pin_id == pin_id {
						if include {
							return;// If the connection already exists, return
						}
						else {
							index_to_remove_opt = Some(conn_i);
						}
					}
				}
			}
		}
		if include {
			// Hasn't returned yet, add connection
			self.connections.push(CircuitWidePinReference::ComponentPin(ComponentPinReference::new(comp_id, pin_id.to_owned())));
		}
		else {
			// If don't include and found it, remove it
			if let Some(index_to_remove) = index_to_remove_opt {
				self.connections.remove(index_to_remove);
			}
		}
	}
	/// Makes sure external connection is or isn't included in this net
	pub fn edit_external_connection(&mut self, include: bool, pin_id: &str) {
		let mut index_to_remove_opt = Option::<usize>::None;
		for (conn_i, conn) in self.connections.iter().enumerate() {
			match conn {
				CircuitWidePinReference::ComponentPin(_) => {},
				CircuitWidePinReference::ExternalConnection(test_pin_ref) => {
					if test_pin_ref == pin_id {
						if include {
							return;// If the connection already exists, return
						}
						else {
							index_to_remove_opt = Some(conn_i);
						}
					}
				}
			}
		}
		if include {
			// Hasn't returned yet, add connection
			self.connections.push(CircuitWidePinReference::ExternalConnection(pin_id.to_owned()));
		}
		else {
			// If don't include and found it, remove it
			if let Some(index_to_remove) = index_to_remove_opt {
				self.connections.remove(index_to_remove);
			}
		}
	}
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
pub enum LogicConnectionPinExternalSource {
	/// Even if not connected, it will have its own net
	Net(u64),
	#[default]
	Global
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
pub enum LogicConnectionPinInternalSource {
	/// Even if not connected, it will have its own net
	Net(u64),
	#[default]
	ComponentInternal
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LogicConnectionPin {
	pub internal_source: Option<LogicConnectionPinInternalSource>,
	internal_state: LogicState,
	pub external_source: Option<LogicConnectionPinExternalSource>,
	pub external_state: LogicState,
	/// Usually 1, may be something else if theres a curve on an OR input or something
	pub length: f32,
	pub ui_data: UIData,
	pub bit_width: u32
}

impl LogicConnectionPin {
	pub fn new(
		internal_source: Option<LogicConnectionPinInternalSource>,
		external_source: Option<LogicConnectionPinExternalSource>,
		relative_end_grid: IntV2,
		direction: FourWayDir,
		length: f32
	) -> Self {
		Self {
			internal_source,
			internal_state: LogicState::Floating,
			external_source,
			external_state: LogicState::Floating,
			length,
			ui_data: UIData::from_pos_dir(relative_end_grid, direction),
			bit_width: 1
		}
	}
	pub fn set_drive_internal(&mut self, state: LogicState) {
		self.internal_state = state;
	}
	pub fn set_drive_external(&mut self, state: LogicState) {
		self.external_state = state;
	}
	pub fn state(&self) -> LogicState {
		merge_logic_states(self.internal_state, self.external_state)
	}
}

/// ONLY meant to be used on external pins on the toplevel circuit, all other pins are just rendered as part of the component
impl GraphicSelectableItem for LogicConnectionPin {
	fn draw<'a>(&self, draw_parent: &ComponentDrawInfo<'a>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.ui_data.position, self.ui_data.direction);
		draw.draw_polyline(
			vec![
				V2::new(-0.9, -0.9),
				V2::new(-0.9, 0.9),
				V2::new(0.9, 0.9),
				V2::new(0.9, -0.9),
				V2::new(-0.9, -0.9)
			].iter().map(|p| p + (self.ui_data.direction.to_unit() * 2.0)).collect(),
			draw.styles.color_from_logic_state(self.state())
		);
		draw.draw_polyline(
			vec![
				V2::zeros(),
				self.ui_data.direction.to_unit()
			],
			draw.styles.color_from_logic_state(self.state())
		);
	}
	fn get_ui_data(&self) -> &UIData {
		&self.ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.ui_data
	}
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let half_diagonal = V2::new(1.0, 1.0);
		let box_center = (self.ui_data.direction.to_unit() * 2.0) + self.ui_data.position.to_v2();
		let global_offset = grid_offset + box_center;
		(global_offset - half_diagonal, global_offset + half_diagonal)
	}
	fn is_connected_to_net(&self, net_id: u64) -> bool {
		match &self.internal_source {
			Some(source) => match source {
				LogicConnectionPinInternalSource::ComponentInternal => false,
				LogicConnectionPinInternalSource::Net(test_net_id) => *test_net_id == net_id
			},
			None => false
		}
	}
	fn get_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::BitWidth(self.bit_width),
			SelectProperty::PositionX(self.ui_data.position.0),
			SelectProperty::PositionY(self.ui_data.position.1),
			SelectProperty::GlobalConnectionState(self.state().to_bool_opt()),
			SelectProperty::Direction(self.ui_data.direction)
		]
	}
	fn set_property(&mut self, property: SelectProperty) {
		match property {
			SelectProperty::BitWidth(bit_width) => {
				self.bit_width = bit_width;
			},
			SelectProperty::PositionX(x) => {
				self.ui_data.position.0 = x;
			},
			SelectProperty::PositionY(y) => {
				self.ui_data.position.1 = y;
			},
			SelectProperty::GlobalConnectionState(driven) => {
				self.external_source = Some(LogicConnectionPinExternalSource::Global);
				self.external_state = driven.into();
			},
			SelectProperty::Direction(direction) => {
				self.ui_data.direction = direction;
			},
			_ => {}
		}
	}
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::ExternalConnection(self.clone())
	}
	fn accept_click(&mut self) -> bool {
		match self.external_source.clone().expect("Pin being used as a graphic item must have an external source") {
			LogicConnectionPinExternalSource::Global => {
				if self.external_state.is_valid() {
					self.external_state = (!self.external_state.to_bool()).into();
					true
				}
				else {
					false
				}
			},
			LogicConnectionPinExternalSource::Net(_) => panic!("Pin being used as a graphic item cannot hav eexternal net source")
		}
	}
}

/// Everything within a circuit
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CircuitWidePinReference {
	ComponentPin(ComponentPinReference),
	ExternalConnection(String)
}

impl CircuitWidePinReference {
	pub fn is_external(&self) -> bool {
		if let Self::ExternalConnection(_) = self {
			true
		}
		else {
			false
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ComponentPinReference {
	/// Has to be a query for something else (()), not <Box<dyn LogicDevice>> so it will work with serde
	component_id: u64,
	pin_id: String
}

impl ComponentPinReference {
	pub fn new(component_id: u64, pin_id: String) -> Self {
		Self {
			component_id,
			pin_id
		}
	}
}

/// Just a straight segment, either horizontal or vertical
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Wire {
	ui_data: UIData,
	pub length: u32,
	pub direction: FourWayDir,
	pub net: u64,
	state: LogicState,
	pub start_connections: Rc<RefCell<HashSet<WireConnection>>>,
	pub end_connections: Rc<RefCell<HashSet<WireConnection>>>,
	start_selected: bool,
	end_selected: bool,
	position_before_dragging: IntV2,
	bit_width: u32
}

impl Wire {
	pub fn new(
		pos: IntV2,
		length: u32,
		direction: FourWayDir,
		bit_width: u32,
		net: u64,
		start_connections: Rc<RefCell<HashSet<WireConnection>>>,
		end_connections: Rc<RefCell<HashSet<WireConnection>>>
	) -> Self {
		Self {
			ui_data: UIData::from_pos_dir(pos, FourWayDir::default()),
			length,
			direction,
			net,
			state: LogicState::Floating,
			start_connections,
			end_connections,
			start_selected: false,
			end_selected: false,
			position_before_dragging: pos,
			bit_width
		}
	}
	/// Returns: (Start, Middle, End)
	pub fn contains_point(&self, point: IntV2) -> ((bool, bool, bool), Option<Rc<RefCell<HashSet<WireConnection>>>>) {
		// Start
		if point == self.ui_data.position {
			return ((true, false, false), Some(Rc::clone(&self.start_connections)));
		}
		// End
		if point == self.ui_data.position + self.direction.to_unit_int().mult(self.length as i32) {
			return ((false, false, true), Some(Rc::clone(&self.end_connections)));
		}
		// Middle
		let this_to_point_v = point - self.ui_data.position;
		if let Some(test_dir) = this_to_point_v.is_along_axis() {
			if test_dir == self.direction && this_to_point_v.to_v2().magnitude() < self.length as f32 {
				return ((false, true, false), None);
			}
		}
		// None
		((false, false, false), None)
	}
	pub fn perpindicular_pair_to_segments(perp_pair: &(IntV2, FourWayDir), end_pos: IntV2) -> Vec<(IntV2, FourWayDir, u32)> {
		// Check if along straight line
		let v = end_pos - perp_pair.0;
		if v.taxicab() == 0 {
			return vec![];
		}
		if let Some(_) = v.is_along_axis() {
			return vec![(
				perp_pair.0,
				perp_pair.1,
				v.taxicab()
			)];
		}
		let pair: [(IntV2, FourWayDir, u32); 2] = match perp_pair.1.is_horizontal() {
			true => if v.0 > 0 {
				let first: (IntV2, FourWayDir, u32) = (perp_pair.0, perp_pair.1, v.0 as u32);
				let second: (IntV2, FourWayDir, u32) = if v.1 > 0 {
					(perp_pair.0 + IntV2(v.0, 0), FourWayDir::N, v.1 as u32)
				}
				else {
					(perp_pair.0 + IntV2(v.0, 0), FourWayDir::S, -v.1 as u32)
				};
				[first, second]
			}
			else {
				let first: (IntV2, FourWayDir, u32) = (perp_pair.0, perp_pair.1, -v.0 as u32);
				let second: (IntV2, FourWayDir, u32) = if v.1 > 0 {
					(perp_pair.0 + IntV2(v.0, 0), FourWayDir::N, v.1 as u32)
				}
				else {
					(perp_pair.0 + IntV2(v.0, 0), FourWayDir::S, -v.1 as u32)
				};
				[first, second]
			},
			false => if v.1 > 0 {
				let first: (IntV2, FourWayDir, u32) = (perp_pair.0, perp_pair.1, v.1 as u32);
				let second: (IntV2, FourWayDir, u32) = if v.0 > 0 {
					(perp_pair.0 + IntV2(0, v.1), FourWayDir::E, v.0 as u32)
				}
				else {
					(perp_pair.0 + IntV2(0, v.1), FourWayDir::W, -v.0 as u32)
				};
				[first, second]
			}
			else {
				let first: (IntV2, FourWayDir, u32) = (perp_pair.0, perp_pair.1, -v.1 as u32);
				let second: (IntV2, FourWayDir, u32) = if v.0 > 0 {
					(perp_pair.0 + IntV2(0, v.1), FourWayDir::E, v.0 as u32)
				}
				else {
					(perp_pair.0 + IntV2(0, v.1), FourWayDir::W, -v.0 as u32)
				};
				[first, second]
			}
		};
		vec![pair[0], pair[1]]
	}
	pub fn end_pos(&self) -> IntV2 {
		self.ui_data.position + (self.direction.to_unit_int().mult(self.length as i32))
	}
}

impl GraphicSelectableItem for Wire {
	fn draw<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		let start_pos = self.ui_data.position.to_v2();
		let end_pos = self.end_pos().to_v2();
		let stroke = draw.styles.color_from_logic_state(self.state);
		draw.draw_polyline(
			vec![
				start_pos,
				end_pos
			],
			stroke
		);
		if self.start_connections.borrow().len() >= 3 {
			draw.draw_circle_filled(start_pos, draw.styles.connection_dot_grid_size, stroke);
		}
		if self.end_connections.borrow().len() >= 3 {
			draw.draw_circle_filled(end_pos, draw.styles.connection_dot_grid_size, stroke);
		}
	}
	fn get_ui_data(&self) -> &UIData {
		&self.ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.ui_data
	}
	/// Excludes end BBs which are special and for dragging the ends around or extruding at right angles
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let local_bb_unrectified: (V2, V2) = match self.direction {
			FourWayDir::E => (V2::new(0.25, -0.25), V2::new(self.length as f32 - 0.25, 0.25)),
			FourWayDir::N => (V2::new(-0.25, 0.25 - (self.length as f32)), V2::new(0.25, -0.25)),
			FourWayDir::W => (V2::new(0.25 - self.length as f32, -0.25), V2::new(-0.25, 0.25)),
			FourWayDir::S => (V2::new(-0.25, -0.25), V2::new(0.25, 0.25 - (self.length as f32)))
		};
		let local_bb: (V2, V2) = merge_points_to_bb(vec![local_bb_unrectified.0, local_bb_unrectified.1]);
		let offset = grid_offset + self.ui_data.position.to_v2();
		(local_bb.0 + offset, local_bb.1 + offset)
	}
	fn is_connected_to_net(&self, net_id: u64) -> bool {
		net_id == self.net
	}
	fn get_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::BitWidth(self.bit_width)
		]
	}
	fn set_property(&mut self, property: SelectProperty) {
		if let SelectProperty::BitWidth(bit_width) = property {
			self.bit_width = bit_width;
		}
		else {
			panic!("Wire doesn't use property {:?}", property)
		}
	}
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::Wire((self.ui_data.position, self.direction, self.length))
	}
}

/// What could the end of a be wire connected to?
/// Up to 3 of these
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum WireConnection {
	/// Component or external pin
	Pin(CircuitWidePinReference),
	/// Another straight wire segment
	Wire(u64)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogicDeviceGeneric {
	pub pins: RefCell<HashMap<String, RefCell<LogicConnectionPin>>>,
	pub ui_data: UIData,
	pub unique_name: String,
	pub sub_compute_cycles: usize,
	/// Only for graphical purposes
	pub bounding_box: (V2, V2)
}

impl LogicDeviceGeneric {
	pub fn new(
		pins: HashMap<String, LogicConnectionPin>,
		position_grid: IntV2,
		unique_name: String,
		sub_compute_cycles: usize,
		rotation: FourWayDir,
		bounding_box: (V2, V2)
	) -> Result<Self, String> {
		if sub_compute_cycles == 0 {
			return Err("Sub-compute cycles cannot be 0".to_string());
		}
		Ok(Self {
			pins: RefCell::new(hashmap_into_refcells(pins)),
			ui_data: UIData::from_pos_dir(position_grid, rotation),
			unique_name,
			sub_compute_cycles,
			bounding_box
		})
	}
}

/// Could be a simple gate, or something more complicated like an adder, or maybe even the whole computer
pub trait LogicDevice: Debug + GraphicSelectableItem where Self: 'static {
	fn get_generic(&self) -> &LogicDeviceGeneric;
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric;
	fn compute_step(&mut self, ancestors: &AncestryStack);
	fn save(&self) -> Result<EnumAllLogicDevices, String>;
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>);
	/// In CircuitVerse there can be for example one AND gate that acts like 8 gates, with 8-bit busses going in and out of it
	fn get_bit_width(&self) -> Option<u32> {Some(1)}
	#[allow(unused)]
	fn set_bit_width(&mut self, bit_width: u32) {}
	fn is_toplevel_circuit(&self) -> bool {false}
	fn is_circuit(&self) -> bool {false}
	/// Everything besides what `impl<T: LogicDevice> GraphicSelectableItem for T::get_properties()` generates
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {Vec::new()}
	/// Everything besides what `impl<T: LogicDevice> GraphicSelectableItem for T::set_property()` accepts
	fn device_set_special_select_property(&mut self, property: SelectProperty) {drop(property);}// So that unused variable warning doesn't happen
	fn compute(&mut self, ancestors: &AncestryStack) {
		for _ in 0..self.get_generic().sub_compute_cycles {
			self.compute_step(ancestors);
		}
	}
	fn get_circuit(&self) -> &LogicCircuit {
		panic!("LogicDevice::get_circuit only works on the LogicCircuit class which overrides it");
	}
	fn get_circuit_mut(&mut self) -> &mut LogicCircuit {
		panic!("LogicDevice::get_circuit_mut only works on the LogicCircuit class which overrides it");
	}
	fn set_pin_external_state(
		&mut self,
		pin_id: &str,
		state: LogicState
	) -> Result<(), String> {
		let generic = self.get_generic_mut();
		let pins = &generic.pins;
		let mut pins_borrow_mut = pins.borrow_mut();
		let pin: &mut LogicConnectionPin = pins_borrow_mut.get_mut(pin_id).expect(&format!("Pin ID {} does not work on logic device \"{}\"", pin_id, generic.unique_name)).get_mut();
		pin.set_drive_external(state);
		Ok(())
	}
	fn get_pins_cell(&self) -> &RefCell<HashMap<String, RefCell<LogicConnectionPin>>> {
		&self.get_generic().pins
	}
	/*fn query_pin_mut(&mut self, pin_id: &str) -> Option<&mut LogicConnectionPin> {
		let generic = self.get_generic_mut();
		match generic.pins.get_mut(pin_id) {
			Some(pin_cell) => ,
			None => None
		}
	}*/
	fn set_all_pin_states(&mut self, states: Vec<(&str, LogicState, LogicDriveSource)>) -> Result<(), String> {
		for (pin_query, state, _source) in states {
			self.set_pin_external_state(&pin_query, state)?;
		}
		Ok(())
	}
	fn get_pin_state_panic(&self, pin_query: &str) -> LogicState {
		self.get_pins_cell().borrow().get(pin_query).expect(&format!("Pin query {:?} for logic device \"{}\" not valid", &pin_query, &self.get_generic().unique_name)).borrow().state()
	}
	fn set_pin_internal_state_panic(&mut self, pin_query: &str, state: LogicState) {
		self.get_pins_cell().borrow_mut().get_mut(pin_query).expect(&format!("Pin query {:?} not valid", &pin_query)).borrow_mut().internal_state = state;
	}
	fn into_box(self: Box<Self>) -> Box<dyn LogicDevice> where Self: Sized {
		self as Box<dyn LogicDevice>
	}
}

/// Everything that implements `Component` also automatically works with the graphics
impl<T: LogicDevice> GraphicSelectableItem for T {
	fn draw<'a>(&self, draw_parent: &ComponentDrawInfo<'a>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.get_generic().ui_data.position, self.get_generic().ui_data.direction);
		if !self.is_toplevel_circuit() {
			for (_, pin_cell) in self.get_pins_cell().borrow().iter() {
				let pin = pin_cell.borrow();
				draw.draw_polyline(
					vec![
						pin.ui_data.position.to_v2(),
						pin.ui_data.position.to_v2() - (pin.ui_data.direction.to_unit() * pin.length)
					],
					draw.styles.color_from_logic_state(pin.state())
				);
			}
		}
		self.draw_except_pins(&draw);
	}
	fn get_ui_data(&self) -> &UIData {
		&self.get_generic().ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.get_generic_mut().ui_data
	}
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let local_bb: (V2, V2) = if self.is_circuit() {
			if self.get_circuit().displayed_as_block {
				self.get_generic().bounding_box
			}
			else {
				self.get_circuit().circuit_internals_bb
			}
		}
		else {
			self.get_generic().bounding_box
		};
		let offset = grid_offset + self.get_generic().ui_data.position.to_v2();
		(local_bb.0 + offset, local_bb.1 + offset)
	}
	fn is_connected_to_net(&self, _net_id: u64) -> bool {
		false// Don't highlight a whole component, only wires and pins
	}
	fn get_properties(&self) -> Vec<SelectProperty> {
		let mut out = vec![
			SelectProperty::PositionX(self.get_generic().ui_data.position.0),
			SelectProperty::PositionY(self.get_generic().ui_data.position.1),
			SelectProperty::Direction(self.get_generic().ui_data.direction)
		];
		if self.is_circuit() {
			out.push(SelectProperty::DisplayCircuitAsBlock(self.get_circuit().displayed_as_block));
		}
		if let Some(bit_width) = self.get_bit_width() {
			out.push(SelectProperty::BitWidth(bit_width));
		}
		out.append(&mut self.device_get_special_select_properties());
		out
	}
	fn set_property(&mut self, property: SelectProperty) {
		match property {
			SelectProperty::BitWidth(bit_width) => {
				self.set_bit_width(bit_width);
			},
			SelectProperty::PositionX(x) => {
				self.get_generic_mut().ui_data.position.0 = x;
			},
			SelectProperty::PositionY(y) => {
				self.get_generic_mut().ui_data.position.1 = y;
			},
			SelectProperty::Direction(dir) => {
				self.get_generic_mut().ui_data.direction = dir;
			},
			SelectProperty::GlobalConnectionState(_) => {},
			SelectProperty::DisplayCircuitAsBlock(block) => {
				if self.is_circuit() {
					self.get_circuit_mut().displayed_as_block = block;
				}
			},
			other => self.device_set_special_select_property(other),
		}
	}
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::Component(self.save().unwrap())
	}
}

#[derive(Clone)]
pub struct AncestryStack<'a>(Vec<&'a LogicCircuit>);

impl<'a> AncestryStack<'a> {
	pub fn new() -> Self {
		Self(Vec::new())
	}
	pub fn parent(&self) -> Option<&'a LogicCircuit> {
		if self.0.len() == 0 {
			None
		}
		else {
			Some(self.0.last().expect("Ancestor stack should not be empty"))
		}
	}
	pub fn grandparent(&self) -> Option<&'a LogicCircuit> {
		if self.0.len() < 2 {
			None
		}
		else {
			Some(&self.0[self.0.len() - 2])
		}
	}
	pub fn trim(&self) -> Self {
		if self.0.len() == 0 {
			panic!("Attempt to trim ancestry stack with no items");
		}
		let mut out = self.clone();
		out.0.pop();
		out
	}
	pub fn push(&self, new_node: &'a LogicCircuit) -> Self {
		let mut out = self.clone();
		out.0.push(new_node);
		out
	}
	/// IMPORTANT: The first entry here will be ignored when creating the path because it would otherwise be redundant
	pub fn to_sub_circuit_path(&self) -> SubCircuitPath {
		let mut out = Vec::<String>::new();
		for (i, circuit) in self.0.iter().enumerate() {
			if i ==  0 {
				continue;
			}
			out.push(circuit.get_generic().unique_name.clone());
		}
		SubCircuitPath(out)
	}
}

impl<'a> PartialEq for AncestryStack<'a> {
	fn eq(&self, other: &Self) -> bool {
		if self.0.len() != other.0.len() {
			return false;
		}
		for i in 0..self.0.len() {
			if self.0[i].get_generic().unique_name != other.0[i].get_generic().unique_name {
				return false;
			}
		}
		return true;
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GraphicSelectableItemRef {
	Component(u64),
	Wire(u64),
	Pin(String)
}

#[derive(Debug, Default, Clone)]
pub enum SelectionState {
	/// Just there
	#[default]
	Fixed,
	/// Being dragged by mouse, keeps track of where it started (wrt grid). If there aren't any selected items, then use this to drag a rectangle to select stuff
	/// (Start, Delta)
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
	HighlightNet(Option<u64>),
	/// Wire initially horizontal/vertical rules:
	/// When the firt perpindicular pair is placed it defauts to Horizontal (horiz), meaning the first segment is horizontal and then a vertical one from the end of that to the mouse
	/// Whenever the most recent pair is completely horiz or vert, its initial direction is set to that direction
	PlaceWire {
		/// Each of these represents two perpindicular segments (if the difference between this one's position and the next one's position is perfectly horizontal or vertical then one of the straight segments will just be length zero)
		/// [(Starting pos, Initial direction to get to next position)]
		/// When the actual wires are created, consecutive segments in the same direction will be combined
		perp_pairs: Vec<(IntV2, FourWayDir)>,
		/// If this wire was started somewhere with a connection (a "Wire termination point") include the net to make later net calculations simpler
		start_net_opt: Option<u64>,
		start_wire_connections_opt: Option<Rc<RefCell<HashSet<WireConnection>>>>
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
			Self::HighlightNet(_) => true,
			Self::PlaceWire{perp_pairs: _, start_net_opt: _, start_wire_connections_opt: _} => false,
		}
	}
	pub fn tool_select_ui(&self, draw: &ComponentDrawInfo) {
		// TODO
	}
}

impl Default for Tool {
	fn default() -> Self {
		Self::Select{selected_graphics: HashSet::new(), selected_graphics_state: SelectionState::default()}
	}
}

#[derive(Debug)]
pub struct LogicCircuit {
	pub generic_device: LogicDeviceGeneric,
	pub components: RefCell<HashMap<u64, RefCell<Box<dyn LogicDevice>>>>,
	pub nets: RefCell<HashMap<u64, RefCell<LogicNet>>>,
	pub wires: RefCell<HashMap<u64, RefCell<Wire>>>,
	pub save_path: String,
	/// Inspired by CircuitVerse, block-diagram version of circuit
	/// {pin ID: (relative position (ending), direction)}
	block_pin_positions: HashMap<String, (IntV2, FourWayDir)>,
	displayed_as_block: bool,
	/// For UI
	pub tool: RefCell<Tool>,
	is_toplevel: bool,
	/// Bounding box for the circuit, not the block diagram, relative to this circuit
	/// The block diagram BB can be found at `self.generic_device.bounding_box`
	pub circuit_internals_bb: (V2, V2)
}

impl LogicCircuit {
	pub fn new(
		components_not_celled: HashMap<u64, Box<dyn LogicDevice>>,
		external_connections: HashMap<String, LogicConnectionPin>,
		nets: HashMap<u64, LogicNet>,
		position_grid: IntV2,
		unique_name: String,
		sub_compute_cycles: usize,
		wires: HashMap<u64, Wire>,
		save_path: String,
		clock_enabled: bool,
		clock_state: bool,
		clock_freq: f32,
		displayed_as_block: bool,
		is_toplevel: bool
	) -> Result<Self, String> {
		let mut components = HashMap::<u64, RefCell<Box<dyn LogicDevice>>>::new();
		for (ref_, comp) in components_not_celled.into_iter() {
			components.insert(ref_, RefCell::new(comp));
		}
		let mut new = Self {
			generic_device: LogicDeviceGeneric::new(
				external_connections,
				position_grid,
				unique_name,
				sub_compute_cycles,
				FourWayDir::E,
				(V2::zeros(), V2::zeros())
			)?,
			components: RefCell::new(components),
			nets: RefCell::new(hashmap_into_refcells(nets)),
			wires: RefCell::new(hashmap_into_refcells(wires)),
			save_path,
			block_pin_positions: HashMap::new(),
			displayed_as_block,
			tool: RefCell::new(Tool::default()),
			is_toplevel,
			circuit_internals_bb: (V2::zeros(), V2::zeros())
		};
		new.recompute_default_layout();
		new.check_wire_geometry_and_connections();
		Ok(new)
	}
	pub fn new_mostly_default(
		name: String,
		save_path: String
	) -> Self {
		Self::new(
			HashMap::new(),
			HashMap::new(),
			HashMap::new(),
			IntV2(0, 0),
			name,
			1,
			HashMap::new(),
			save_path,
			false,
			false,
			1.0,
			false,
			true
		).unwrap()
	}
	pub fn from_save(mut save: LogicCircuitSave, save_path: String, displayed_as_block: bool, toplevel: bool, pos: IntV2, dir: FourWayDir) -> Result<Self, String> {
		// Set position and direction correctly
		save.generic_device.ui_data.position = pos;
		save.generic_device.ui_data.direction = dir;
		// Init compnents
		let mut components = HashMap::<u64, RefCell<Box<dyn LogicDevice>>>::new();
		for (ref_, save_comp) in save.components.into_iter() {
			components.insert(ref_, RefCell::new(EnumAllLogicDevices::to_dynamic(save_comp)?));
		}
		// Get rid of Global pin sources if not toplevel and nets if toplevel
		for (_, pin_cell) in &mut save.generic_device.pins.borrow().iter() {
			let mut pin = pin_cell.borrow_mut();
			if let Some(source) = &pin.external_source {
				if toplevel {
					if let LogicConnectionPinExternalSource::Net(_) = source {
						pin.external_source = Some(LogicConnectionPinExternalSource::Global);
					}
				}
				else {
					if let LogicConnectionPinExternalSource::Global = source {
						pin.external_source = None;
					}
				}
			}
		}
		let mut out = Self {
			generic_device: save.generic_device,
			components: RefCell::new(components),
			nets: RefCell::new(hashmap_into_refcells(save.nets)),
			wires: RefCell::new(hashmap_into_refcells(save.wires)),
			save_path,
			block_pin_positions: save.block_pin_positions,
			displayed_as_block,
			tool: RefCell::new(Tool::default()),
			is_toplevel: toplevel,
			circuit_internals_bb: (V2::zeros(), V2::zeros())
		};
		out.check_wire_geometry_and_connections();
		Ok(out)
	}
	fn recompute_default_layout(&mut self) {
		// Bounding box & layout, like in CircuitVerse
		let mut count_pins_not_clock: i32 = 0;
		let mut block_pin_positions: HashMap<String, (IntV2, FourWayDir)> = HashMap::new();
		for (pin_ref, pin_cell) in self.get_pins_cell().borrow().iter() {
			if count_pins_not_clock % 2 == 0 {
				block_pin_positions.insert(pin_ref.clone(), (IntV2(-(CIRCUIT_LAYOUT_DEFAULT_HALF_WIDTH as i32) - 1, count_pins_not_clock / 2), FourWayDir::W));
			}
			else {
				block_pin_positions.insert(pin_ref.clone(), (IntV2(CIRCUIT_LAYOUT_DEFAULT_HALF_WIDTH as i32 + 1, count_pins_not_clock / 2), FourWayDir::E));
			}
			count_pins_not_clock += 1;
		}
		self.block_pin_positions = block_pin_positions;
		self.generic_device.bounding_box = (V2::new(-(CIRCUIT_LAYOUT_DEFAULT_HALF_WIDTH as f32), -1.0), V2::new(CIRCUIT_LAYOUT_DEFAULT_HALF_WIDTH as f32, ((count_pins_not_clock / 2) + 1) as f32));
	}
	/// Nets reference connections to pins and pins may reference nets, this function makes sure each "connection" is either connected both ways or deleted if one end is missing
	/// Use `update_pin_to_wire_connections()` instead
	#[deprecated]
	pub fn recompute_connections(&mut self) {
		// Net -> Pin
		let mut net_to_pin_connections_to_drop = Vec::<(u64, Vec<usize>)>::new();// (net ID, index into net's connections vec)
		for (net_id, net) in self.nets.borrow().iter() {
			let mut this_net_to_pin_connections_to_drop = Vec::<usize>::new();
			for (conn_i, net_connection) in net.borrow().connections.iter().enumerate() {
				match net_connection {
					CircuitWidePinReference::ComponentPin(component_pin_ref) => match self.components.borrow_mut().get_mut(&component_pin_ref.component_id) {
						Some(component) => match component.borrow_mut().get_pins_cell().borrow_mut().get_mut(&component_pin_ref.pin_id) {
							Some(pin_cell) => {
								pin_cell.borrow_mut().external_source = Some(LogicConnectionPinExternalSource::Net(*net_id));
							}
							None => this_net_to_pin_connections_to_drop.push(conn_i),
						},
						None => this_net_to_pin_connections_to_drop.push(conn_i)
					},
					CircuitWidePinReference::ExternalConnection(ext_conn_query) => match self.get_pins_cell().borrow_mut().get_mut(ext_conn_query) {
						Some(pin_cell) => {
							pin_cell.borrow_mut().internal_source = Some(LogicConnectionPinInternalSource::Net(*net_id));
						},
						None => this_net_to_pin_connections_to_drop.push(conn_i)
					}
				}
			}
			net_to_pin_connections_to_drop.push((*net_id, this_net_to_pin_connections_to_drop));
		}
		// Remove broken connections
		for (net_id, conns) in net_to_pin_connections_to_drop {
			for conn_i in conns.iter().rev() {// Very important to reverse list of indices to delete, because deleting lower indices first would shift the array and make later ones invalid
				self.nets.borrow().get(&net_id).unwrap().borrow_mut().connections.remove(*conn_i);
			}
		}
		// Component pin -> Net
		let mut pin_to_net_connections_to_drop = Vec::<CircuitWidePinReference>::new();
		for (comp_id, comp) in self.components.borrow().iter() {
			for (pin_id, pin) in comp.borrow().get_pins_cell().borrow().iter() {
				if let Some(source) = &pin.borrow().external_source {
					match source {
						LogicConnectionPinExternalSource::Global => panic!("Pin {:?} of component {:?} has external source 'Global' which doesn't make sense", &pin_id, &comp_id),
						LogicConnectionPinExternalSource::Net(net_id) => match self.nets.borrow_mut().get_mut(&net_id) {
							Some(net) => net.borrow_mut().edit_component_connection(true, *comp_id, pin_id),
							None => pin_to_net_connections_to_drop.push(CircuitWidePinReference::ComponentPin(ComponentPinReference::new(*comp_id, pin_id.to_owned()))),
						}
					}
				}
			}
		}
		// External pin -> Net
		for (pin_id, pin) in self.get_pins_cell().borrow().iter() {
			if let Some(source) = &pin.borrow().internal_source {
				match source {
					LogicConnectionPinInternalSource::ComponentInternal => panic!("External connection pin {} of circuit {:?} has internal source 'ComponentInternal' which doesn't make sense", pin_id, &self.generic_device.unique_name),
					LogicConnectionPinInternalSource::Net(net_query) => match self.nets.borrow().get(&net_query) {
						Some(net) => net.borrow_mut().edit_external_connection(true, pin_id),
						None => pin_to_net_connections_to_drop.push(CircuitWidePinReference::ExternalConnection(pin_id.to_owned())),
					}
				}
			}
		}
		// Remove broken Pin -> Net connections
		for pin_ref in pin_to_net_connections_to_drop {
			match pin_ref {
				CircuitWidePinReference::ComponentPin(component_pin_ref) => {
					self.components.borrow().get(&component_pin_ref.component_id).expect(&format!("Component ID {} from net connections to delete is invalid", component_pin_ref.component_id)).borrow().get_pins_cell().borrow().get(&component_pin_ref.pin_id).expect(&format!("Pin ID {} for component {} from net connections to delete is invalid", component_pin_ref.pin_id, component_pin_ref.component_id)).borrow_mut().external_source = None;
				},
				CircuitWidePinReference::ExternalConnection(ext_conn_query) => {
					self.get_pins_cell().borrow_mut().get_mut(&ext_conn_query).expect(&format!("External connection ID {} from net connections to delete is invalid", &ext_conn_query)).borrow_mut().internal_source = None;
				}
			}
		}
	}
	/// Returns: (Whether to recompute the circuit, Whether to upen the component select popup)
	pub fn toplevel_ui_interact<'a>(&mut self, response: Response, context: &egui::Context, draw: &ComponentDrawInfo<'a>, mut input_state: egui::InputState) -> bool {
		let mut return_recompute_connections = false;
		let mut new_tool_opt = Option::<Tool>::None;
		let mouse_pos_grid_opt: Option<V2> = match response.hover_pos() {
			Some(pos_px) => Some(draw.mouse_pos2_to_grid(pos_px)),
			None => None
		};
		match self.tool.borrow_mut().deref_mut() {
			Tool::Select{selected_graphics, selected_graphics_state} => {
				match selected_graphics_state {
					SelectionState::Fixed => {
						if response.drag_started_by(PointerButton::Primary) {
							let begining_mouse_pos_grid: V2 = mouse_pos_grid_opt.expect("Hover pos should work when dragging");
							*selected_graphics_state =  SelectionState::Dragging(begining_mouse_pos_grid, emath_vec2_to_v2(response.drag_delta()) / draw.grid_size);
							for item_ref in selected_graphics.iter() {
								self.run_function_on_graphic_item_mut(item_ref.clone(), |graphic_item| {
									graphic_item.start_dragging(begining_mouse_pos_grid);
								})
							}
						}
						if response.clicked() {
							// Find if command/ctrl is being held down
							let multi_select_key: bool = input_state.key_down(Key::A);// TODO: Command / Control
							// Find what was clicked (if anything)
							match self.was_anything_clicked(draw.mouse_pos2_to_grid(response.interact_pointer_pos().expect("Interact pointer pos should work when clicked"))) {
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
							new_tool_opt = Some(Tool::PlaceWire{perp_pairs: vec![], start_net_opt: None, start_wire_connections_opt: None});
						}
					},
					SelectionState::Dragging(start_grid, delta_grid) => {
						let delta_grid_backwards_y = emath_vec2_to_v2(response.drag_delta()) / draw.grid_size;
						*delta_grid += if cfg!(feature = "reverse_y") {
							V2::new(delta_grid_backwards_y.x, -delta_grid_backwards_y.y)
						} else {
							delta_grid_backwards_y
						};
						match selected_graphics.len() {
							0 => {// Drag a rectangle
								let select_bb: (V2, V2) = merge_points_to_bb(vec![*start_grid, *start_grid + *delta_grid]);
								draw.draw_rect(*start_grid, *start_grid + *delta_grid, draw.styles.select_rect_color, draw.styles.select_rect_edge_color);
								if response.drag_stopped_by(PointerButton::Primary) {
									// Find all items that have BBs intersected by the rectangle and select them
									selected_graphics.clear();
									for item_ref in self.get_all_graphics_references() {
										if self.run_function_on_graphic_item(item_ref.clone(), |graphic_item| -> bool {
											bbs_overlap(graphic_item.bounding_box(V2::zeros()), select_bb)
										}) {
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
									})
								}
								if response.drag_stopped_by(PointerButton::Primary) {
									for item_ref in selected_graphics.iter() {
										self.run_function_on_graphic_item_mut(item_ref.clone(), |graphic_item| {
											graphic_item.stop_dragging(*start_grid + *delta_grid);
										})
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
				// Draw selected items BB
				if selected_graphics.len() >= 1 {
					let mut points = Vec::<V2>::new();
					for item_ref in selected_graphics.iter() {
						self.run_function_on_graphic_item(item_ref.clone(), |graphic_item| {
							let new_bb = graphic_item.bounding_box(V2::zeros());
							points.push(new_bb.0);
							points.push(new_bb.1);
						})
					}
					let bb = merge_points_to_bb(points);
					draw.draw_rect(bb.0, bb.1, [0, 0, 0, 0], draw.styles.select_rect_edge_color);
				}
				// Copy and Paste will use a plain-text JSON array of instances of the `CopiedGraphicItem` enum
				// Copy
				if input_state.consume_shortcut(&KeyboardShortcut::new(Modifiers::NONE, Key::C)) {// TODO
					let mut copied_items = Vec::<CopiedGraphicItem>::new();
					// Get combined BB center
					let mut bb_opt = Option::<(V2, V2)>::None;
					for item_ref in selected_graphics.iter() {
						let bb_float = self.run_function_on_graphic_item::<(V2, V2)>(item_ref.clone(), |item_box| item_box.bounding_box(V2::zeros()));
						bb_opt = Some(match bb_opt.clone() {
							Some(bb) => {
								merge_points_to_bb(vec![bb_float.0, bb_float.1, bb.0, bb.1])
							}
							None => bb_float
						});
					}
					// If there is a BB then at least one selected item to copy, otherwise do nothing
					if let Some(bb) = bb_opt {
						let bb_center: V2 = (bb.1 + bb.0) / 2.0;
						//let bb_int = (IntV2(bb_float.0.x as i32, bb_float.0.y as i32), IntV2(bb_float.1.x as i32, bb_float.1.y as i32));
						for item_ref in selected_graphics.iter() {
							copied_items.push(self.copy_graphic_item(item_ref.clone()));
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
							let pasted_selected_graphics = self.paste(&item_set);
							new_tool_opt = Some(Tool::Select{selected_graphics: HashSet::from_iter(pasted_selected_graphics.into_iter()), selected_graphics_state: SelectionState::FollowingMouse(mouse_pos_grid_opt.unwrap())});
						},
						Err(_) => {}
					}
				}
				// Delete
				if input_state.consume_key(Modifiers::NONE, Key::Backspace) {
					for item_ref in selected_graphics.iter() {
						self.remove_graphic_item(item_ref);
					}
					return_recompute_connections = true;
					*selected_graphics = HashSet::new();
				}
			},
			Tool::HighlightNet(net_id) => {
				// TODO
			},
			Tool::PlaceWire{perp_pairs, start_net_opt, start_wire_connections_opt} => {
				match response.hover_pos() {
					Some(mouse_pos_px) => {
						let mouse_pos_grid_rounded: IntV2 = round_v2_to_intv2(draw.mouse_pos2_to_grid(mouse_pos_px));
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
								let (is_term_point, net_opt, end_connections_opt) = self.is_connection_point(mouse_pos_grid_rounded);
								if is_term_point {
									// End wire
									self.add_wire_geometry(perp_pairs.clone(), mouse_pos_grid_rounded, *start_net_opt, net_opt, clone_option_rc(start_wire_connections_opt), end_connections_opt);
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
							// Display in-progress wire
							for (i, pair) in perp_pairs.iter().enumerate() {
								let end_pos = if i == perp_pairs.len() - 1 {
									mouse_pos_grid_rounded
								}
								else {
									perp_pairs[i+1].0
								};
								let segments = Wire::perpindicular_pair_to_segments(pair, end_pos);
								for segment in segments {
									draw.draw_polyline(vec![
										segment.0.to_v2(),
										segment.0.to_v2() + (segment.1.to_unit() * (segment.2 as f32))
									], draw.styles.color_wire_in_progress);
								}
							}
						}
						// Wire not started
						else {
							if response.clicked_by(PointerButton::Primary) {
								let (_, net_opt, start_opt) = self.is_connection_point(mouse_pos_grid_rounded);
								perp_pairs.push((
									mouse_pos_grid_rounded,
									FourWayDir::E
								));
								*start_net_opt = net_opt;
								*start_wire_connections_opt = start_opt;
							}
						}
					}
					None => {
						// TODO: Maybe delete this and turn match into if let?
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
		return_recompute_connections
	}
	pub fn paste(&self, item_set: &CopiedItemSet) -> Vec<GraphicSelectableItemRef> {
		let mut out = Vec::<GraphicSelectableItemRef>::new();
		for pasted_item in &item_set.items {
			out.push(match pasted_item {
				CopiedGraphicItem::Component(comp_save) => self.insert_component(comp_save),
				CopiedGraphicItem::ExternalConnection(pin) => {
					let mut pins = self.generic_device.pins.borrow_mut();
					let name = new_pin_name(&*pins);
					pins.insert(name.clone(), RefCell::new(pin.clone()));
					GraphicSelectableItemRef::Pin(name)
				},
				CopiedGraphicItem::Wire((pos, dir, len)) => {
					let wire_ids = self.add_wire_geometry(vec![(*pos, *dir)], *pos + dir.to_unit_int().mult(*len as i32), None, None, None, None);
					GraphicSelectableItemRef::Wire(wire_ids[0])// There shoud be exactly one
				}
			});
		}
		// Set each item's pre-drag position to the difference from the BB center it it's position
		for item_ref in &out {
			self.run_function_on_graphic_item_mut(item_ref.clone(), |item_box| {item_box.get_ui_data_mut().position_before_dragging = item_box.get_ui_data().position - item_set.bb_center;});
		}
		out
	}
	pub fn insert_component(&self, comp_save: &EnumAllLogicDevices) -> GraphicSelectableItemRef {
		let mut components = self.components.borrow_mut();
		let new_comp_id = lowest_unused_key(&components);
		components.insert(new_comp_id, RefCell::new(EnumAllLogicDevices::to_dynamic(comp_save.clone()).unwrap()));
		GraphicSelectableItemRef::Component(new_comp_id)
	}
	/// Checks: All wires, external pins, component pins
	/// Returns: (Is termination point, Optional net, Optional shared wire connection set)
	fn is_connection_point(&self, point: IntV2) -> (bool, Option<u64>, Option<Rc<RefCell<HashSet<WireConnection>>>>) {
		if let Some((_, net_id, _, connections_opt)) = self.is_point_on_wire(point, None) {
			return (true, Some(net_id), connections_opt);
		}
		for (_, pin) in self.get_pins_cell().borrow().iter() {
			if pin.borrow().ui_data.position == point {
				if let Some(source) = &pin.borrow().internal_source {
					match source {
						LogicConnectionPinInternalSource::Net(net_id) => {
							return (true, Some(*net_id), None);
						},
						LogicConnectionPinInternalSource::ComponentInternal => panic!("Circuit external connection should not have ComponentInternal source")
					}
				}
				return (true, None, None);
			}
		}
		for (_, comp_cell) in self.components.borrow().iter() {
			let comp = comp_cell.borrow();
			for (_, pin) in comp.get_pins_cell().borrow().iter() {
				if pin.borrow().ui_data.position + comp.get_ui_data().position == point {
					if let Some(source) = &pin.borrow().external_source {
						match source {
							LogicConnectionPinExternalSource::Net(net_id) => {
								return (true, Some(*net_id), None);
							},
							_ => panic!("Component connection should be to a Net and not something else")
						}
					}
					return (true, None, None);
				}
			}
		}
		(false, None, None)
	}
	/// Returns: Option<(Wire ID, Net ID, Wire intrcept triple, Optional end connection set)>
	fn is_point_on_wire(&self, point: IntV2, wire_to_exclude_opt: Option<u64>) -> Option<(u64, u64, (bool, bool, bool), Option<Rc<RefCell<HashSet<WireConnection>>>>)> {
		for (wire_id, wire) in self.wires.borrow().iter() {
			if let Some(wire_to_exclude) = wire_to_exclude_opt {
				if *wire_id == wire_to_exclude {
					continue;
				}
			}
			let (bool_triple, wire_connections_opt) = wire.borrow().contains_point(point);
			if bool_triple.0 || bool_triple.1 || bool_triple.2 {
				return Some((*wire_id, wire.borrow().net, bool_triple, wire_connections_opt));
			}
		}
		None
	}
	/// Adds new wires to circuit, `perp_segment_pairs` works the same as described in `Tool::PlaceWire`
	/// Returns: Vec of new wire IDs
	fn add_wire_geometry(
		&self,
		perp_segment_pairs: Vec<(IntV2, FourWayDir)>,
		ending_pos: IntV2, start_net: Option<u64>,
		end_net: Option<u64>,
		wire_string_start_connections_opt: Option<Rc<RefCell<HashSet<WireConnection>>>>,
		wire_string_end_connections_opt: Option<Rc<RefCell<HashSet<WireConnection>>>>
	) -> Vec<u64> {
		// Get net to use or make a new one
		let new_wires_net: u64 = match start_net {
			Some(net_id) => net_id,
			None => match end_net {
				Some(net_id) => net_id,
				None => {// Create new net
					let mut nets = self.nets.borrow_mut();
					let new_id = lowest_unused_key(nets.deref());
					nets.insert(new_id, RefCell::new(LogicNet::new(vec![])));
					//println!("Add wire geometry: New net created: ID={}", new_id);
					new_id
				}
			}
		};
		//println!("Add wire geometry: net ID={}", new_wires_net);
		// Create vec of bare wire geometries
		let mut wires = self.wires.borrow_mut();
		let mut new_wire_segment_geometries = Vec::<(IntV2, FourWayDir, u32)>::new();
		for (i, pair) in perp_segment_pairs.iter().enumerate() {
			let end_pos = if i == perp_segment_pairs.len() - 1 {
				ending_pos
			}
			else {
				perp_segment_pairs[i+1].0
			};
			let segments = Wire::perpindicular_pair_to_segments(pair, end_pos);
			for segment in segments {
				new_wire_segment_geometries.push(segment);
			}
		}
		// Create new wire IDs now that the number of wire segments is known
		let new_wire_ids: Vec<u64> = batch_unused_keys(&*wires, new_wire_segment_geometries.len());
		// Place down as actual wires
		for (i, segment) in new_wire_segment_geometries.iter().enumerate() {
			let new_wire_id: u64 = new_wire_ids[i];
			let end_connections: Rc<RefCell<HashSet<WireConnection>>> = if i == new_wire_segment_geometries.len() - 1 {
				match &wire_string_end_connections_opt {
					Some(end_connections) => Rc::clone(end_connections),
					None => Rc::new(RefCell::new(HashSet::new()))
				}
			}
			else {
				Rc::new(RefCell::new(HashSet::from_iter(vec![WireConnection::Wire(new_wire_ids[i+1])].into_iter())))
			};
			let start_connections: Rc<RefCell<HashSet<WireConnection>>> = if i == 0 {
				match &wire_string_start_connections_opt {
					Some(start_connections) => Rc::clone(start_connections),
					None => Rc::new(RefCell::new(HashSet::new()))
				}
			}
			else {
				// There is a previus segment in the new chain, clone that one's end connectins Rc
				Rc::clone(&wires.get(&new_wire_ids[i-1]).unwrap().borrow().end_connections)
			};
			let this_wire_connection = WireConnection::Wire(new_wire_id);
			end_connections.borrow_mut().insert(this_wire_connection.clone());
			start_connections.borrow_mut().insert(this_wire_connection);
			let new_wire = Wire::new(
				segment.0, segment.2,
				segment.1,
				1,
				new_wires_net,
				start_connections,
				end_connections
			);
			wires.insert(new_wire_id, RefCell::new(new_wire));
		}
		new_wire_ids
	}
	/// Fixes everything, should be run when a new circuit is created or when anything is moved, deleted, or placed
	pub fn check_wire_geometry_and_connections(&mut self) {
		// Remove all wire connections except to themselves
		self.check_wires_connected_to_just_themselves();
		// Find overlapping wires and correct them, make sure to preserve connections
		// TODO
		// Combine overlapping but seperate end connection and T-connection HashSets
		self.combine_overlapping_wire_connection_sets();
		// Traverse wires by end connections, check that they are all on the same net, possibly merge nets
		self.check_all_connected_wires_on_same_net();
		// Check if any net spans multiple "islands" of wires, split the net
		self.split_disjoint_nets();
		// Remove all connections from nets, the legit ones will be added back later
		self.remove_net_connections();
		// Everything so far just deals with wires, now update pin connections to the wires, possibly changing pin nets
		self.update_pin_to_wire_connections();
		// Combine consecutive segments in the same direction
		self.merge_consecutive_wires();
		// Recompute BB for circuit internals
		self.recompute_internals_bb();
	}
	fn check_wires_connected_to_just_themselves(&self) {
		let wires = self.wires.borrow();
		for (wire_id, wire_cell) in wires.iter() {
			let this_connection = WireConnection::Wire(*wire_id);
			let mut wire = wire_cell.borrow_mut();
			wire.start_connections = Rc::new(RefCell::new(HashSet::from_iter(vec![this_connection.clone()].into_iter())));
			wire.end_connections = Rc::new(RefCell::new(HashSet::from_iter(vec![this_connection].into_iter())));
		}
	}
	/// Should be called AFTER the overlapping parallel wire checks, so that T-connections can only be intercepting along one wire
	fn combine_overlapping_wire_connection_sets(&self) {
		// First: T-conn check
		// vec<(Wire with end at pos, Wire to be sliced, Position)>
		// The code is in a seperate block so that the self.wires borrow will be dropped and won't cause borrow issues when T connections are fixed
		let t_conns_to_fix: Vec<(u64, u64, IntV2)> = {
			let wires = self.wires.borrow();
			let mut out = Vec::<(u64, u64, IntV2)>::new();
			for (wire_id, wire_cell) in wires.iter() {
				let wire = wire_cell.borrow();
				let start_pos = wire.ui_data.position;
				let end_pos = wire.end_pos();
				if let Some((sliced_wire_id, _, intercept, _)) = self.is_point_on_wire(start_pos, Some(*wire_id)) {// Some(*wire_id) is provided to avoid detecting an interception with the same wire
					if intercept == (false, true, false) {// Intercepting other wire in the middle
						out.push((*wire_id, sliced_wire_id, start_pos));
					}
				}
				if let Some((sliced_wire_id, _, intercept, _)) = self.is_point_on_wire(end_pos, Some(*wire_id)) {// Some(*wire_id) is provided to avoid detecting an interception with the same wire
					if intercept == (false, true, false) {// Intercepting other wire in the middle
						out.push((*wire_id, sliced_wire_id, end_pos));
					}
				}
			}
			out
		};
		for t_conn in t_conns_to_fix {
			let wire_conn = WireConnection::Wire(t_conn.0);
			self.add_connection_to_wire(wire_conn, t_conn.1, t_conn.2, (false, true, false));
		}
		// Now for end-to-end connections
		let wires = self.wires.borrow();
		for (wire_id, wire_cell) in wires.iter() {
			for (wire_id_2, wire_cell_2) in wires.iter() {
				if wire_id <= wire_id_2 {// This is a convenient way to avoid double-checking and self-checking
					continue;
				}
				let wire = wire_cell.borrow();
				let start_pos = wire.ui_data.position;
				let end_pos = wire.end_pos();
				let mut wire_2 = wire_cell_2.borrow_mut();
				let start_pos_2 = wire_2.ui_data.position;
				let end_pos_2 = wire_2.end_pos();
				if start_pos == start_pos_2 {
					merge_wire_end_connection_sets(&wire.start_connections, &wire_2.start_connections);
					wire_2.start_connections = Rc::clone(&wire.start_connections);
				}
				if start_pos == end_pos_2 {
					merge_wire_end_connection_sets(&wire.start_connections, &wire_2.end_connections);
					wire_2.end_connections = Rc::clone(&wire.start_connections);
				}
				if end_pos == start_pos_2 {
					merge_wire_end_connection_sets(&wire.end_connections, &wire_2.start_connections);
					wire_2.start_connections = Rc::clone(&wire.end_connections);
				}
				if end_pos == end_pos_2 {
					merge_wire_end_connection_sets(&wire.end_connections, &wire_2.end_connections);
					wire_2.end_connections = Rc::clone(&wire.end_connections);
				}
			}
		}
	}
	/// How it works:
	/// For each wire ("Wire 1") and each wire ("Wire 2") connected to Wire 1:
	/// 	create flags to optionally change one wire's net to the same as the other wire
	/// 	create flag to optionally migrate one net to another by modifying both `visited_wires` and `net_wire_associations`
	/// 	Compare the wire's nets:
	/// 		Same => {}
	/// 		Different => Check which wires are included in `visited_wires`:
	/// 			Neither => Set flag to set wire 2's net to wire 1's net (or other way, this is arbitrary)
	/// 			One => Set flag of unincluded wire to use net of included wire
	/// 			Two => Set flag to set wire 2's net to wire 1's net. Also set flag to update all of net 2 to net 1
	/// 	Add both wires to `visited_wires` and `net_wire_associations`
	/// 	If the net migrate flag is set:
	/// 		Move all net wire associations from the old net to the new one and change all of the associated wire's nets
	/// 	If the change wire net flag is set:
	/// 		Change wire net to that of wire other wire
	/// 		Add wire to `net_wire_associations` again with net of other wire
	/// 	else:
	/// 		Add wires to `net_wire_associations` normally
	fn check_all_connected_wires_on_same_net(&self) {
		// {Wire ID, ...}
		let mut visited_wires = HashSet::<u64>::new();
		// {Net ID: {Wire ID, ...}}
		// For keeping track of what wires are on a net so they can be updated when nets have to be merged
		let mut net_wire_associations = HashMap::<u64, HashSet<u64>>::new();
		let wires = self.wires.borrow();
		for (wire_1_id, wire_1_cell) in wires.iter() {
			let mut wire_1 = wire_1_cell.borrow_mut();
			let wire_1_connections: Vec<WireConnection> = wire_1.start_connections.borrow().iter().chain(wire_1.end_connections.borrow().iter()).map(|conn_ref| conn_ref.clone()).collect();
			for wire_connection in &wire_1_connections {
				match wire_connection {
					WireConnection::Wire(wire_2_id) => {
						if wire_1_id != wire_2_id {// Don't compare to itself because dynamic borrow error!
							let mut wire_2 = wires.get(wire_2_id).unwrap().borrow_mut();
							// (Net ID to assign to both wires, Optional net migration (Old net, New net))
							let (both_wires_eventual_net, net_migration): (u64, Option<(u64, u64)>) = match wire_1.net == wire_2.net {
								true => (wire_1.net, None),
								false => {// Check which wires are included in `visited_wires`
									let wire_1_visited = visited_wires.contains(wire_1_id);
									let wire_2_visited = visited_wires.contains(wire_2_id);
									if wire_1_visited && wire_2_visited {// Two => Set flag to set wire 2's net to wire 1's net. Also set flag to update all of net 2 to net 1
										(wire_1.net, Some((wire_2.net, wire_1.net)))
									}
									else {
										if wire_1_visited || wire_2_visited {// One => Set flag of unincluded wire to use net of included wire
											if wire_1_visited {
												(wire_1.net, None)
											}
											else {
												(wire_2.net, None)
											}
										}
										else {// Neither => Set flag to set wire 2's net to wire 1's net (or other way, this is arbitrary)
											(wire_1.net, None)
										}
									}
								}
							};
							// Add both wires to `visited_wires` and `net_wire_associations`
							visited_wires.insert(*wire_1_id);
							visited_wires.insert(*wire_2_id);
							if let Some(wire_assoc_set) = net_wire_associations.get_mut(&both_wires_eventual_net) {
								wire_assoc_set.insert(*wire_1_id);
								wire_assoc_set.insert(*wire_2_id);
							}
							else {
								let mut wire_assoc_set = HashSet::<u64>::new();
								wire_assoc_set.insert(*wire_1_id);
								wire_assoc_set.insert(*wire_2_id);
								net_wire_associations.insert(both_wires_eventual_net, wire_assoc_set);
							}
							// Possible net migration
							if let Some((old_net, new_net)) = net_migration {
								let net_missing_err = "Net migration is only supposed to happen if both wires are already visited, meaning they're net should be in `net_wire_associations`";
								let old_net_wires = net_wire_associations.get(&old_net).expect(net_missing_err).clone();
								let new_net_wires = net_wire_associations.get_mut(&new_net).expect(net_missing_err);
								for wire_id in &old_net_wires {
									// Update wire's net ref
									if !(wire_id == wire_1_id || wire_id == wire_2_id) {
										wires.get(wire_id).unwrap().borrow_mut().net = new_net;
									}
									// Add wire to new net wire association set
									new_net_wires.insert(*wire_id);
								}
							}
							// Assign common net ID to wires
							wire_1.net = both_wires_eventual_net;
							wire_2.net = both_wires_eventual_net;
						}
					},
					WireConnection::Pin(_) => {}
				}
			}
		}
	}
	/// From Gemini
	/// Called AFTER `self.check_all_connected_wires_on_same_net()`
	/// This function uses the wire end connections (no spacial checking needed) to determine if a single net spans multiple "islands" of wires
	/// Only needs to update the net fields of wires, so do not care about updating connected pins because that will happen later
	/// May add new nets
	fn split_disjoint_nets(&self) {
		let wires = self.wires.borrow();
		let mut nets = self.nets.borrow_mut();

		// 1. Group all wires by their net ID.
		// {Net ID: {Wire ID, ...}}
		let mut wires_by_net = HashMap::<u64, HashSet<u64>>::new();
		for (wire_id, wire_cell) in wires.iter() {
			let wire = wire_cell.borrow();
			wires_by_net.entry(wire.net).or_default().insert(*wire_id);
		}

		// A vec to store net modifications to avoid borrowing issues.
		// vec<(Wire IDs in the new island, original net ID))
		let mut islands_to_create_nets_for = Vec::<(HashSet<u64>, u64)>::new();

		// 2. Iterate through each net group to find disjoint islands.
		for (net_id, all_wires_in_net) in wires_by_net.iter() {
			if all_wires_in_net.is_empty() {
				continue;
			}

			let mut visited_wires_in_net = HashSet::<u64>::new();
			let mut is_first_island = true;

			while visited_wires_in_net.len() < all_wires_in_net.len() {
				// Find a starting wire for the next island search that hasn't been visited.
				let start_wire_id = *all_wires_in_net
					.iter()
					.find(|wire_id| !visited_wires_in_net.contains(wire_id))
					.unwrap();

				// 3. Use BFS to find all connected wires in the current island.
				let current_island = self.find_wire_island(start_wire_id, &wires);
				
				visited_wires_in_net.extend(&current_island);

				// 4. The first island stays on the original net. Subsequent islands need a new net.
				if is_first_island {
					is_first_island = false;
				} else {
					islands_to_create_nets_for.push((current_island, *net_id));
				}
			}
		}
		
		// 5. Create new nets and re-assign wires for the found islands.
		for (island_wires, _) in islands_to_create_nets_for {
			// Now it's safe to get a new net ID and then mutate `nets`.
			let new_net_id = lowest_unused_key(&nets);
			let new_net = LogicNet::new(Vec::new());
			nets.insert(new_net_id, RefCell::new(new_net));

			// Re-assign all wires in the island to the new net.
			for wire_id in island_wires {
				if let Some(wire_cell) = wires.get(&wire_id) {
					wire_cell.borrow_mut().net = new_net_id;
				}
			}
		}
	}
	/// From Gemini
	/// Helper function to find a connected "island" of wires using BFS.
	///
	/// # Arguments
	/// * `start_wire_id` - The ID of the wire to start the search from.
	/// * `wires` - A reference to the circuit's wires HashMap.
	///
	/// # Returns
	/// A HashSet containing the IDs of all wires in the connected island.
	fn find_wire_island(
		&self,
		start_wire_id: u64,
		wires: &HashMap<u64, RefCell<Wire>>,
	) -> HashSet<u64> {
		let mut q = VecDeque::new();
		let mut island_wires = HashSet::new();

		q.push_back(start_wire_id);
		island_wires.insert(start_wire_id);

		while let Some(current_wire_id) = q.pop_front() {
			let current_wire_cell = wires.get(&current_wire_id).unwrap();
			let current_wire = current_wire_cell.borrow();

			// Check both ends of the wire for connections
			let connections = current_wire
				.start_connections
				.borrow()
				.iter()
				.chain(current_wire.end_connections.borrow().iter())
				.cloned()
				.collect::<Vec<WireConnection>>();

			for connection in connections {
				if let WireConnection::Wire(connected_wire_id) = connection {
					if !island_wires.contains(&connected_wire_id) {
						island_wires.insert(connected_wire_id);
						q.push_back(connected_wire_id);
					}
				}
			}
		}
		island_wires
	}
	fn remove_net_connections(&self) {
		let nets = self.nets.borrow();
		for (_, net_cell) in nets.iter() {
			let mut net = net_cell.borrow_mut();
			net.connections = Vec::new();
		}
	}
	/// Almost last step in recomputing circuit connections after the circuit is edited
	/// If any pins (component or external) touch any wire, update the pin's net to the wire's net
	/// Also make sure pins not touching have no connection
	/// Consecutive-wires-in-same-direction check should happen after this so that any triple-joints that have been abandoned get merged back to a signle wire
	fn update_pin_to_wire_connections(&self) {
		// Component pins
		for (comp_id, comp_cell) in self.components.borrow().iter() {
			let comp = comp_cell.borrow();
			for (pin_id, pin_cell) in comp.get_pins_cell().borrow().iter() {
				let mut pin = pin_cell.borrow_mut();
				let pin_pos: IntV2 = pin.ui_data.position + comp.get_ui_data().position;
				// Fist check if pin is already connected to another net and remove from that net's connection list in case it's a different net
				if let Some(source) = &pin.external_source {
					if let LogicConnectionPinExternalSource::Net(old_net_id) = source {
						self.nets.borrow().get(old_net_id).expect(&format!("Net ID {} invalid when removing pin \"{}\" from its connection list", old_net_id, pin_id)).borrow_mut().edit_component_connection(false, *comp_id, pin_id);
					}
				}
				match self.is_point_on_wire(pin_pos, None) {
					Some((wire_id, new_net_id, wire_intercept_triple, _)) => {
						{// Make sure wire reference is dropped before calling `self.add_connection_to_wire`
							//let wire = self.wires.borrow_mut();
							//let new_net_id: u64 = wire.get(&wire_id).expect(&format!("self.is_point_on_wire() returned invalid wire ID {}", wire_id)).borrow().net;
							pin.external_source = Some(LogicConnectionPinExternalSource::Net(new_net_id));
							self.nets.borrow().get(&new_net_id).expect("Net ID invalid").borrow_mut().edit_component_connection(true, *comp_id, pin_id);
						}
						// Update wire connection
						self.add_connection_to_wire(WireConnection::Pin(CircuitWidePinReference::ComponentPin(ComponentPinReference::new(*comp_id, pin_id.clone()))), wire_id, pin_pos, wire_intercept_triple);
					},
					None => {
						pin.external_source = None;
					}
				}
			}
		}
		// External pins
		for (pin_id, pin_cell) in self.get_pins_cell().borrow().iter() {
			let mut pin = pin_cell.borrow_mut();
			let pin_pos: IntV2 = pin.ui_data.position;
			// Fist check if pin is already connected to another net and remove from that net's connection list in case it's a different net
			if let Some(source) = &pin.internal_source {
				if let LogicConnectionPinInternalSource::Net(old_net_id) = source {
					self.nets.borrow().get(old_net_id).expect("Net ID invalid").borrow_mut().edit_external_connection(false, &pin_id);
				}
			}
			match self.is_point_on_wire(pin_pos, None) {
				Some((wire_id, new_net_id, wire_intercept_triple, _)) => {
					{// Make sure wire reference is dropped before calling `self.add_connection_to_wire`
						//let wires = self.wires.borrow_mut();
						//let new_net_id: u64 = wires.get(&wire_id).expect(&format!("self.is_point_on_wire() returned invalid wire ID {}", wire_id)).borrow().net;
						pin.internal_source = Some(LogicConnectionPinInternalSource::Net(new_net_id));
						self.nets.borrow().get(&new_net_id).expect(&format!("Wire {} has invalid net ID {}", wire_id, new_net_id)).borrow_mut().edit_external_connection(true, &pin_id);
					}
					// Update wire connection
					self.add_connection_to_wire(WireConnection::Pin(CircuitWidePinReference::ExternalConnection(pin_id.clone())), wire_id, pin_pos, wire_intercept_triple);
				},
				None => {
					pin.internal_source = None;
				}
			}
		}
	}
	/// Checks for consecutive wire segments that are in the same direction and are only connected to each other. Wires like this can be merged into one wire segment
	/// It also takes care of updating the end connections
	/// Should be run AFTER the pin update and wire connection update functions
	/// Does not deal with overlapping wires
	fn merge_consecutive_wires(&self) {
		// TODO
	}
	/// Will split a wire into two sections if the joint is somewhere in the middle, otherwise just adds it at one end
	/// Returns: The shared connection set that should be used in case a wire is what is being connected
	fn add_connection_to_wire(&self, connection: WireConnection, wire_id: u64, position: IntV2, wire_intercept_triple: (bool, bool, bool)) -> Rc<RefCell<HashSet<WireConnection>>> {
		match wire_intercept_triple {
			(true, false, false) => {
				let conns = Rc::clone(&self.wires.borrow().get(&wire_id).unwrap().borrow().start_connections);
				conns.borrow_mut().insert(connection);
				conns
			},
			(false, true, false) => {// Break wire in two
				let new_wire_id: u64 = lowest_unused_key(&*self.wires.borrow());
				// Get info from original wire (which stays in same position, length reduced)
				let (middle_conns, end_conns, direction, new_length, bit_width, net): (Rc<RefCell<HashSet<WireConnection>>>, Rc<RefCell<HashSet<WireConnection>>>, FourWayDir, u32, u32, u64) = {
					let binding = self.wires.borrow();
					let mut wire = binding.get(&wire_id).unwrap().borrow_mut();
					// Calculate new lengths
					let wire_len = (position - wire.ui_data.position).taxicab();
					let new_wire_len = (wire.end_pos() - position).taxicab();
					wire.length = wire_len;// Assign this later so `wire.end_pos()` can be used
					// Change old wire's connections to just itself, the new wire, and the new connection
					let end_conns = Rc::clone(&wire.end_connections);
					wire.end_connections = Rc::new(RefCell::new(HashSet::from_iter(vec![
						WireConnection::Wire(wire_id),
						WireConnection::Wire(new_wire_id),
						connection.clone()
					].into_iter())));
					(Rc::clone(&wire.end_connections), end_conns, wire.direction.clone(), new_wire_len, wire.bit_width, wire.net)
				};
				// Update old end conns (`end_conns`) to remove old wire and add new wire
				{
					let mut end_conns_borrowed = end_conns.borrow_mut();
					end_conns_borrowed.remove(&WireConnection::Wire(wire_id));
					end_conns_borrowed.insert(WireConnection::Wire(new_wire_id));
				}
				// Create new wire
				self.wires.borrow_mut().insert(new_wire_id, RefCell::new(Wire::new(position, new_length, direction, bit_width, net, Rc::clone(&middle_conns), end_conns)));
				middle_conns
			},
			(false, false, true) => {
				let conns = Rc::clone(&self.wires.borrow().get(&wire_id).unwrap().borrow().end_connections);
				conns.borrow_mut().insert(connection);
				conns
			},
			other => panic!("LogicCircuit.add_connection_to_wire() provided with invalid wire intercept triple: {:?}", other)
		}
	}
	fn recompute_internals_bb(&mut self) {
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
	fn was_anything_clicked<'a>(&self, grid_pos: V2) -> Option<GraphicSelectableItemRef> {
		let mut selected_opt = Option::<GraphicSelectableItemRef>::None;
		for ref_ in self.get_all_graphics_references() {
			self.run_function_on_graphic_item_mut(ref_.clone(), |graphic_item| {
				if graphic_item.is_click_hit(grid_pos, V2::zeros()) {
					if !graphic_item.accept_click() {
						selected_opt = Some(ref_.clone());
					}
				}
			});
		}
		selected_opt
	}
	fn remove_graphic_item(&self, ref_: &GraphicSelectableItemRef) {
		let error_msg = format!("Graphic item reference {:?} cannot be found", &ref_);
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => {self.components.borrow_mut().remove(comp_id).expect(&error_msg);},
			GraphicSelectableItemRef::Wire(wire_id) => {self.wires.borrow_mut().remove(wire_id).expect(&error_msg);},
			GraphicSelectableItemRef::Pin(pin_id) => {self.generic_device.pins.borrow_mut().remove(pin_id).expect(&error_msg);},
		}
	}
	pub fn run_function_on_graphic_item<T>(&self, ref_: GraphicSelectableItemRef, mut func: impl FnMut(Box<&dyn GraphicSelectableItem>) -> T) -> T {
		let error_msg = format!("Graphic item reference {:?} cannot be found", &ref_);
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => func(Box::new(logic_device_to_graphic_item(self.components.borrow().get(&comp_id).expect(&error_msg).borrow().deref().as_ref()))),
			GraphicSelectableItemRef::Wire(wire_id) => func(Box::new(self.wires.borrow().get(&wire_id).expect(&error_msg).borrow().deref())),
			GraphicSelectableItemRef::Pin(pin_id) => func(Box::new(self.generic_device.pins.borrow().get(&pin_id).expect(&error_msg).borrow().deref())),
		}
	}
	pub fn run_function_on_graphic_item_mut<T>(&self, ref_: GraphicSelectableItemRef, mut func: impl FnMut(Box<&mut dyn GraphicSelectableItem>) -> T) -> T {
		let error_msg = format!("Graphic item reference {:?} cannot be found", &ref_);
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => func(Box::new(logic_device_to_graphic_item_mut(self.components.borrow().get(&comp_id).expect(&error_msg).borrow_mut().deref_mut().as_mut()))),
			GraphicSelectableItemRef::Wire(wire_id) => func(Box::new(self.wires.borrow().get(&wire_id).expect(&error_msg).borrow_mut().deref_mut())),
			GraphicSelectableItemRef::Pin(pin_id) => func(Box::new(self.generic_device.pins.borrow().get(&pin_id).expect(&error_msg).borrow_mut().deref_mut())),
		}
	}
	/// Copy something(s) that have been selected and return a `CopiedGraphicItem` that can be put onto the clipboard as JSON
	fn copy_graphic_item(&self, ref_: GraphicSelectableItemRef) -> CopiedGraphicItem {
		let error_msg = format!("Graphic item reference {:?} cannot be found", &ref_);
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => self.components.borrow().get(&comp_id).expect(&error_msg).borrow().copy(),
			GraphicSelectableItemRef::Wire(wire_id) => self.wires.borrow().get(&wire_id).expect(&error_msg).borrow().copy(),
			GraphicSelectableItemRef::Pin(pin_id) => self.generic_device.pins.borrow().get(&pin_id).expect(&error_msg).borrow().copy(),
		}
	}
	/*fn delete_graphic_item(&mut self, ref_: GraphicSelectableItem) {
		let error_msg = format!("Graphic item reference {:?} cannot be found", &ref_);
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => func(Box::new(logic_device_to_graphic_item(self.components.get_item_tuple(&(comp_id.into())).expect(&error_msg).1.borrow().deref().as_ref()))),
			GraphicSelectableItemRef::Wire(wire_id) => func(Box::new(wire_to_graphic_item(&self.wires.get_item_tuple(&(wire_id.into())).expect(&error_msg).1))),
			GraphicSelectableItemRef::Pin(pin_id) => func(Box::new(pin_to_graphic_item(&self.generic_device.pins.get_item_tuple(&(pin_id.into())).expect(&error_msg).1))),
		}
	}*/
	fn get_all_graphics_references(&self) -> Vec<GraphicSelectableItemRef> {
		let mut out = Vec::<GraphicSelectableItemRef>::new();
		for (ref_, _) in self.components.borrow().iter() {
			out.push(GraphicSelectableItemRef::Component(*ref_));
		}
		for (ref_, _) in self.wires.borrow().iter() {
			out.push(GraphicSelectableItemRef::Wire(*ref_));
		}
		for (ref_, _) in self.generic_device.pins.borrow().iter() {
			out.push(GraphicSelectableItemRef::Pin(ref_.clone()));
		}
		// TODO: Text boxes once I implement them
		out
	}
	fn compute_immutable(&self, ancestors_above: &AncestryStack) {
		let ancestors = ancestors_above.push(&self);
		// Update net states
		let mut new_net_states = HashMap::<u64, (LogicState, Vec<GlobalSourceReference>)>::new();
		for (net_id, net) in self.nets.borrow().iter() {
			new_net_states.insert(*net_id, net.borrow().update_state(&ancestors, *net_id));
		}
		drop(ancestors);
		for (net_id, (state, sources)) in new_net_states.into_iter() {
			let binding = self.nets.borrow();
			let mut net_mut_ref = binding.get(&net_id).unwrap().borrow_mut();
			net_mut_ref.state = state;
			net_mut_ref.sources = sources;
		}
		// Update pin & wire states from nets
		// Wires
		for (wire_id, wire_cell) in self.wires.borrow_mut().iter_mut() {
			let net_id = wire_cell.borrow().net;
			wire_cell.borrow_mut().state = self.nets.borrow().get(&net_id).expect(&format!("Wire {} has invalid net ID {}", wire_id, net_id)).borrow().state;
		}
		// External connection pins
		for (pin_id, pin_cell) in self.generic_device.pins.borrow_mut().iter_mut() {
			let int_source_opt = pin_cell.borrow().internal_source.clone();
			if let Some(source) = int_source_opt {
				match source {
					LogicConnectionPinInternalSource::Net(net_id) => pin_cell.borrow_mut().set_drive_internal(self.nets.borrow().get(&net_id).expect(&format!("External connection pin {} has invalid net query {}", pin_id, net_id)).borrow().state),
					LogicConnectionPinInternalSource::ComponentInternal => panic!("External connection pin {} for circuit \"{}\" has the internal source as ComponentInternal which shouldn't happen", pin_id, &self.generic_device.unique_name)
				}
			}
			else {
				pin_cell.borrow_mut().set_drive_internal(LogicState::Floating);
			}
		}
		// Component pins, and propagate through components
		for (comp_id, comp) in self.components.borrow().iter() {
			for (pin_id, pin_cell) in comp.borrow_mut().get_generic_mut().pins.borrow_mut().iter_mut() {
				let ext_source_opt = pin_cell.borrow().external_source.clone();
				if let Some(source) = ext_source_opt {
					match source {
						LogicConnectionPinExternalSource::Global => panic!("Pin {} of component {} has external source 'Global' which doesn't make sense", pin_id, comp_id),
						LogicConnectionPinExternalSource::Net(net_id) => {
							pin_cell.borrow_mut().set_drive_external(self.nets.borrow_mut().get_mut(&net_id).expect(&format!("Pin {} of component {} references net {} which doesn't exist", pin_id, comp_id, net_id)).borrow().state);
						}
					}
				}
				else {
					pin_cell.borrow_mut().set_drive_external(LogicState::Floating);
				}
			}
		}
		// THIS GOES LAST, has to be seperate loop then before
		let ancestors = ancestors_above.push(&self);
		for (_, comp) in self.components.borrow().iter() {
			let is_circuit = comp.borrow().is_circuit();
			if is_circuit {
				for _ in 0..comp.borrow().get_generic().sub_compute_cycles {
					comp.borrow().get_circuit().compute_immutable(&ancestors);
				}
			}
			else {
				comp.borrow_mut().compute(&ancestors);
			}
		}
	}
	pub fn save_circuit(&self) -> Result<(), String> {
		// Convert components to enum variants to be serialized
		let mut components_save = HashMap::<u64, EnumAllLogicDevices>::new();
		for (ref_, component) in self.components.borrow().iter() {
			components_save.insert(*ref_, component.borrow().save()?);
		}
		// Un-RefCell Wires
		let mut wires_save = HashMap::<u64, Wire>::new();
		for (ref_, wire) in self.wires.borrow().iter() {
			wires_save.insert(*ref_, wire.borrow().clone());
		}
		// First, actually save this circuit
		let save = LogicCircuitSave {
			generic_device: self.generic_device.clone(),
			components: components_save,
			nets: hashmap_unwrap_refcells(self.nets.borrow().clone()),
			wires: wires_save,
			block_pin_positions: self.block_pin_positions.clone()
		};
		let raw_string: String = to_string_err(serde_json::to_string(&save))?;
		to_string_err(fs::write(resource_interface::get_circuit_file_path(&self.save_path), &raw_string))?;
		Ok(())
	}
}

impl LogicDevice for LogicCircuit {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic_device
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic_device
	}
	fn compute_step(&mut self, ancestors_above: &AncestryStack) {
		self.compute_immutable(ancestors_above);
	}
	/// Returns handle to file for inclusion in other circuits
	/// The actual save is done with `LogicCircuit::save_circuit()`
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		// Path to save file
		Ok(EnumAllLogicDevices::SubCircuit(self.save_path.clone(), self.displayed_as_block, self.get_ui_data().position, self.get_ui_data().direction))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		if self.displayed_as_block {
			// Rectangle
			draw.draw_polyline(
				vec![
					V2::new(-self.generic_device.bounding_box.0.x, -self.generic_device.bounding_box.0.y),
					V2::new(self.generic_device.bounding_box.1.x, -self.generic_device.bounding_box.0.y),
					V2::new(self.generic_device.bounding_box.1.x, self.generic_device.bounding_box.1.y),
					V2::new(-self.generic_device.bounding_box.0.x, self.generic_device.bounding_box.1.y),
					V2::new(-self.generic_device.bounding_box.0.x, -self.generic_device.bounding_box.0.y)
				],
				draw.styles.color_foreground
			);
			// Pins at alternate locations
			// TODO
			// Name
			// TODO
		}
		else {
			// Draws the circuit with wires and everything how you would expect
			for ref_ in self.get_all_graphics_references() {
				if let GraphicSelectableItemRef::Pin(_) = ref_ {
					if !self.is_toplevel {
						continue;
					}
				}
				self.run_function_on_graphic_item(ref_, |graphic_item| graphic_item.draw(draw));
			}
		}
	}
	fn get_circuit(&self) -> &Self {
		&self
	}
	fn get_circuit_mut(&mut self) -> &mut Self {
		self
	}
	fn is_toplevel_circuit(&self) -> bool {
		self.is_toplevel
	}
	fn is_circuit(&self) -> bool {
		true
	}
}