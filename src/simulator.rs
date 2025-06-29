//! Heavily based off of the logic simulation I wrote in TS for use w/ MotionCanvas, found at https://github.com/HDrizzle/stack_machine/blob/main/presentation/src/logic_sim.tsx

use std::{cell::RefCell, collections::{HashMap, HashSet}, default::Default, fmt::Debug, fs, ops::{Deref, DerefMut}, time::{Duration, Instant}};
use serde::{Deserialize, Serialize};
use crate::{prelude::*, resource_interface};
use resource_interface::LogicCircuitSave;
use eframe::{egui::{self, response::Response, Key, KeyboardShortcut, Modifiers}, glow::FALSE};

fn logic_device_to_graphic_item(x: &dyn LogicDevice) -> &dyn GraphicSelectableItem {
	x
}

fn pin_to_graphic_item(x: &LogicConnectionPin) -> &dyn GraphicSelectableItem {
	x
}

fn wire_to_graphic_item(x: &Wire) -> &dyn GraphicSelectableItem {
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
					CircuitWidePinReference::ComponentPin(component_pin_ref) => match circuit.components.get(&component_pin_ref.component_id) {
						Some(component_cell) => match component_cell.borrow().query_pin(&component_pin_ref.pin_id) {
							Some(pin) => {
								// Check what the internal source is
								if let Some(source) = &pin.internal_source {
									match source {
										// Check if pin is internally driven (by a sub-circuit)
										LogicConnectionPinInternalSource::Net(child_circuit_net_id) => {
											let component = component_cell.borrow();
											let circuit = component.get_circuit();
											match circuit.nets.get(child_circuit_net_id) {
												Some(child_net) => out.append(&mut child_net.resolve_sources(&self_ancestors.push(circuit), *child_circuit_net_id, &new_caller_history)),
												None => panic!("Internal connection in circuit \"{}\" references net {:?} inside sub-circuit \"{}\", the net does not exist", circuit.get_generic().unique_name, &child_circuit_net_id, component_cell.borrow().get_generic().unique_name)
											}
										},
										// Check if pin is driven by a regular component
										LogicConnectionPinInternalSource::ComponentInternal => {
											out.push((GlobalSourceReference::ComponentInternal(self_ancestors.to_sub_circuit_path(), component_pin_ref.clone()), pin.internal_state));
										}
									}
								}
							},
							None => panic!("Net references internal pin {} on component \"{}\" circuit \"{}\", which doesn't exist on that component", component_pin_ref.pin_id, component_cell.borrow().get_generic().unique_name, circuit.get_generic().unique_name)
						},
						None => panic!("Net references internal pin on component {} circuit \"{}\", which doesn't exist in the circuit", component_pin_ref.component_id, circuit.get_generic().unique_name)
					},
					CircuitWidePinReference::ExternalConnection(ext_conn_id) => match circuit.query_pin(&ext_conn_id) {
						Some(pin) => {
							// Check what the external source is
							if let Some(source) = &pin.external_source {
								match source {
									// Check if external pin is connected to net on other side
									LogicConnectionPinExternalSource::Net(parent_circuit_net_id) => {
										// External pin is connected to a net in a circuit that contains the circuit that this net is a part of
										// Check that this net's "grandparent" is a circuit and not toplevel
										match self_ancestors.grandparent() {
											Some(parent_circuit) => match parent_circuit.nets.get(parent_circuit_net_id) {
												Some(parent_circuit_net) => out.append(&mut parent_circuit_net.resolve_sources(&self_ancestors.trim(), *parent_circuit_net_id, &new_caller_history)),
												None => panic!("External connection is referencing a net ({}) which does not exist in this circuit's parent", parent_circuit_net_id)
											},
											None => panic!("External connection is connected to a net, but the parent of this circuit is toplevel, this shouldn't happen")
										}
									}
									// Check if it is connected to a global pin
									LogicConnectionPinExternalSource::Global => {
										out.push((GlobalSourceReference::Global(ext_conn_id.to_owned()), pin.external_state));
									},
									LogicConnectionPinExternalSource::Clock => {
										out.push((GlobalSourceReference::Clock(self_ancestors.to_sub_circuit_path()), pin.external_state));
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
	pub fn add_component_connection_if_missing(&mut self, comp_id: u64, pin_id: &str) {
		for conn in &self.connections {
			match conn {
				CircuitWidePinReference::ExternalConnection(_) => {},
				CircuitWidePinReference::ComponentPin(test_comp_pin_ref) => {
					// If the connection already exists, return
					if test_comp_pin_ref.component_id == comp_id && test_comp_pin_ref.pin_id == pin_id {
						return;
					}
				}
			}
		}
		// Hasn't returned yet, add connection
		self.connections.push(CircuitWidePinReference::ComponentPin(ComponentPinReference::new(comp_id, pin_id.to_owned())));
	}
	pub fn add_external_connection_if_missing(&mut self, pin_id: &str) {
		for conn in &self.connections {
			match conn {
				CircuitWidePinReference::ExternalConnection(test_pin_id) => {
					// If the connection already exists, return
					if test_pin_id == pin_id {
						return;
					}
				},
				CircuitWidePinReference::ComponentPin(_) => {}
			}
		}
		// Hasn't returned yet, add connection
		self.connections.push(CircuitWidePinReference::ExternalConnection(pin_id.to_owned()));
	}
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
pub enum LogicConnectionPinExternalSource {
	Net(u64),
	#[default]
	Global,
	Clock
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
pub enum LogicConnectionPinInternalSource {
	Net(u64),
	#[default]
	ComponentInternal
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LogicConnectionPin {
	pub internal_source: Option<LogicConnectionPinInternalSource>,
	internal_state: LogicState,
	pub external_source: Option<LogicConnectionPinExternalSource>,
	external_state: LogicState,
	pub relative_end_grid: IntV2,
	pub direction: FourWayDir,
	/// Usually 1, may be something else if theres a curve on an OR input or something
	pub length: f32,
	selected: bool,
	pub position_before_dragging: IntV2
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
			relative_end_grid,
			direction,
			length,
			selected: false,
			position_before_dragging: relative_end_grid
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
	fn draw<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(
			vec![
				V2::new(-0.9, -0.9),
				V2::new(-0.9, 0.9),
				V2::new(0.9, 0.9),
				V2::new(0.9, -0.9),
				V2::new(-0.9, -0.9)
			].iter().map(|p| p + self.relative_end_grid.to_v2() + self.direction.to_unit()).collect(),
			draw.styles.color_from_logic_state(self.state())
		);
		if let Some(ext_source) = &self.external_source {
			if let LogicConnectionPinExternalSource::Clock = ext_source {
				let clk_scale = 0.7;
				draw.draw_polyline(
					vec![
						V2::new(-clk_scale, 0.0),
						V2::new(-clk_scale, clk_scale),
						V2::new(0.0, clk_scale),
						V2::new(0.0, -clk_scale),
						V2::new(clk_scale, -clk_scale),
						V2::new(clk_scale, 0.0)
					].iter().map(|p| p + self.relative_end_grid.to_v2() + self.direction.to_unit()).collect(),
					draw.styles.color_foreground
				);
			}
		}
	}
	fn get_selected(&self) -> bool {
		self.selected
	}
	fn set_selected(&mut self, selected: bool) {
		self.selected = selected;
	}
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let half_diagonal = V2::new(0.9, 0.9);
		let box_center = self.direction.to_unit() + self.relative_end_grid.to_v2();
		let global_offset = grid_offset + box_center;
		(global_offset - half_diagonal, global_offset + half_diagonal)
	}
	fn dragging_to(&mut self, current_pos: V2) {
		self.relative_end_grid = round_v2_to_intv2(current_pos);
	}
	fn start_dragging(&mut self) {
		self.position_before_dragging = self.relative_end_grid;
	}
	fn stop_dragging(&mut self, final_pos: V2) {
		self.relative_end_grid = round_v2_to_intv2(final_pos);
	}
	fn get_position_before_dragging(&self) -> IntV2 {
		self.position_before_dragging
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
}

/// Everything within a circuit
#[derive(Clone, Debug, Serialize, Deserialize)]
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

#[derive(Clone, Debug, Serialize, Deserialize)]
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

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Wire {
	start_pos: IntV2,
	length: u32,
	direction: FourWayDir,
	net: u64,
	state: LogicState,
	start_connection: Option<WireConnection>,
	end_connection: Option<WireConnection>,
	start_selected: bool,
	end_selected: bool,
	dragging_another_wire: Option<(IntV2, FourWayDir, u32)>,
	position_before_dragging: IntV2
}

impl GraphicSelectableItem for Wire {
	fn draw<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(
			vec![
				self.start_pos.to_v2(),
				self.start_pos.to_v2() + (self.direction.to_unit() * (self.length as f32))
			],
			draw.styles.color_from_logic_state(self.state)
		);
	}
	fn get_selected(&self) -> bool {
		self.start_selected || self.end_selected
	}
	fn set_selected(&mut self, selected: bool) {
		self.start_selected = selected;
		self.end_selected = selected;
	}
	/// Excludes end BBs which are special and for dragging the ends around or extruding at right angles
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let local_bb: (V2, V2) = match self.direction {
			FourWayDir::E => (V2::new(0.5, -0.25), V2::new(self.length as f32 - 0.5, 0.25)),
			FourWayDir::N => (V2::new(-0.25, 0.5 - (self.length as f32)), V2::new(0.25, -0.5)),
			FourWayDir::W => (V2::new(self.length as f32 - 0.5, -0.25), V2::new(0.5, 0.25)),
			FourWayDir::S => (V2::new(-0.25, -0.5), V2::new(0.25, 0.5 - (self.length as f32)))
		};
		(local_bb.0 + grid_offset, local_bb.1 + grid_offset)
	}
	fn dragging_to(&mut self, current_pos: V2) {
		self.start_pos = round_v2_to_intv2(current_pos);
	}
	fn start_dragging(&mut self) {
		self.position_before_dragging = self.start_pos;
	}
	fn stop_dragging(&mut self, final_pos: V2) {
		self.start_pos = round_v2_to_intv2(final_pos);
	}
	fn get_position_before_dragging(&self) -> IntV2 {
		self.position_before_dragging
	}
	fn is_connected_to_net(&self, net_id: u64) -> bool {
		net_id == self.net
	}
}

/// What is the end of a wire connected to?
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WireConnection {
	/// Component or external pin
	Pin(CircuitWidePinReference),
	/// "Dor", 3 or 4 wires
	WireJoint(GenericQuery<WireJoint>),
	/// "Elbow" joint, no dot required
	Wire(GenericQuery<Wire>)
}

/// "Dot", connecting 2 or 4 wires
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct WireJoint {
	wires: Vec<u64>,
	pos: IntV2,
	net: u64,
	state: LogicState
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogicDeviceGeneric {
	pub pins: HashMap<String, LogicConnectionPin>,
	pub position_grid: IntV2,
	pub position_before_dragging: IntV2,
	pub unique_name: String,
	pub sub_compute_cycles: usize,
	pub rotation: FourWayDir,
	pub selected: bool,
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
			pins,
			position_grid,
			position_before_dragging: position_grid,
			unique_name,
			sub_compute_cycles,
			rotation,
			selected: false,
			bounding_box
		})
	}
}

/// Could be a simple gate, or something more complicated like an adder, or maybe even the whole computer
pub trait LogicDevice: Debug + GraphicSelectableItem where Self: 'static {
	fn get_generic(&self) -> &LogicDeviceGeneric;
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric;
	fn compute_step(&mut self, ancestors: &AncestryStack);
	fn save(&self) -> Result<EnumAllLogicDevicesSave, String>;
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>);
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
		let generic: &mut LogicDeviceGeneric = self.get_generic_mut();
		let pin: &mut LogicConnectionPin = generic.pins.get_mut(pin_id).expect(&format!("Pin ID {} does not work on logic device \"{}\"", pin_id, generic.unique_name));
		pin.set_drive_external(state);
		Ok(())
	}
	fn query_pin(&self, pin_id: &str) -> Option<&LogicConnectionPin> {
		let generic = self.get_generic();
		generic.pins.get(pin_id)
	}
	fn query_pin_mut(&mut self, pin_id: &str) -> Option<&mut LogicConnectionPin> {
		let generic = self.get_generic_mut();
		generic.pins.get_mut(pin_id)
	}
	fn set_all_pin_states(&mut self, states: Vec<(&str, LogicState, LogicDriveSource)>) -> Result<(), String> {
		for (pin_query, state, _source) in states {
			self.set_pin_external_state(&pin_query, state)?;
		}
		Ok(())
	}
	fn get_pin_state_panic(&self, pin_query: &str) -> LogicState {
		self.query_pin(pin_query).expect(&format!("Pin query {:?} for logic device \"{}\" not valid", &pin_query, &self.get_generic().unique_name)).state()
	}
	fn set_pin_internal_state_panic(&mut self, pin_query: &str, state: LogicState) {
		self.query_pin_mut(pin_query).expect(&format!("Pin query {:?} not valid", &pin_query)).internal_state = state;
	}
	fn into_box(self: Box<Self>) -> Box<dyn LogicDevice> where Self: Sized {
		self as Box<dyn LogicDevice>
	}
}

/// Everything that implements `Component` also automatically works with the graphics
impl<T: LogicDevice> GraphicSelectableItem for T {
	fn draw<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		for (_, pin) in self.get_generic().pins.iter() {
			draw.draw_polyline(
				vec![
					pin.relative_end_grid.to_v2(),
					pin.relative_end_grid.to_v2() - (pin.direction.to_unit() * pin.length)
				],
				draw.styles.color_from_logic_state(pin.state())
			);
		}
		self.draw_except_pins(&draw);
	}
	fn get_selected(&self) -> bool {
		self.get_generic().selected
	}
	fn set_selected(&mut self, selected: bool) {
		self.get_generic_mut().selected = selected;
	}
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let local_bb = self.get_generic().bounding_box;
		(local_bb.0 + grid_offset, local_bb.1 + grid_offset)
	}
	fn dragging_to(&mut self, current_pos: V2) {
		self.get_generic_mut().position_grid = round_v2_to_intv2(current_pos);
	}
	fn start_dragging(&mut self) {
		self.get_generic_mut().position_before_dragging = self.get_generic().position_grid;
	}
	fn stop_dragging(&mut self, final_pos: V2) {
		self.get_generic_mut().position_grid = round_v2_to_intv2(final_pos);
	}
	fn get_position_before_dragging(&self) -> IntV2 {
		self.get_generic().position_before_dragging
	}
	fn is_connected_to_net(&self, _net_id: u64) -> bool {
		false// Don't highlight a whole component, only wires and pins
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
	FollowingMouse
}

#[derive(Debug, Clone)]
pub enum Tool {
	Select {
		selected_graphics: HashSet<GraphicSelectableItemRef>,
		selected_graphics_state: SelectionState
	},
	HighlightNet(Option<u64>),
	PlaceComponnet,
	PlaceWire,
	PlacePin
}

impl Default for Tool {
	fn default() -> Self {
		Self::Select{selected_graphics: HashSet::new(), selected_graphics_state: SelectionState::default()}
	}
}

#[derive(Debug)]
pub struct LogicCircuit {
	pub generic_device: LogicDeviceGeneric,
	pub components: HashMap<u64, RefCell<Box<dyn LogicDevice>>>,
	pub nets: HashMap<u64, LogicNet>,
	pub wires: HashMap<u64, Wire>,
	save_path: String,
	/// The clock is only meant to be used in the toplevel circuit, if you want to use the same clock everywhere, just have an input pin dedicated to it for all sub-circuits.
	/// Alternatively, a sub-circuit can have its own internal clock which would be different from the outside clock
	pub clock_enabled: bool,
	pub clock_state: bool,
	pub clock_freq: f32,
	clock_last_change: Instant,
	/// Inspired by CircuitVerse, block-diagram version of circuit
	/// {pin ID: (relative position (ending), direction)}
	block_pin_positions: HashMap<String, (IntV2, FourWayDir)>,
	displayed_as_block: bool,
	/// For UI
	tool: RefCell<Tool>
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
		displayed_as_block: bool
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
			components,
			nets,
			wires,
			save_path,
			clock_enabled,
			clock_state,
			clock_freq,
			clock_last_change: Instant::now(),
			block_pin_positions: HashMap::new(),
			displayed_as_block,
			tool: RefCell::new(Tool::default())
		};
		new.recompute_default_layout();
		new.recompute_connections();
		Ok(new)
	}
	pub fn from_save(save: LogicCircuitSave, save_path: String, displayed_as_block: bool) -> Result<Self, String> {
		let mut components = HashMap::<u64, RefCell<Box<dyn LogicDevice>>>::new();
		// Init compnents
		for (ref_, save_comp) in save.components.into_iter() {
			components.insert(ref_, RefCell::new(EnumAllLogicDevicesSave::to_dynamic(save_comp)?));
		}
		Ok(Self {
			generic_device: save.generic_device,
			components,
			nets: save.nets,
			wires: save.wires,
			save_path,
			clock_enabled: save.clock_enabled,
			clock_state: save.clock_state,
			clock_freq: save.clock_freq,
			clock_last_change: Instant::now(),
			block_pin_positions: save.block_pin_positions,
			displayed_as_block,
			tool: RefCell::new(Tool::default())
		})
	}
	fn recompute_default_layout(&mut self) {
		// Bounding box & layout, like in CircuitVerse
		let mut count_pins_not_clock: i32 = 0;
		let mut block_pin_positions: HashMap<String, (IntV2, FourWayDir)> = HashMap::new();
		for (pin_ref, pin) in self.generic_device.pins.iter() {
			if let Some(ext_source) = &pin.external_source {
				if let LogicConnectionPinExternalSource::Clock = ext_source {
					continue;
				}
			}
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
	/*pub fn resolve_circuit_pin_ref(&self, ref_: &CircuitWidePinReference) -> Option<&LogicConnectionPin> {
		match ref_ {
			CircuitWidePinReference::ComponentPin(component_pin_ref) => match self.components.get_item_tuple(&component_pin_ref.component_query.into_another_type()) {
				Some((_, component)) => {
					let component_uncelled = component.borrow();
					component_uncelled.query_pin(&component_pin_ref.pin_query)
				},
				None => None
			},
			CircuitWidePinReference::ExternalConnection(ext_conn_query) => match self.generic_device.pins.get_item_tuple(&ext_conn_query) {
				Some(t) => Some(t.1),
				None => None
			}
		}
	}
	pub fn resolve_circuit_pin_ref_mut(&mut self, ref_: &CircuitWidePinReference) -> Option<&mut LogicConnectionPin> {
		match ref_ {
			CircuitWidePinReference::ComponentPin(component_pin_ref) => match self.components.get_item_mut(&component_pin_ref.component_query.into_another_type()) {
				Some(component) => component.query_pin_mut(&component_pin_ref.pin_query),
				None => None
			},
			CircuitWidePinReference::ExternalConnection(ext_conn_query) => self.generic_device.pins.get_item_mut(&ext_conn_query)
		}
	}*/
	/// Nets reference connections to pins and pins may reference nets, this function makes sure each "connection" is either connected both ways or deleted if one end is missing
	pub fn recompute_connections(&mut self) {
		// Net -> Pin
		let mut net_to_pin_connections_to_drop = Vec::<(u64, Vec<usize>)>::new();// (net ID, index into net's connections vec)
		for (net_id, net) in self.nets.iter() {
			let mut this_net_to_pin_connections_to_drop = Vec::<usize>::new();
			for (conn_i, net_connection) in net.connections.iter().enumerate() {
				match net_connection {
					CircuitWidePinReference::ComponentPin(component_pin_ref) => match self.components.get_mut(&component_pin_ref.component_id) {
						Some(component) => match component.borrow_mut().query_pin_mut(&component_pin_ref.pin_id) {
							Some(pin) => {
								pin.external_source = Some(LogicConnectionPinExternalSource::Net(*net_id));
							}
							None => this_net_to_pin_connections_to_drop.push(conn_i),
						},
						None => this_net_to_pin_connections_to_drop.push(conn_i)
					},
					CircuitWidePinReference::ExternalConnection(ext_conn_query) => match self.generic_device.pins.get_mut(ext_conn_query) {
						Some(pin) => {
							pin.internal_source = Some(LogicConnectionPinInternalSource::Net(*net_id));
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
				self.nets.get_mut(&net_id).unwrap().connections.remove(*conn_i);
			}
		}
		// Component pin -> Net
		let mut pin_to_net_connections_to_drop = Vec::<CircuitWidePinReference>::new();
		for (comp_id, comp) in self.components.iter() {
			for (pin_id, pin) in comp.borrow().get_generic().pins.iter() {
				if let Some(source) = &pin.external_source {
					match source {
						LogicConnectionPinExternalSource::Global => panic!("Pin {:?} of component {:?} has external source 'Global' which doesn't make sense", &pin_id, &comp_id),
						LogicConnectionPinExternalSource::Net(net_id) => match self.nets.get_mut(net_id) {
							Some(net) => net.add_component_connection_if_missing(*comp_id, pin_id),
							None => pin_to_net_connections_to_drop.push(CircuitWidePinReference::ComponentPin(ComponentPinReference::new(*comp_id, pin_id.to_owned()))),
						},
						LogicConnectionPinExternalSource::Clock => {}
					}
				}
			}
		}
		// External pin -> Net
		for (pin_id, pin) in self.generic_device.pins.iter() {
			if let Some(source) = &pin.internal_source {
				match source {
					LogicConnectionPinInternalSource::ComponentInternal => panic!("External connection pin {} of circuit {:?} has internal source 'ComponentInternal' which doesn't make sense", pin_id, &self.generic_device.unique_name),
					LogicConnectionPinInternalSource::Net(net_query) => match self.nets.get_mut(net_query) {
						Some(net) => net.add_external_connection_if_missing(pin_id),
						None => pin_to_net_connections_to_drop.push(CircuitWidePinReference::ExternalConnection(pin_id.to_owned())),
					}
				}
			}
		}
		// Remove broken Pin -> Net connections
		for pin_ref in pin_to_net_connections_to_drop {
			match pin_ref {
				CircuitWidePinReference::ComponentPin(component_pin_ref) => {
					self.components.get_mut(&component_pin_ref.component_id).expect(&format!("Component ID {} from net connections to delete is invalid", component_pin_ref.component_id)).borrow_mut().query_pin_mut(&component_pin_ref.pin_id).expect(&format!("Pin ID {} for component {} from net connections to delete is invalid", component_pin_ref.pin_id, component_pin_ref.component_id)).external_source = None;
				},
				CircuitWidePinReference::ExternalConnection(ext_conn_query) => {
					self.generic_device.pins.get_mut(&ext_conn_query).expect(&format!("External connection ID {} from net connections to delete is invalid", &ext_conn_query)).internal_source = None;
				}
			}
		}
	}
	pub fn toplevel_ui_interact<'a>(&mut self, response: Response, draw: &ComponentDrawInfo<'a>, mut input_state: egui::InputState) {
		match self.tool.borrow_mut().deref_mut() {
			Tool::Select{selected_graphics, selected_graphics_state} => {
				match selected_graphics_state {
					SelectionState::Fixed => {
						if response.dragged() {
							*selected_graphics_state = SelectionState::Dragging(emath_pos2_to_v2(response.hover_pos().expect("Hover pos should work when dragging")) / draw.grid_size, emath_vec2_to_v2(response.drag_delta()) / draw.grid_size);
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
									false => {selected_graphics.insert(new_selected_item);}
								},
								None => {
									if !multi_select_key {
										*selected_graphics = HashSet::new();
									}
								}
							}
						}
						// Cmd-A for select-all
						if input_state.consume_shortcut(&KeyboardShortcut::new(Modifiers::COMMAND, Key::A)) {
							*selected_graphics = HashSet::from_iter(self.get_all_graphics_references());
						}
						
					},
					SelectionState::Dragging(start_grid, delta_grid) => {
						// TODO
					},
					SelectionState::FollowingMouse => {
						// TODO
					}
				}
				// Copy
				if input_state.consume_shortcut(&KeyboardShortcut::new(Modifiers::COMMAND, Key::C)) {
					// TODO
				}
				// Delete
				if input_state.consume_key(Modifiers::NONE, Key::Delete) {

				}
			},
			Tool::HighlightNet(net_id) => {
				// TODO
			},
			Tool::PlaceComponnet => {
				// TODO
			},
			Tool::PlaceWire => {
				// TODO
			},
			Tool::PlacePin => {
				// TODO
			}
		}
		
	}
	fn was_anything_clicked<'a>(&self, grid_pos: V2) -> Option<GraphicSelectableItemRef> {
		None// TODO
	}
	fn remove_graphic_item(&mut self, ref_: GraphicSelectableItemRef) {
		// TODO
	}
	fn run_function_on_graphic_item<T>(&self, ref_: GraphicSelectableItemRef, mut func: impl FnMut(Box<&dyn GraphicSelectableItem>) -> T) -> T {
		let error_msg = format!("Graphic item reference {:?} cannot be found", &ref_);
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => func(Box::new(logic_device_to_graphic_item(self.components.get(&(comp_id.into())).expect(&error_msg).borrow().deref().as_ref()))),
			GraphicSelectableItemRef::Wire(wire_id) => func(Box::new(wire_to_graphic_item(&self.wires.get(&(wire_id.into())).expect(&error_msg)))),
			GraphicSelectableItemRef::Pin(pin_id) => func(Box::new(pin_to_graphic_item(&self.generic_device.pins.get(&pin_id).expect(&error_msg)))),
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
		for (ref_, _) in self.components.iter() {
			out.push(GraphicSelectableItemRef::Component(*ref_));
		}
		for (ref_, _) in self.wires.iter() {
			out.push(GraphicSelectableItemRef::Wire(*ref_));
		}
		for (ref_, _) in self.generic_device.pins.iter() {
			out.push(GraphicSelectableItemRef::Pin(ref_.clone()));
		}
		// TODO: Text boxes once I implement them
		out
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
		// Update clock
		if self.clock_last_change.elapsed() > Duration::from_secs_f32(0.5 / self.clock_freq) {// The frequency is based on a whole period, it must change twice per period, so 0.5/f not 1/f
			self.clock_state = !self.clock_state;
			self.clock_last_change = Instant::now();
		}
		let ancestors = ancestors_above.push(&self);
		// Update net states
		let mut new_net_states = HashMap::<u64, (LogicState, Vec<GlobalSourceReference>)>::new();
		for (net_id, net) in self.nets.iter() {
			new_net_states.insert(*net_id, net.update_state(&ancestors, *net_id));
		}
		drop(ancestors);
		for (net_id, (state, sources)) in new_net_states.into_iter() {
			let net_mut_ref = self.nets.get_mut(&net_id).unwrap();
			net_mut_ref.state = state;
			net_mut_ref.sources = sources;
		}
		// Update pin & wire states from nets
		// Wires
		for (wire_id, wire) in self.wires.iter_mut() {
			wire.state = self.nets.get(&wire.net).expect(&format!("Wire {} has invalid net ID {}", wire_id, wire.net)).state;
		}
		// External connection pins
		for (pin_id, pin) in self.generic_device.pins.iter_mut() {
			if let Some(source) = &pin.internal_source {
				match source {
					LogicConnectionPinInternalSource::Net(net_id) => pin.set_drive_internal(self.nets.get(net_id).expect(&format!("External connection pin {} has invalid net query {}", pin_id, net_id)).state),
					LogicConnectionPinInternalSource::ComponentInternal => panic!("External connection pin {} for circuit \"{}\" has the internal source as ComponentInternal which shouldn't happen", pin_id, &self.generic_device.unique_name)
				}
			}
			if let Some(ext_source) = &pin.external_source {
				if let LogicConnectionPinExternalSource::Clock = ext_source {
					pin.external_state = self.clock_state.into();
				}
			}
		}
		// Component pins, and propagate through components
		for (comp_id, comp) in self.components.iter() {
			for (pin_id, pin) in comp.borrow_mut().get_generic_mut().pins.iter_mut() {
				if let Some(source) = &pin.external_source {
					match source {
						LogicConnectionPinExternalSource::Global => panic!("Pin {} of component {} has external source 'Global' which doesn't make sense", pin_id, comp_id),
						LogicConnectionPinExternalSource::Net(net_id) => {
							pin.external_state = self.nets.get_mut(&net_id).expect(&format!("Pin {} of component {} references net {} which doesn't exist", pin_id, comp_id, net_id)).state;
						},
						LogicConnectionPinExternalSource::Clock => {}// A clock-connected pin is handled by the sub-circuit itself
					}
				}
			}
		}
		// THIS GOES LAST, has to be seperate loop then before
		let ancestors = ancestors_above.push(&self);
		for (_, comp) in self.components.iter() {
			comp.borrow_mut().compute(&ancestors);
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevicesSave, String> {
		// Convert components to enum variants to be serialized
		let mut components_save = HashMap::<u64, EnumAllLogicDevicesSave>::new();
		for (ref_, component) in self.components.iter() {
			components_save.insert(*ref_, component.borrow().save()?);
		}
		// First, actually save this circuit
		let save = LogicCircuitSave {
			generic_device: self.generic_device.clone(),
			components: components_save,
			nets: self.nets.clone(),
			wires: self.wires.clone(),
			clock_enabled: self.clock_enabled,
			clock_state: self.clock_state,
			clock_freq: self.clock_freq,
			block_pin_positions: self.block_pin_positions.clone()
		};
		let raw_string: String = to_string_err(serde_json::to_string(&save))?;
		to_string_err(fs::write(resource_interface::get_circuit_file_path(&self.save_path), &raw_string))?;
		// Path to save file
		Ok(EnumAllLogicDevicesSave::SubCircuit(self.save_path.clone(), self.displayed_as_block))
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
}