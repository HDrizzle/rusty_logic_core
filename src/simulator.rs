//! Heavily based off of the logic simulation I wrote in TS for use w/ MotionCanvas, found at https://github.com/HDrizzle/stack_machine/blob/main/presentation/src/logic_sim.tsx

use std::{fmt::Debug, fs};
use serde::{Deserialize, Serialize};
use crate::{prelude::*, resource_interface};
use resource_interface::LogicCircuitSave;

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
	ComponentInternal(ComponentPinReference)
}

impl LogicDriveSource {
	/// Basic component or global interface, cannot resolve any deeper
	pub fn is_final_source(&self) -> bool {
		match &self {
			Self::Global => true,
			Self::ExternalConnection(_) => false,
			Self::Net(_) => false,
			Self::ComponentInternal(_) => true
		}
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GlobalSourceReference {
	/// Pin of top level circuit, which always takes its inputs from Global
	Global(GenericQuery<LogicConnectionPin>),
	/// Output from basic logic component
	/// 0. Vec of strings, each one a sub-circuit of the last
	/// 1. Component reference within the previously given circuit
	ComponentInternal(SubCircuitPath, ComponentPinReference)
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
					CircuitWidePinReference::ComponentPin(component_pin_ref) => match circuit.components.get_item_tuple(&component_pin_ref.component_query.into_another_type()) {
						Some((_, component)) => match component.query_pin(&component_pin_ref.pin_query) {
							Some(pin) => {
								// Check what the internal source is
								match &pin.internal_source {
									// Check if pin is internally driven (by a sub-circuit)
									LogicConnectionPinInternalSource::Net(child_circuit_net_query) => {
										let circuit = component.get_circuit();
										match circuit.nets.get_item_tuple(child_circuit_net_query) {
											Some((child_net_ref, child_net)) => out.append(&mut child_net.resolve_sources(&self_ancestors.push(circuit), child_net_ref.id, &new_caller_history)),
											None => panic!("Internal connection in circuit \"{}\" references net {:?} inside sub-circuit \"{}\", the net does not exist", circuit.get_generic().unique_name, &child_circuit_net_query, component.get_generic().unique_name)
										}
									},
									// Check if pin is driven by a regular component
									LogicConnectionPinInternalSource::ComponentInternal => {
										out.push((GlobalSourceReference::ComponentInternal(self_ancestors.to_sub_circuit_path(), component_pin_ref.clone()), pin.internal_state));
									}
								}
							},
							None => panic!("Net references internal pin {:?} on component \"{}\" circuit \"{}\", which doesn't exist on that component", &component_pin_ref.pin_query, component.get_generic().unique_name, circuit.get_generic().unique_name)
						},
						None => panic!("Net references internal pin on component {:?} circuit \"{}\", which doesn't exist in the circuit", &component_pin_ref.component_query, circuit.get_generic().unique_name)
					},
					CircuitWidePinReference::ExternalConnection(ext_conn_query) => match circuit.query_pin(&ext_conn_query) {
						Some(pin) => {
							// Check what the external source is
							match &pin.external_source {
								// Check if external pin is connected to net on other side
								LogicConnectionPinExternalSource::Net(parent_circuit_net_query) => {
									// External pin is connected to a net in a circuit that contains the circuit that this net is a part of
									// Check that this net's "grandparent" is a circuit and not toplevel
									match self_ancestors.grandparent() {
										Some(parent_circuit) => match parent_circuit.nets.get_item_tuple(parent_circuit_net_query) {
											Some((parent_net_ref, parent_circuit_net)) => out.append(&mut parent_circuit_net.resolve_sources(&self_ancestors.trim(), parent_net_ref.id, &new_caller_history)),
											None => panic!("External connection is referencing a net ({:?}) which does not exist in this circuit's parent", &parent_circuit_net_query)
										},
										None => panic!("External connection is connected to a net, but the parent of this circuit is toplevel, this shouldn't happen")
									}
								}
								// Check if it is connected to a global pin
								LogicConnectionPinExternalSource::Global => {
									out.push((GlobalSourceReference::Global(ext_conn_query.clone()), pin.external_state));
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
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub enum LogicConnectionPinExternalSource {
	Net(GenericQuery<LogicNet>),
	#[default]
	Global
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub enum LogicConnectionPinInternalSource {
	Net(GenericQuery<LogicNet>),
	#[default]
	ComponentInternal
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LogicConnectionPin {
	internal_source: Option<LogicConnectionPinInternalSource>,
	internal_state: LogicState,
	external_source: Option<LogicConnectionPinExternalSource>,
	external_state: LogicState,
	relative_end_grid: IntV2,
	direction: FourWayDir,
	/// Usually 1, may be something else if theres a curve on an OR input or something
	length: f32,
	name: String
}

impl LogicConnectionPin {
	pub fn new(
		relative_end_grid: IntV2,
		direction: FourWayDir,
		length: f32,
		name: &str
	) -> Self {
		Self {
			internal_source: None,
			internal_state: LogicState::Floating,
			external_source: None,
			external_state: LogicState::Floating,
			relative_end_grid,
			direction,
			length,
			name: name.to_string()
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

/// Everything within a circuit
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CircuitWidePinReference {
	ComponentPin(ComponentPinReference),
	ExternalConnection(GenericQuery<LogicConnectionPin>)
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
	component_query: GenericQuery<()>,
	pin_query: GenericQuery<LogicConnectionPin>
}

impl ComponentPinReference {
	pub fn new(component_query: GenericQuery<()>, pin_query: GenericQuery<LogicConnectionPin>) -> Self {
		Self {
			component_query,
			pin_query
		}
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Wire {
	segments: Vec<(V2, V2)>,
	net: GenericQuery<LogicNet>,
	state: LogicState
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogicDeviceGeneric {
	pub pins: GenericDataset<LogicConnectionPin>,
	position_grid: IntV2,
	unique_name: String,
	sub_compute_cycles: usize,
	rotation: FourWayDir
}

impl LogicDeviceGeneric {
	pub fn new(
		pins: GenericDataset<LogicConnectionPin>,
		position_grid: IntV2,
		unique_name: String,
		sub_compute_cycles: usize,
		rotation: FourWayDir
	) -> Result<Self, String> {
		if sub_compute_cycles == 0 {
			return Err("Sub-compute cycles cannot be 0".to_string());
		}
		Ok(Self {
			pins,
			position_grid,
			unique_name,
			sub_compute_cycles,
			rotation
		})
	}
}

/// Could be a simple gate, or something more complicated like an adder, or maybe even the whole computer
pub trait LogicDevice: Debug {
	// TODO: Add a draw method
	fn get_generic(&self) -> &LogicDeviceGeneric;
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric;
	fn compute_step(&mut self, ancestors: &AncestryStack);
	fn save(&self) -> Result<EnumAllLogicDevicesSave, String>;
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
		pin_query: &GenericQuery<LogicConnectionPin>,
		state: LogicState
	) -> Result<(), String> {
		let generic: &mut LogicDeviceGeneric = self.get_generic_mut();
		let pin: &mut LogicConnectionPin = generic.pins.get_item_mut(pin_query).expect(&format!("Pin query {:?} does not work on logic device \"{}\"", pin_query, generic.unique_name));
		pin.set_drive_external(state);
		Ok(())
	}
	fn query_pin(&self, pin_query: &GenericQuery<LogicConnectionPin>) -> Option<&LogicConnectionPin> {
		let generic = self.get_generic();
		match generic.pins.get_item_tuple(pin_query) {
			Some(t) => Some(t.1),
			None => None
		}
	}
	fn query_pin_mut(&mut self, pin_query: &GenericQuery<LogicConnectionPin>) -> Option<&mut LogicConnectionPin> {
		let generic = self.get_generic_mut();
		match generic.pins.get_item_mut(pin_query) {
			Some(pin) => Some(pin),
			None => None
		}
	}
	fn set_all_pin_states(&mut self, states: Vec<(GenericQuery<LogicConnectionPin>, LogicState, LogicDriveSource)>) -> Result<(), String> {
		for (pin_query, state, _source) in states {
			self.set_pin_external_state(&pin_query, state)?;
		}
		Ok(())
	}
	fn get_pin_state_panic(&self, pin_query: &GenericQuery<LogicConnectionPin>) -> LogicState {
		self.query_pin(pin_query).expect(&format!("Pin query {:?} for logic device \"{}\" not valid", &pin_query, &self.get_generic().unique_name)).state()
	}
	fn set_pin_internal_state_panic(&mut self, pin_query: &GenericQuery<LogicConnectionPin>, state: LogicState) {
		self.query_pin_mut(pin_query).expect(&format!("Pin query {:?} not valid", &pin_query)).internal_state = state;
	}
}

#[derive(Clone)]
pub struct AncestryStack<'a>(Vec<&'a LogicCircuit>);

impl<'a> AncestryStack<'a> {
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

#[derive(Debug)]
pub struct LogicCircuit {
	generic_device: LogicDeviceGeneric,
	components: GenericDataset<Box<dyn LogicDevice>>,
	nets: GenericDataset<LogicNet>,
	wires: GenericDataset<Wire>,
	save_path: String
}

impl LogicCircuit {
	pub fn new(
		components: GenericDataset<Box<dyn LogicDevice>>,
		external_connections: GenericDataset<LogicConnectionPin>,
		nets: GenericDataset<LogicNet>,
		position_grid: IntV2,
		unique_name: String,
		sub_compute_cycles: usize,
		wires: GenericDataset<Wire>,
		save_path: String
	) -> Result<Self, String> {
		Ok(Self {
			generic_device: LogicDeviceGeneric::new(
				external_connections,
				position_grid,
				unique_name,
				sub_compute_cycles,
				FourWayDir::E
			)?,
			components,
			nets,
			wires,
			save_path
		})
	}
	pub fn from_save(save: LogicCircuitSave, save_path: String) -> Result<Self, String> {
		let mut components = GenericDataset::<Box<dyn LogicDevice>>::new();
		// Init compnents
		for (ref_, save_comp) in save.components.items {
			components.items.push((ref_.into_another_type(), EnumAllLogicDevicesSave::to_dynamic(save_comp)?));
		}
		Ok(Self {
			generic_device: save.generic_device,
			components,
			nets: save.nets,
			wires: save.wires,
			save_path
		})
	}
	pub fn resolve_circuit_pin_ref(&self, ref_: &CircuitWidePinReference) -> Option<&LogicConnectionPin> {
		match ref_ {
			CircuitWidePinReference::ComponentPin(component_pin_ref) => match self.components.get_item_tuple(&component_pin_ref.component_query.into_another_type()) {
				Some((_, component)) => component.query_pin(&component_pin_ref.pin_query),
				None => None
			},
			CircuitWidePinReference::ExternalConnection(ext_conn_query) => match self.generic_device.pins.get_item_tuple(&ext_conn_query) {
				Some(t) => Some(t.1),
				None => None
			}
		}
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
		let ancestors = ancestors_above.push(&self);
		// Update net states
		let mut new_net_states: Vec<(LogicState, Vec<GlobalSourceReference>)> = Vec::new();
		for (net_ref, net) in &self.nets.items {
			new_net_states.push(net.update_state(&ancestors, net_ref.id));
		}
		for (i, (_, net)) in &mut self.nets.items.iter_mut().enumerate() {
			net.state = new_net_states[i].0;
			net.sources = new_net_states[i].1.clone();
		}
		// Update pin & wire states from nets
		for (wire_ref, wire) in &mut self.wires.items {
			wire.state = self.nets.get_item_tuple(&wire.net).expect(&format!("Wire {:?} has invalid net query {:?}", wire_ref, &wire.net)).1.state;
		}
		for (pin_ref, pin) in &mut self.generic_device.pins.items {
			match &pin.internal_source {
				LogicConnectionPinInternalSource::Net(net_query) => pin.set_drive_internal(self.nets.get_item_tuple(net_query).expect(&format!("External connection pin {:?} has invalid net query {:?}", pin_ref, net_query)).1.state),
				LogicConnectionPinInternalSource::ComponentInternal => panic!("External connection pin {:?} for circuit \"{}\" has the internal source as ComponentInternal which shouldn't happen", pin_ref, &self.generic_device.unique_name)
			}
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevicesSave, String> {
		// Convert components to enum variants to be serialized
		let mut components_save = GenericDataset::<EnumAllLogicDevicesSave>::new();
		for (ref_, component) in &self.components.items {
			components_save.items.push((ref_.into_another_type(), component.save()?));
		}
		// First, actually save this circuit
		let save = LogicCircuitSave {
			generic_device: self.generic_device.clone(),
			components: components_save,
			nets: self.nets.clone(),
			wires: self.wires.clone()
		};
		let raw_string: String = to_string_err(serde_json::to_string(&save))?;
		to_string_err(fs::write(resource_interface::get_circuit_file_path(&self.save_path), &raw_string))?;
		// Path to save file
		Ok(EnumAllLogicDevicesSave::SubCircuit(self.save_path.clone()))
	}
	fn get_circuit(&self) -> &Self {
		&self
	}
	fn get_circuit_mut(&mut self) -> &mut Self {
		self
	}
}