//! Heavily based off of the logic simulation I wrote in TS for use w/ MotionCanvas, found at https://github.com/HDrizzle/stack_machine/blob/main/presentation/src/logic_sim.tsx

use std::{fmt::Debug, fs, cell::RefCell};
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
						Some((_, component_cell)) => match component_cell.borrow().query_pin(&component_pin_ref.pin_query) {
							Some(pin) => {
								// Check what the internal source is
								if let Some(source) = &pin.internal_source {
									match source {
										// Check if pin is internally driven (by a sub-circuit)
										LogicConnectionPinInternalSource::Net(child_circuit_net_query) => {
											let component = component_cell.borrow();
											let circuit = component.get_circuit();
											match circuit.nets.get_item_tuple(child_circuit_net_query) {
												Some((child_net_ref, child_net)) => out.append(&mut child_net.resolve_sources(&self_ancestors.push(circuit), child_net_ref.id, &new_caller_history)),
												None => panic!("Internal connection in circuit \"{}\" references net {:?} inside sub-circuit \"{}\", the net does not exist", circuit.get_generic().unique_name, &child_circuit_net_query, component_cell.borrow().get_generic().unique_name)
											}
										},
										// Check if pin is driven by a regular component
										LogicConnectionPinInternalSource::ComponentInternal => {
											out.push((GlobalSourceReference::ComponentInternal(self_ancestors.to_sub_circuit_path(), component_pin_ref.clone()), pin.internal_state));
										}
									}
								}
							},
							None => panic!("Net references internal pin {:?} on component \"{}\" circuit \"{}\", which doesn't exist on that component", &component_pin_ref.pin_query, component_cell.borrow().get_generic().unique_name, circuit.get_generic().unique_name)
						},
						None => panic!("Net references internal pin on component {:?} circuit \"{}\", which doesn't exist in the circuit", &component_pin_ref.component_query, circuit.get_generic().unique_name)
					},
					CircuitWidePinReference::ExternalConnection(ext_conn_query) => match circuit.query_pin(&ext_conn_query) {
						Some(pin) => {
							// Check what the external source is
							if let Some(source) = &pin.external_source {
								match source {
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
	pub fn add_component_connection_if_missing(&mut self, comp_ref: &GenericRef<RefCell<Box<dyn LogicDevice>>>, pin_ref: &GenericRef<LogicConnectionPin>) {
		for conn in &self.connections {
			match conn {
				CircuitWidePinReference::ExternalConnection(_) => {},
				CircuitWidePinReference::ComponentPin(test_comp_pin_ref) => {
					// If the connection already exists, return
					if comp_ref.query_matches(&test_comp_pin_ref.component_query.into_another_type()) && pin_ref.query_matches(&test_comp_pin_ref.pin_query) {
						return;
					}
				}
			}
		}
		// Hasn't returned yet, add connection
		self.connections.push(CircuitWidePinReference::ComponentPin(ComponentPinReference::new(comp_ref.to_query().into_another_type(), pin_ref.to_query())));
	}
	pub fn add_external_connection_if_missing(&mut self, pin_ref: &GenericRef<LogicConnectionPin>) {
		for conn in &self.connections {
			match conn {
				CircuitWidePinReference::ExternalConnection(test_pin_query) => {
					// If the connection already exists, return
					if pin_ref.query_matches(&test_pin_query) {
						return;
					}
				},
				CircuitWidePinReference::ComponentPin(_) => {}
			}
		}
		// Hasn't returned yet, add connection
		self.connections.push(CircuitWidePinReference::ExternalConnection(pin_ref.to_query()));
	}
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
pub enum LogicConnectionPinExternalSource {
	Net(GenericQuery<LogicNet>),
	#[default]
	Global
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
pub enum LogicConnectionPinInternalSource {
	Net(GenericQuery<LogicNet>),
	#[default]
	ComponentInternal
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LogicConnectionPin {
	pub internal_source: Option<LogicConnectionPinInternalSource>,
	internal_state: LogicState,
	pub external_source: Option<LogicConnectionPinExternalSource>,
	external_state: LogicState,
	relative_end_grid: IntV2,
	direction: FourWayDir,
	/// Usually 1, may be something else if theres a curve on an OR input or something
	length: f32
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
			length
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
pub trait LogicDevice: Debug where Self: 'static {
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
	fn into_box(self: Box<Self>) -> Box<dyn LogicDevice> where Self: Sized {
		self as Box<dyn LogicDevice>
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

#[derive(Debug)]
pub struct LogicCircuit {
	generic_device: LogicDeviceGeneric,
	pub components: GenericDataset<RefCell<Box<dyn LogicDevice>>>,
	pub nets: GenericDataset<LogicNet>,
	pub wires: GenericDataset<Wire>,
	save_path: String
}

impl LogicCircuit {
	pub fn new(
		components_not_celled: GenericDataset<Box<dyn LogicDevice>>,
		external_connections: GenericDataset<LogicConnectionPin>,
		nets: GenericDataset<LogicNet>,
		position_grid: IntV2,
		unique_name: String,
		sub_compute_cycles: usize,
		wires: GenericDataset<Wire>,
		save_path: String
	) -> Result<Self, String> {
		let mut components = GenericDataset::<RefCell<Box<dyn LogicDevice>>>::new();
		for (ref_, comp) in components_not_celled.items {
			components.items.push((ref_.into_another_type(), RefCell::new(comp)));
		}
		let mut new = Self {
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
		};
		new.recompute_connections();
		Ok(new)
	}
	pub fn from_save(save: LogicCircuitSave, save_path: String) -> Result<Self, String> {
		let mut components = GenericDataset::<RefCell<Box<dyn LogicDevice>>>::new();
		// Init compnents
		for (ref_, save_comp) in save.components.items {
			components.items.push((ref_.into_another_type(), RefCell::new(EnumAllLogicDevicesSave::to_dynamic(save_comp)?)));
		}
		Ok(Self {
			generic_device: save.generic_device,
			components,
			nets: save.nets,
			wires: save.wires,
			save_path
		})
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
		let mut net_to_pin_connections_to_drop = Vec::<(usize, Vec<usize>)>::new();
		for (net_i, (net_ref, net)) in self.nets.items.iter().enumerate() {
			let mut this_net_to_pin_connections_to_drop = Vec::<usize>::new();
			for (conn_i, net_connection) in net.connections.iter().enumerate() {
				match net_connection {
					CircuitWidePinReference::ComponentPin(component_pin_ref) => match self.components.get_item_mut(&component_pin_ref.component_query.into_another_type()) {
						Some(component) => match component.borrow_mut().query_pin_mut(&component_pin_ref.pin_query) {
							Some(pin) => {
								pin.external_source = Some(LogicConnectionPinExternalSource::Net(net_ref.to_query()));
							}
							None => this_net_to_pin_connections_to_drop.push(conn_i),
						},
						None => this_net_to_pin_connections_to_drop.push(conn_i)
					},
					CircuitWidePinReference::ExternalConnection(ext_conn_query) => match self.generic_device.pins.get_item_mut(&ext_conn_query) {
						Some(pin) => {
							pin.internal_source = Some(LogicConnectionPinInternalSource::Net(net_ref.to_query()));
						},
						None => this_net_to_pin_connections_to_drop.push(conn_i)
					}
				}
			}
			net_to_pin_connections_to_drop.push((net_i, this_net_to_pin_connections_to_drop));
		}
		// Remove broken connections
		for (net_i, conns) in net_to_pin_connections_to_drop {
			for conn_i in conns.iter().rev() {// Very important to reverse list of indices to delete, because deleting lower indices first would shift the array and make later ones invalid
				self.nets.items[net_i].1.connections.remove(*conn_i);
			}
		}
		// Component pin -> Net
		let mut pin_to_net_connections_to_drop = Vec::<CircuitWidePinReference>::new();
		for (comp_ref, comp) in &self.components.items {
			for (pin_ref, pin) in &comp.borrow().get_generic().pins.items {
				if let Some(source) = &pin.external_source {
					let ref_to_this_pin = CircuitWidePinReference::ComponentPin(ComponentPinReference::new(comp_ref.to_query().into_another_type(), pin_ref.to_query()));
					match source {
						LogicConnectionPinExternalSource::Global => panic!("Pin {:?} of component {:?} has external source 'Global' which doesn't make sense", &pin_ref, &comp_ref),
						LogicConnectionPinExternalSource::Net(net_query) => match self.nets.get_item_mut(net_query) {
							Some(net) => net.add_component_connection_if_missing(comp_ref, pin_ref),
							None => pin_to_net_connections_to_drop.push(ref_to_this_pin.clone()),
						}
					}
				}
			}
		}
		// External pin -> Net
		for (pin_ref, pin) in &self.generic_device.pins.items {
			if let Some(source) = &pin.internal_source {
				match source {
					LogicConnectionPinInternalSource::ComponentInternal => panic!("External connection pin {:?} of circuit {:?} has internal source 'ComponentInternal' which doesn't make sense", &pin_ref, &self.generic_device.unique_name),
					LogicConnectionPinInternalSource::Net(net_query) => match self.nets.get_item_mut(&net_query) {
						Some(net) => net.add_external_connection_if_missing(pin_ref),
						None => pin_to_net_connections_to_drop.push(CircuitWidePinReference::ExternalConnection(pin_ref.to_query())),
					}
				}
			}
		}
		// Remove broken Pin -> Net connections
		for pin_ref in pin_to_net_connections_to_drop {
			match pin_ref {
				CircuitWidePinReference::ComponentPin(component_pin_ref) => {
					self.components.get_item_mut(&component_pin_ref.component_query.into_another_type()).expect(&format!("Component query {:?} from net connections to delete is invalid", &component_pin_ref.component_query)).borrow_mut().query_pin_mut(&component_pin_ref.pin_query).expect(&format!("Pin query {:?} for component {:?} from net connections to delete is invalid", &component_pin_ref.pin_query, &component_pin_ref.component_query)).external_source = None;
				},
				CircuitWidePinReference::ExternalConnection(ext_conn_query) => {
					self.generic_device.pins.get_item_mut(&ext_conn_query).expect(&format!("External connection query {:?} from net connections to delete is invalid", &ext_conn_query)).internal_source = None;
				}
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
		drop(ancestors);
		for (i, (_, net)) in &mut self.nets.items.iter_mut().enumerate() {
			net.state = new_net_states[i].0;
			net.sources = new_net_states[i].1.clone();
		}
		// Update pin & wire states from nets
		// Wires
		for (wire_ref, wire) in &mut self.wires.items {
			wire.state = self.nets.get_item_tuple(&wire.net).expect(&format!("Wire {:?} has invalid net query {:?}", wire_ref, &wire.net)).1.state;
		}
		// External connection pins
		for (pin_ref, pin) in &mut self.generic_device.pins.items {
			if let Some(source) = &pin.internal_source {
				match source {
					LogicConnectionPinInternalSource::Net(net_query) => pin.set_drive_internal(self.nets.get_item_tuple(net_query).expect(&format!("External connection pin {:?} has invalid net query {:?}", pin_ref, net_query)).1.state),
					LogicConnectionPinInternalSource::ComponentInternal => panic!("External connection pin {:?} for circuit \"{}\" has the internal source as ComponentInternal which shouldn't happen", pin_ref, &self.generic_device.unique_name)
				}
			}
		}
		// Component pins, and propagate through components
		for (comp_ref, comp) in &self.components.items {
			for (pin_ref, pin) in &mut comp.borrow_mut().get_generic_mut().pins.items {
				if let Some(source) = &pin.external_source {
					match source {
						LogicConnectionPinExternalSource::Global => panic!("Pin {:?} of component {:?} has external source 'Global' which doesn't make sense", &pin_ref, &comp_ref),
						LogicConnectionPinExternalSource::Net(net_query) => {
							pin.external_state = self.nets.get_item_mut(&net_query).expect(&format!("Pin {:?} of component {:?} references net {:?} which doesn't exist", &pin_ref, &comp_ref, &net_query)).state;
						}
					}
				}
			}
		}
		// THIS GOES LAST, has to be seperate loop then before
		let ancestors = ancestors_above.push(&self);
		for (_, comp) in &self.components.items {
			comp.borrow_mut().compute(&ancestors);
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevicesSave, String> {
		// Convert components to enum variants to be serialized
		let mut components_save = GenericDataset::<EnumAllLogicDevicesSave>::new();
		for (ref_, component) in &self.components.items {
			components_save.items.push((ref_.into_another_type(), component.borrow().save()?));
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