//! Heavily based off of the logic simulation I wrote in TS for use w/ MotionCanvas, found at https://github.com/HDrizzle/stack_machine/blob/main/presentation/src/logic_sim.tsx

use std::{fmt::{format, Debug}, rc::Rc};
use eframe::egui::{Rgba, Ui};
use crate::prelude::*;

#[derive(Clone, Copy, Debug, PartialEq)]
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

// Not just something that is connected, but something that is setting the voltage either high or low
#[derive(Clone, Debug)]
pub enum LogicDriveSource {
	/// Set by the UI or the Clock or whatever
	Global,
	/// Connection to something outside this circuit
	ExternalConnection(GenericQuery<LogicConnectionPin>),
	/// A set of things connected together
	Net(GenericQuery<LogicNet>),
	/// Output of a basic logic gate, the actual transistors are not gonna be simulated
	InternalConnection(ComponentPinReference)
}

impl LogicDriveSource {
	/// Basic component or global interface, cannot resolve any deeper
	pub fn is_final_source(&self) -> bool {
		match &self {
			Self::Global => true,
			Self::ExternalConnection(_) => false,
			Self::Net(_) => false,
			Self::InternalConnection(_) => true
		}
	}
}

#[derive(Clone, Debug)]
pub struct LogicNet {
	connections: Vec<CircuitWidePinReference>,
	sources: Vec<LogicDriveSource>,
	state: LogicState
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
	fn resolve_sources(&self, ancestors: &AncestryStack) -> Vec<(LogicDriveSource, LogicState)> {
		// TODO: Keep history of recursion to ignore nets that have already been reached
		let mut out = Vec::<(LogicDriveSource, LogicState)>::new();
		for connection in &self.connections {
			match ancestors.parent() {
				LogicParent::Circuit(circuit) => match connection {
					CircuitWidePinReference::ComponentPin(component_pin_ref) => match circuit.components.get_item_tuple(&component_pin_ref.component_query) {
						Some((_, component)) => match component.query_pin(&component_pin_ref.pin_query) {
							Some(pin) => {
								// Check what the internal source is
								if let Some(logic_source) = &pin.internal_source {
									// Check if pin is internally driven (by a sub-circuit)
									if let LogicDriveSource::Net(child_circuit_net_query) = logic_source {
										let circuit = component.get_circuit();
										match circuit.nets.get_item_tuple(child_circuit_net_query) {
											Some((_, child_net)) => out.append(&mut child_net.resolve_sources(&ancestors.push(LogicParent::Circuit(circuit)))),
											None => panic!("Internal connection in circuit \"{}\" references net {:?} inside sub-circuit \"{}\", the net does not exist", circuit.get_generic().unique_name, &child_circuit_net_query, component.get_generic().unique_name)
										}
										continue;
									}
									// Check if pin is driven by a regular component
									if let LogicDriveSource::InternalConnection(component_pin_ref) = logic_source {
										out.push((LogicDriveSource::InternalConnection(component_pin_ref.clone()), pin.internal_state));
										continue;
									}
									// Other disallowed drive sources
									panic!("Internal connection {:?} has internal logic source {:?} which is not allowed", &connection, &pin.external_source);
								}
							},
							None => panic!("Net references internal pin {:?} on component \"{}\" circuit \"{}\", which doesn't exist on that component", &component_pin_ref.pin_query, component.get_generic().unique_name, circuit.get_generic().unique_name)
						},
						None => panic!("Net references internal pin on component {:?} circuit \"{}\", which doesn't exist in the circuit", &component_pin_ref.component_query, circuit.get_generic().unique_name)
					},
					CircuitWidePinReference::ExternalConnection(ext_conn_query) => match circuit.query_pin(&ext_conn_query) {
						Some(pin) => {
							// Check what the external source is
							if let Some(logic_source) = &pin.external_source {
								// Check if external pin is connected to net on other side
								if let LogicDriveSource::Net(parent_circuit_net_query) = logic_source {
									// External pin is connected to a net in a circuit that contains the circuit that this net is a part of
									// Check that this net's "grandparent" is a circuit and not toplevel
									match ancestors.grandparent() {
										LogicParent::Toplevel(_) => panic!("External connection is connected to a net, but the parent of this circuit is toplevel, this shouldn't happen"),
										LogicParent::Circuit(parent_circuit) => match parent_circuit.nets.get_item_tuple(parent_circuit_net_query) {
											Some((_, parent_circuit_net)) => out.append(&mut parent_circuit_net.resolve_sources(&ancestors.trim())),
											None => panic!("External connection is referencing a net ({:?}) which does not exist in this circuit's parent", &parent_circuit_net_query)
										}
									}
									continue;
								}
								// Check if it is connected to a global pin
								if let LogicDriveSource::Global = logic_source {
									out.push((LogicDriveSource::Global, pin.external_state));
									continue;
								}
								// Other disallowed drive sources
								panic!("External connection {:?} has external logic source {:?} which is not allowed", &connection, &pin.external_source);
							}
						},
						None => panic!("Net references external connection {:?} which is invalid", connection)
					}
				},
				LogicParent::Toplevel(_) => panic!("Net cannot have a toplevel wrapper as its parent")
			}
		}
		// Done
		out
	}
	pub fn update_state(&mut self, ancestors: &AncestryStack) {
		let sources_raw = self.resolve_sources(ancestors);
		let mut sources = Vec::<LogicDriveSource>::new();
		let mut new_state = LogicState::Floating;
		// Go through and remove any logic states that are floating
		for (source, state) in sources_raw {
			if !state.is_floating() {
				sources.push(source);
				new_state = merge_logic_states(new_state, state);
			}
		}
		self.state = new_state;
		self.sources = sources;
	}
}

#[derive(Clone, Debug)]
pub struct LogicConnectionPin {
	internal_source: Option<LogicDriveSource>,
	internal_state: LogicState,
	external_source: Option<LogicDriveSource>,
	external_state: LogicState,
	relative_start_grid: V2,
	direction: FourWayDir,
	length: f32,
	name: String
}

impl LogicConnectionPin {
	pub fn set_drive_internal(&mut self, state: LogicState, source: LogicDriveSource) {
		if !state.is_floating() {
			self.internal_source = Some(source);
		}
		else {
			self.internal_source = None;
		}
		self.internal_state = state;
	}
	pub fn set_drive_external(&mut self, state: LogicState, source: LogicDriveSource) {
		if !state.is_floating() {
			self.external_source = Some(source);
		}
		else {
			self.external_source = None;
		}
		self.external_state = state;
	}
	pub fn state(&self) -> LogicState {
		merge_logic_states(self.internal_state, self.external_state)
	}
}

/// Everything within a circuit
#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct ComponentPinReference {
	component_query: GenericQuery<Box<dyn LogicDevice>>,
	pin_query: GenericQuery<LogicConnectionPin>
}

#[derive(Debug)]
pub struct Wire {
	segments: Vec<(V2, V2)>,
	net: GenericQuery<LogicNet>
}

#[derive(Debug)]
pub struct LogicDeviceGeneric {
	pub pins: GenericDataset<LogicConnectionPin>,
	position_grid: V2,
	border_stroke: Rgba,
	unique_name: String,
	sub_compute_cycles: usize,
	rotation: FourWayDir
}

impl LogicDeviceGeneric {
	pub fn new(
		pins: GenericDataset<LogicConnectionPin>,
		position_grid: V2,
		border_stroke: Rgba,
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
			border_stroke,
			unique_name,
			sub_compute_cycles,
			rotation
		})
	}
}

/// Could be a simple gate, or something more complicated like an adder, or maybe even the whole computer
pub trait LogicDevice: Debug {
	fn get_generic(&self) -> &LogicDeviceGeneric;
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric;
	fn compute_private(&mut self, ancestors: &AncestryStack);
	fn compute(&mut self, ancestors: &AncestryStack) {
		for _ in 0..self.get_generic().sub_compute_cycles {
			self.compute_private(ancestors);
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
		state: LogicState,
		external_driver: LogicDriveSource// Either Global or Logic net
	) -> Result<(), String> {
		let generic: &mut LogicDeviceGeneric = self.get_generic_mut();
		let pin: &mut LogicConnectionPin = generic.pins.get_item_mut(pin_query).expect(&format!("Pin query {:?} does not work on logic device \"{}\"", pin_query, generic.unique_name));
		pin.set_drive_external(state, external_driver);
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
		for (pin_query, state, source) in states {
			self.set_pin_external_state(&pin_query, state, source)?;
		}
		Ok(())
	}
}

#[derive(Clone)]
pub enum LogicParent<'a> {
	Circuit(&'a LogicCircuit),
	Toplevel(&'a LogicCircuitToplevelView)
}

impl<'a> LogicParent<'a> {
	pub fn is_toplevel(&self) -> bool {
		match &self {
			Self::Circuit(_) => false,
			Self::Toplevel(_) => true
		}
	}
}

#[derive(Clone)]
pub struct AncestryStack<'a>(Vec<LogicParent<'a>>);

impl<'a> AncestryStack<'a> {
	pub fn parent(&self) -> &LogicParent<'a> {
		self.0.last().expect("Ancestor stack should not be empty")
	}
	pub fn grandparent(&self) -> &LogicParent<'a> {
		&self.0[self.0.len() - 2]
	}
	pub fn trim(&self) -> Self {
		let mut out = self.clone();
		out.0.pop();
		out
	}
	pub fn push(&self, new_node: LogicParent<'a>) -> Self {
		let mut out = self.clone();
		out.0.push(new_node);
		out
	}
}

#[derive(Debug)]
pub struct LogicCircuit {
	generic_device: LogicDeviceGeneric,
	components: GenericDataset<Box<dyn LogicDevice>>,
	nets: GenericDataset<LogicNet>,
	wires: GenericDataset<Wire>
}

impl LogicCircuit {
	pub fn new(
		components: GenericDataset<Box<dyn LogicDevice>>,
		external_connections: GenericDataset<LogicConnectionPin>,
		nets: GenericDataset<LogicNet>,
		position_grid: V2,
		unique_name: String,
		sub_compute_cycles: usize,
		wires: GenericDataset<Wire>
	) -> Result<Self, String> {
		Ok(Self {
			generic_device: LogicDeviceGeneric::new(
				external_connections,
				position_grid,
				Rgba::WHITE,
				unique_name,
				sub_compute_cycles,
				FourWayDir::E
			)?,
			components,
			nets,
			wires
		})
	}
	pub fn resolve_circuit_pin_ref(&self, ref_: &CircuitWidePinReference) -> Option<&LogicConnectionPin> {
		match ref_ {
			CircuitWidePinReference::ComponentPin(component_pin_ref) => match self.components.get_item_tuple(&component_pin_ref.component_query) {
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
	fn compute_private(&mut self, ancestors_above: &AncestryStack) {
		// TODO: Get around borrow checker
		//let ancestors = ancestors_above.push(LogicParent::Circuit(&self));
		// Update net states
		for (_, net) in &mut self.nets.items {
			net.update_state(&ancestors_above);
		}
	}
	fn get_circuit(&self) -> &Self {
		&self
	}
	fn get_circuit_mut(&mut self) -> &mut Self {
		self
	}
}