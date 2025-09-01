use crate::prelude::*;
use crate::simulator::{AncestryStack, LogicConnectionPinExternalSource, LogicConnectionPinInternalSource};
use std::rc::Rc;

#[test]
fn logic_state_merge() {
	use crate::simulator::{merge_logic_states, LogicState};
	// Floating vs ?
	assert_eq!(merge_logic_states(LogicState::Floating, LogicState::Floating), LogicState::Floating);
	assert_eq!(merge_logic_states(LogicState::Floating, LogicState::Contested), LogicState::Contested);
	assert_eq!(merge_logic_states(LogicState::Contested, LogicState::Floating), LogicState::Contested);
	assert_eq!(merge_logic_states(LogicState::Floating, LogicState::Driven(true)), LogicState::Driven(true));
	assert_eq!(merge_logic_states(LogicState::Driven(true), LogicState::Floating), LogicState::Driven(true));
	assert_eq!(merge_logic_states(LogicState::Floating, LogicState::Driven(false)), LogicState::Driven(false));
	assert_eq!(merge_logic_states(LogicState::Driven(false), LogicState::Floating), LogicState::Driven(false));
	// Contested vs ?
	assert_eq!(merge_logic_states(LogicState::Contested, LogicState::Driven(false)), LogicState::Contested);
	assert_eq!(merge_logic_states(LogicState::Driven(false), LogicState::Contested), LogicState::Contested);
	assert_eq!(merge_logic_states(LogicState::Contested, LogicState::Driven(true)), LogicState::Contested);
	assert_eq!(merge_logic_states(LogicState::Driven(true), LogicState::Contested), LogicState::Contested);
	assert_eq!(merge_logic_states(LogicState::Contested, LogicState::Contested), LogicState::Contested);
	// Creating contention
	assert_eq!(merge_logic_states(LogicState::Driven(false), LogicState::Driven(true)), LogicState::Contested);
	assert_eq!(merge_logic_states(LogicState::Driven(true), LogicState::Driven(false)), LogicState::Contested);
	// Successful merging
	assert_eq!(merge_logic_states(LogicState::Driven(false), LogicState::Driven(false)), LogicState::Driven(false));
	assert_eq!(merge_logic_states(LogicState::Driven(true), LogicState::Driven(true)), LogicState::Driven(true));
}

/*fn create_simple_circuit() -> LogicCircuit {
	LogicCircuit::new(
		vec![
			Box::new(basic_components::GateAnd::new(IntV2(0, 0), "and", FourWayDir::default())).into_box()
		].into(),
		vec![
			(LogicConnectionPin::new(None, Some(LogicConnectionPinExternalSource::Global), IntV2(-3, -1), FourWayDir::default(), 1.0), "a"),
			(LogicConnectionPin::new(None, Some(LogicConnectionPinExternalSource::Global), IntV2(-3, 1), FourWayDir::default(), 1.0), "b"),
			(LogicConnectionPin::new(None, Some(LogicConnectionPinExternalSource::Global), IntV2(3, 0), FourWayDir::default(), 1.0), "q"),
		].into(),
		vec![
			LogicNet::new(vec![
				CircuitWidePinReference::ComponentPin(ComponentPinReference::new(0.into(), "a".into())),
				CircuitWidePinReference::ExternalConnection("a".into())
			]),
			LogicNet::new(vec![
				CircuitWidePinReference::ComponentPin(ComponentPinReference::new(0.into(), "b".into())),
				CircuitWidePinReference::ExternalConnection("b".into())
			]),
			LogicNet::new(vec![
				CircuitWidePinReference::ComponentPin(ComponentPinReference::new(0.into(), "q".into())),
				CircuitWidePinReference::ExternalConnection("q".into())
			]),
		].into(),
		IntV2(0, 0),
		"test-circuit".to_string(),
		1,
		GenericDataset::new(),
		"test".to_string(),
		true,
		false,
		1.0,
		false
	).unwrap()
}*/

#[test]
fn basic_sim_and_gate() {
	let mut circuit = create_simple_circuit();
	// Connections b/w wires & pins
	assert!(Rc::ptr_eq(circuit.generic_device.graphic_pins.borrow().get(&0).unwrap().wire_connections.as_ref().unwrap(), &circuit.wires.borrow().get(&0).unwrap().borrow().start_connections));
	assert!(Rc::ptr_eq(circuit.components.borrow().get(&0).unwrap().borrow().get_generic().graphic_pins.borrow().get(&0).unwrap().wire_connections.as_ref().unwrap(), &circuit.wires.borrow().get(&0).unwrap().borrow().end_connections));
	assert!(Rc::ptr_eq(circuit.generic_device.graphic_pins.borrow().get(&1).unwrap().wire_connections.as_ref().unwrap(), &circuit.wires.borrow().get(&1).unwrap().borrow().start_connections));
	assert!(Rc::ptr_eq(circuit.components.borrow().get(&0).unwrap().borrow().get_generic().graphic_pins.borrow().get(&1).unwrap().wire_connections.as_ref().unwrap(), &circuit.wires.borrow().get(&1).unwrap().borrow().end_connections));
	assert!(Rc::ptr_eq(circuit.generic_device.graphic_pins.borrow().get(&2).unwrap().wire_connections.as_ref().unwrap(), &circuit.wires.borrow().get(&2).unwrap().borrow().start_connections));
	assert!(Rc::ptr_eq(circuit.components.borrow().get(&0).unwrap().borrow().get_generic().graphic_pins.borrow().get(&2).unwrap().wire_connections.as_ref().unwrap(), &circuit.wires.borrow().get(&2).unwrap().borrow().end_connections));
	// Nets computed correctly
	{
		let binding = circuit.components.borrow();
		let binding2 = binding.get(&0).unwrap().borrow();
		let and_gate = binding2.get_generic();
		{
			let graphic_pin_id: u64 = 0;
			let binding = circuit.get_logic_pins_cell().borrow();
			let binding2 = binding.get(&circuit.generic_device.graphic_pins.borrow().get(&graphic_pin_id).unwrap().owned_pins[0]).unwrap().borrow();
			let ext_conn_0_net: &LogicConnectionPinInternalSource = binding2.internal_source.as_ref().unwrap();
			if let LogicConnectionPinInternalSource::Net(ext_net) = ext_conn_0_net {
				// Now get corresponding component pin net
				let binding = and_gate.logic_pins.borrow();
				let binding2 = binding.get(&graphic_pin_id).unwrap().borrow();// For the builtin components, graphic and logic pins share the same IDs
				let comp_pin_source = binding2.external_source.as_ref().unwrap();
				if let LogicConnectionPinExternalSource::Net(comp_net) = comp_pin_source {
					assert_eq!(ext_net, comp_net);
				}
				else {
					panic!("Comp pin ext source not a net");
				}
			}
			else {
				panic!("Ext conn pin internal source not a net");
			}
		}
		{
			let graphic_pin_id: u64 = 1;
			let binding = circuit.get_logic_pins_cell().borrow();
			let binding2 = binding.get(&circuit.generic_device.graphic_pins.borrow().get(&graphic_pin_id).unwrap().owned_pins[0]).unwrap().borrow();
			let ext_conn_0_net: &LogicConnectionPinInternalSource = binding2.internal_source.as_ref().unwrap();
			if let LogicConnectionPinInternalSource::Net(ext_net) = ext_conn_0_net {
				// Now get corresponding component pin net
				let binding = and_gate.logic_pins.borrow();
				let binding2 = binding.get(&graphic_pin_id).unwrap().borrow();// For the builtin components, graphic and logic pins share the same IDs
				let comp_pin_source = binding2.external_source.as_ref().unwrap();
				if let LogicConnectionPinExternalSource::Net(comp_net) = comp_pin_source {
					assert_eq!(ext_net, comp_net);
				}
				else {
					panic!("Comp pin ext source not a net");
				}
			}
			else {
				panic!("Ext conn pin internal source not a net");
			}
		}
		{
			let graphic_pin_id: u64 = 2;
			let binding = circuit.get_logic_pins_cell().borrow();
			let binding2 = binding.get(&circuit.generic_device.graphic_pins.borrow().get(&graphic_pin_id).unwrap().owned_pins[0]).unwrap().borrow();
			let ext_conn_0_net: &LogicConnectionPinInternalSource = binding2.internal_source.as_ref().unwrap();
			if let LogicConnectionPinInternalSource::Net(ext_net) = ext_conn_0_net {
				// Now get corresponding component pin net
				let binding = and_gate.logic_pins.borrow();
				let binding2 = binding.get(&graphic_pin_id).unwrap().borrow();// For the builtin components, graphic and logic pins share the same IDs
				let comp_pin_source = binding2.external_source.as_ref().unwrap();
				if let LogicConnectionPinExternalSource::Net(comp_net) = comp_pin_source {
					assert_eq!(ext_net, comp_net);
				}
				else {
					panic!("Comp pin ext source not a net");
				}
			}
			else {
				panic!("Ext conn pin internal source not a net");
			}
		}
	}
	// Set global inputs
	circuit.set_logic_pin_external_state(0, true.into()).unwrap();
	circuit.set_logic_pin_external_state(1, true.into()).unwrap();
	// Compute
	circuit.compute(&AncestryStack::new(), 0, true, true);
	circuit.compute(&AncestryStack::new(), 0, true, true);
	// AND gate output
	assert_eq!(circuit.get_pin_state_panic(2), true.into());
	// Set global inputs
	circuit.set_logic_pin_external_state(0, false.into()).unwrap();
	// Compute
	circuit.compute(&AncestryStack::new(), 0, true, true);
	circuit.compute(&AncestryStack::new(), 0, true, true);
	// AND gate output
	assert_eq!(circuit.get_pin_state_panic(2), false.into());
	// Set global inputs
	circuit.set_logic_pin_external_state(0, true.into()).unwrap();
	circuit.set_logic_pin_external_state(1, false.into()).unwrap();
	// Compute
	circuit.compute(&AncestryStack::new(), 0, true, true);
	circuit.compute(&AncestryStack::new(), 0, true, true);
	// AND gate output
	assert_eq!(circuit.get_pin_state_panic(2), false.into());
}