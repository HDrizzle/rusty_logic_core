use crate::prelude::*;
use crate::basic_components;
use crate::simulator::{AncestryStack, CircuitWidePinReference, ComponentPinReference, LogicConnectionPinExternalSource, LogicConnectionPinInternalSource};

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
	let mut circuit = create_simple_circuit(false);
	// Connections computed correctly
	assert_eq!(circuit.get_generic().pins.get("a").unwrap().borrow().internal_source, Some(LogicConnectionPinInternalSource::Net(0)));
	assert_eq!(circuit.get_generic().pins.get("b").unwrap().borrow().internal_source, Some(LogicConnectionPinInternalSource::Net(1)));
	assert_eq!(circuit.get_generic().pins.get("q").unwrap().borrow().internal_source, Some(LogicConnectionPinInternalSource::Net(2)));
	{
		let binding = circuit.components.get(&0).unwrap().borrow();
		let and_gate = binding.get_generic();
		assert_eq!(and_gate.pins.get("a").unwrap().borrow().external_source, Some(LogicConnectionPinExternalSource::Net(0)));
		assert_eq!(and_gate.pins.get("b").unwrap().borrow().external_source, Some(LogicConnectionPinExternalSource::Net(1)));
		assert_eq!(and_gate.pins.get("q").unwrap().borrow().external_source, Some(LogicConnectionPinExternalSource::Net(2)));
	}
	// Set global inputs
	circuit.set_pin_external_state("a", true.into()).unwrap();
	circuit.set_pin_external_state("b", true.into()).unwrap();
	// Compute
	circuit.compute(&AncestryStack::new());
	circuit.compute(&AncestryStack::new());
	// AND gate output
	assert_eq!(circuit.get_pin_state_panic("q"), true.into());
	// Set global inputs
	circuit.set_pin_external_state("a", false.into()).unwrap();
	// Compute
	circuit.compute(&AncestryStack::new());
	circuit.compute(&AncestryStack::new());
	// AND gate output
	assert_eq!(circuit.get_pin_state_panic("q"), false.into());
	// Set global inputs
	circuit.set_pin_external_state("a", true.into()).unwrap();
	circuit.set_pin_external_state("b", false.into()).unwrap();
	// Compute
	circuit.compute(&AncestryStack::new());
	circuit.compute(&AncestryStack::new());
	// AND gate output
	assert_eq!(circuit.get_pin_state_panic("q"), false.into());
}