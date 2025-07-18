//! For loading and saving things using JSON

use std::{fs, collections::HashMap};
use crate::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json;
use crate::basic_components;


// STATICS
pub static RESOURCES_DIR: &str = "resources/";
pub static CIRCUITS_DIR: &str = "resources/circuits/";
pub static STYLES_FILE: &str = "resources/styles.json";

/// Loads circuit, path is relative to `CIRCUITS_DIR` and does not include .json extension
/// Example: File is located at `resources/circuits/sequential/d_latch.json` -> circuit path is `sequential/d_latch`
pub fn load_circuit(circuit_rel_path: &str, displayed_as_block: bool) -> Result<LogicCircuit, String> {
	let path = get_circuit_file_path(circuit_rel_path);
	let string_raw = load_file_with_better_error(&path)?;
	LogicCircuit::from_save(to_string_err(serde_json::from_str(&string_raw))?, circuit_rel_path.to_string(), displayed_as_block)
}

/// Like LogicCircuit but serializable
#[derive(Debug, Serialize, Deserialize)]
pub struct LogicCircuitSave {
	pub generic_device: LogicDeviceGeneric,
	pub components: HashMap<u64, EnumAllLogicDevices>,
	pub nets: HashMap<u64, LogicNet>,
	pub wires: HashMap<u64, Wire>,
	pub clock_enabled: bool,
	pub clock_state: bool,
	pub clock_freq: f32,
	/// Inspired by CircuitVerse, block-diagram version of circuit
	/// (pin ID, relative position (ending), direction)
	pub block_pin_positions: HashMap<String, (IntV2, FourWayDir)>
}

/// Not great but I can't think of anything else
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EnumAllLogicDevices {
	/// (Relative path of circuit, Whether to use block diagram)
	SubCircuit(String, bool),
	GateAnd(basic_components::GateAnd),
	GateNand(basic_components::GateNand)
}

impl EnumAllLogicDevices {
	pub fn to_dynamic(self_instance: Self) -> Result<Box<dyn LogicDevice>, String> {
		match self_instance {
			Self::SubCircuit(circuit_rel_path, displayed_as_block) => Ok(Box::new(load_circuit(&circuit_rel_path, displayed_as_block)?)),
			Self::GateAnd(gate) => Ok(Box::new(gate)),
			Self::GateNand(gate) => Ok(Box::new(gate))
		}
	}
	/// Example: "AND Gate" or "D Latch", for the component search UI
	pub fn type_name(&self) -> String {
		match self {
			Self::SubCircuit(circuit_rel_path, displayed_as_block) => circuit_rel_path.clone(),
			Self::GateAnd(gate) => "AND Gate".to_owned(),
			Self::GateNand(gate) => "NAND Gate".to_owned()
		}
	}
}

pub fn get_circuit_file_path(circuit_rel_path: &str) -> String {
	CIRCUITS_DIR.to_owned() + circuit_rel_path + ".json"
}

pub fn load_file_with_better_error(path: &str) -> Result<String, String> {// With help from ChatGPT because I'm lasy
	match fs::read_to_string(path) {
		Ok(contents) => Ok(contents),
		Err(err) => {
			// Combine the error with the path information
			Err(format!("Error reading file '{}': {}", path, err))
		}
	}
}