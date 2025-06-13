//! For loading and saving things using JSON

use std::fs;
use crate::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json;
use crate::simulator;


// STATICS
pub static RESOURCES_DIR: &str = "resources/";
pub static CIRCUITS_DIR: &str = "resources/circuits/";
pub static STYLES_FILE: &str = "resources/styles.json";

/// Loads circuit, path is relative to `CIRCUITS_DIR` and does not include .json extension
/// Example: File is located at `resources/circuits/sequential/d_latch.json` -> circuit path is `sequential/d_latch`
pub fn load_circuit(circuit_rel_path: &str) -> Result<LogicCircuit, String> {
    let path = get_circuit_file_path(circuit_rel_path);
    let string_raw = load_file_with_better_error(&path)?;
    LogicCircuit::from_save(to_string_err(serde_json::from_str(&string_raw))?, circuit_rel_path.to_string())
}

/// Like LogicCircuit but serializable
#[derive(Debug, Serialize, Deserialize)]
pub struct LogicCircuitSave {
	pub generic_device: LogicDeviceGeneric,
	pub components: GenericDataset<EnumAllLogicDevicesSave>,
	pub nets: GenericDataset<LogicNet>,
	pub wires: GenericDataset<Wire>
}

/// Not great but I can't think of anything else
#[derive(Debug, Serialize, Deserialize)]
pub enum EnumAllLogicDevicesSave {
    /// Relative path of circuit
    SubCircuit(String),
    GateAnd(simulator::GateAnd)
}

impl EnumAllLogicDevicesSave {
    pub fn to_dynamic(self_instance: Self) -> Result<Box<dyn LogicDevice>, String> {
        match self_instance {
            Self::SubCircuit(circuit_rel_path) => Ok(Box::new(load_circuit(&circuit_rel_path)?)),
            Self::GateAnd(gate) => Ok(Box::new(gate))
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