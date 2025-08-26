//! This module implements logic for `LogicCircuit` to recompute the nets of all wires, pins, & splitters
//! The logic for correcting wire geometry is not here

use std::{cell::{Ref, RefCell}, collections::{HashMap, HashSet, VecDeque}};
use crate::{prelude::*, simulator::CircuitWideGraphicPinReference};

/// Wire or Splitter
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum NotAWire {
	/// Logical pin
	Pin(CircuitWideGraphicPinReference),
	/// Splitter ID, Splitter graphic connection ID
	Splitter(u64, u16)
}

/// For net island search
#[derive(Clone)]
enum Conductor {
	Wire(u64),
	/// Splitter ID, Splitter net index (NOT graphical split)
	Splitter(u64, u16)
}

impl From<WireConnection> for NotAWire {
	fn from(value: WireConnection) -> Self {
		match value {
			WireConnection::Wire(_) => panic!("This thing is called `NotAWire` dumbass"),
			WireConnection::Pin(pin_ref) => Self::Pin(pin_ref),
			WireConnection::Splitter(id, pin) => Self::Splitter(id, pin)
		}
	}
}

#[derive(Debug, Clone)]
pub struct BitWidthError(Vec<(NotAWire, u16)>);

struct WireIsland {
	pub wires: HashSet<u64>,
	pub external_connection_bit_widths: HashMap<NotAWire, u16>,
	pub nets: Vec<u64>,
	pub bit_width_mismatch: bool
}

impl LogicCircuit {
	/// Run this function after all wires have been fixed and graphical connections have been connected properly, this will set the nets of all wires but not pins
	/// How it works
	/// * Generate list of wire islands (only connected by wires, not across splitters) that have their own bit width
	///   If a wire island is connected to any splitters or pins that have different bit widths, then create a bit width error
	///   Each island will have a Vec<Option<u64>> for nets and a HashSet<NotAWire> for anything also connected
	/// * Iterate all splitters and create/merge nets on each side (if the wire islands on both sides don't have bit width errors)
	/// * Update wire nets/bitwidths from islands
	/// * Set pin nets and add connections to nets
	pub fn recompute_nets(&self) -> Vec<BitWidthError> {
		// Init stuff
		let mut errors = Vec::<BitWidthError>::new();
		// New set of nets, wire islands will be created with all new nets and then they may be merged together
		let mut nets = HashSet::<u64>::new();
		let wires = self.wires.borrow();
		let splitters = self.splitters.borrow();
		// Find all wire islands
		let (islands, ext_conn_island_map): (Vec<WireIsland>, HashMap<NotAWire, usize>) = {
			let mut islands = Vec::<WireIsland>::new();
			let mut ext_conn_island_map = HashMap::<NotAWire, usize>::new();
			let mut i: usize = 0;
			for wire_id in wires.keys() {
				// Check that wire is not in any already found islands
				let mut already_found = false;
				for island in &islands {
					already_found |= island.wires.contains(wire_id);
					break;
				}
				if already_found {
					continue;
				}
				// Create island
				let island = self.find_wire_island(*wire_id, &mut nets);
				for ext_conn in island.external_connection_bit_widths.keys() {
					ext_conn_island_map.insert(ext_conn.clone(), i);
				}
				// Add bit width error if mismatch
				if island.bit_width_mismatch {
					errors.push(BitWidthError(island.external_connection_bit_widths.iter().map(|t| (t.0.clone(), *t.1)).collect()));
				}
				islands.push(island);
				i += 1;
			}
			(islands, ext_conn_island_map)
		};
		// Net islands
		// TODO: Fix: island nets could be double-updated and become disconnected from earlier splitter connections
		for (splitter_id, splitter) in splitters.iter() {
			let base_handle = NotAWire::Splitter(*splitter_id, 0);
			// If there is an island connected to the base of the splitter, iterate over the splitter's fanout and set all those islands (if they exist) to the corresponding base island nets
			if let Some(base_insland_i) = ext_conn_island_map.get(&base_handle) {
				let base_nets: &Vec<u64> = &islands[*base_insland_i].nets;
				for i_raw in 0..splitter.splits.len() {
					let i = i_raw as u16 + 1;
					if let Some(split_island_i) = ext_conn_island_map.get(&NotAWire::Splitter(*splitter_id, i)) {

					}
				}
			}
		}
		// Update wire nets/bitwidths from islands
		for island in &islands {
			for wire_id in &island.wires {
				wires.get(wire_id).unwrap().borrow_mut().nets = island.nets.clone();
			}
		}
		// Reset nets
		*self.nets.borrow_mut() = HashMap::from_iter(nets.iter().map(|k| (*k, RefCell::new(LogicNet::new(Vec::new())))));
		errors
	}
	/// From Gemini, modified by me during multi-bit-width upgrade
	/// Helper function to find a connected "island" of wires using BFS.
	fn find_wire_island(&self, start_wire_id: u64, all_nets: &mut HashSet<u64>) -> WireIsland {
		let wires = self.wires.borrow();
		let mut q = VecDeque::new();
		let mut island_wires = HashSet::new();
		let mut external_connection_bit_widths = HashMap::<NotAWire, u16>::new();
		let mut bit_width_mismatch = false;
		// Set this once first NotAWire is encountered, if any afterward don't match then return an error
		// Initially 1 so that floating wire islands won't bit width=0
		let mut highest_bit_width = 1;
		let mut ext_og_bit_width = Option::<u16>::None;

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
				else {
					// Check bit width
					let ext_conn: NotAWire = connection.into();
					let new_bit_width = self.not_a_wire_bit_width(&ext_conn);
					if new_bit_width > highest_bit_width {
						highest_bit_width = new_bit_width;
					}
					if let Some(current_set_bw) = ext_og_bit_width {
						if current_set_bw != new_bit_width {
							bit_width_mismatch = true;
						}
					}
					else {
						ext_og_bit_width = Some(new_bit_width);
					}
					external_connection_bit_widths.insert(ext_conn, new_bit_width);
				}
			}
		}
		let mut nets = Vec::<u64>::new();
		for _ in 0..highest_bit_width {
			let new_id: u64 = set_lowest_unused_key(all_nets);
			nets.push(new_id);
			all_nets.insert(new_id);
		}
		WireIsland {
			wires: island_wires,
			external_connection_bit_widths,
			nets,
			bit_width_mismatch
		}
	}
	fn not_a_wire_bit_width(&self, handle: &NotAWire) -> u16 {
		match handle {
			NotAWire::Pin(global_pin_handle) => match global_pin_handle {
				CircuitWideGraphicPinReference::ComponentPin(component_pin_ref) => self.components.borrow().get(&component_pin_ref.component_id).unwrap().borrow().get_generic().graphic_pins.borrow().get(&component_pin_ref.pin_id).unwrap().owned_pins.len() as u16,
				CircuitWideGraphicPinReference::ExternalConnection(ext_conn_id) => self.generic_device.graphic_pins.borrow().get(ext_conn_id).unwrap().owned_pins.len() as u16
			},
			NotAWire::Splitter(splitter_id, splitter_pin_id) => self.splitters.borrow().get(splitter_id).unwrap().graphic_pin_bit_width(*splitter_pin_id)
		}
	}
}