//! This module implements logic for `LogicCircuit` to recompute the nets of all wires, pins, & splitters
//! The logic for correcting wire geometry is not here

use std::{cell::{Ref, RefCell}, collections::{HashMap, HashSet, VecDeque}};
use crate::{prelude::*, simulator::CircuitWideGraphicPinReference};

/// Wire or Splitter
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum NotAWire {
	/// Logical pin
	Pin(CircuitWideGraphicPinReference),
	/// Splitter ID, Splitter GRAPHIC pin ID
	Splitter(u64, u16)
}

/// For net island search
#[derive(Clone)]
enum Conductor {
	Wire(u64),
	/// Splitter ID, Splitter net index (NOT graphical split)
	Splitter(u64, u16)
}

/// Wire or Splitter specific bit
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ConductorBit {
	/// Wire ID, wire bit index
	Wire(u64, u16),
	/// Splitter ID, bit index
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
	/// * Update wire nets/bitwidths from islands
	/// * DFS to find all connected bits across wires and splitters
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
		// Update wire nets/bitwidths from islands
		for island in &islands {
			for wire_id in &island.wires {
				wires.get(wire_id).unwrap().borrow_mut().nets = island.nets.clone();
			}
		}
		// DFS over each net to connect it properly
		let mut visited_bits = HashSet::<ConductorBit>::new();
		for (base_wire_id, wire_cell) in wires.iter() {
			let mut wire = wire_cell.borrow_mut();
			let mut base_bit_i: u16 = 0;
			while base_bit_i < wire.nets.len() as u16 {
				let base_net: u64 = wire.nets[base_bit_i as usize];
				let conductor_bit_handle = ConductorBit::Wire(*base_wire_id, base_bit_i as u16);
				if visited_bits.contains(&conductor_bit_handle) {
					base_bit_i += 1;
					continue;
				}
				// DFS stack
				let mut bits_to_explore = Vec::<ConductorBit>::new();
				bits_to_explore.push(conductor_bit_handle);
				// DFS
				while bits_to_explore.len() > 0 {
					let current_bit: ConductorBit = bits_to_explore.pop().unwrap();
					if visited_bits.contains(&current_bit) {
						continue;
					}
					visited_bits.insert(current_bit);
					match current_bit {
						ConductorBit::Wire(wire_id, bit_index) => {
							// Set wire net
							if *base_wire_id == wire_id {// Check if same wire to prevent double-borrow
								wire.nets[bit_index as usize] = base_net;
							}
							else {
								wires.get(&wire_id).unwrap().borrow_mut().nets[bit_index as usize] = base_net;
							};
							// Add ends to search stack
							for wire_end_conn in wire.start_connections.borrow().iter().chain(wire.end_connections.borrow().iter()) {
								match wire_end_conn {
									WireConnection::Wire(other_wire_id) => {
										bits_to_explore.push(ConductorBit::Wire(*other_wire_id, bit_index));
									},
									WireConnection::Splitter(splitter_id, pin_i) => {
										let splitter = splitters.get(splitter_id).unwrap();
										if bit_index < splitter.graphic_pin_bit_width(*pin_i) {// Check that this wire's bit index is within the splitter's pin's range
											let splitter_bit_index: u16 = splitter.get_bit_index_from_pin_i_and_wire_bit_index(*pin_i, bit_index);
											bits_to_explore.push(ConductorBit::Splitter(*splitter_id, splitter_bit_index));
										}
									},
									WireConnection::Pin(_) => {}// Handled by seperate function
								}
							}
						},
						ConductorBit::Splitter(splitter_id, splitter_bit_index) => {
							// Add to search stack
							let splitter: &Splitter = splitters.get(&splitter_id).unwrap();
							// Base connection
							if let Some(base_conns) = &splitter.base_connections_opt {
								for conn in base_conns.borrow().iter() {
									if let WireConnection::Wire(other_wire_id) = conn {
										bits_to_explore.push(ConductorBit::Wire(*other_wire_id, splitter_bit_index));
									}
								}
							}
							// Split connections
							/*for (split_i, (_, split_conns_opt)) in splitter.splits.iter().enumerate() {
								if let Some(split_conns_cell) = split_conns_opt {
									for conn in split_conns_cell.borrow().iter() {
										if let WireConnection::Wire(other_wire_id) = conn {
											bits_to_explore.push(ConductorBit::Wire(*other_wire_id, splitter.get_wire_bit_index_from_pin_i_and_bit_index(split_i as u16 + 1, splitter_bit_index)));
										}
									}
								}
							}*/
							// From Gemini
							// Find the correct split pin for this bit index and propagate to its connections.
							if let Some((pin_i, wire_bit_index)) = splitter.get_pin_and_wire_bit_from_splitter_bit(splitter_bit_index) {
								// pin_i is 1-based, so the vector index is pin_i - 1
								let (_, split_conns_opt) = &splitter.splits[pin_i as usize - 1];
								if let Some(split_conns_cell) = split_conns_opt {
									for conn in split_conns_cell.borrow().iter() {
										if let WireConnection::Wire(other_wire_id) = conn {
											// The wire connected to this pin receives the local wire_bit_index.
											bits_to_explore.push(ConductorBit::Wire(*other_wire_id, wire_bit_index));
										}
									}
								}
							}
						}
					}
				}
				base_bit_i += 1;
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
		// Initially 1 so that floating wire islands won't have bit width=0
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