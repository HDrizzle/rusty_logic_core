//! Probe, used only for recording to the timing diagram

use std::{cell::RefCell, default::Default, fmt::Debug};
use crate::prelude::*;

/// Read-only probe, used for the timing diagram
#[derive(Debug, Default, Clone)]
pub struct Probe {
	pub ui_data: UIData,
	pub name: String,
	pub nets_opt: Vec<Option<u64>>,
	/// Wrt grid
	text_len: RefCell<f32>
}

impl Probe {
	pub fn load(save: ProbeSave) -> Self {
		Self {
			ui_data: UIData::new(save.0, save.1, (V2::zeros(), V2::zeros())),
			name: save.2,
			nets_opt: vec![None],
			text_len: RefCell::new(save.3)
		}
	}
	pub fn save(&self) -> ProbeSave {
		(self.ui_data.position, self.ui_data.direction, self.name.clone(), *self.text_len.borrow())
	}
}

impl GraphicSelectableItem for Probe {
	fn get_ui_data(&self) -> &UIData {
		&self.ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.ui_data
	}
	fn draw<'a>(&self, draw_parent: &Box<dyn DrawInterface>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.ui_data.position, self.ui_data.direction);
		let text_length: f32 = draw.text_size(&self.name, 1.0).x;
		let half_height: f32 = 0.7;
		draw.draw_polyline(
			vec![
				V2::new(0.0, 0.0),
				V2::new(1.0, 0.0),
				V2::new(1.0 + half_height, -half_height),
				V2::new(1.0 + text_length + half_height, -half_height),
				V2::new(1.0 + text_length + half_height*2.0, 0.0),
				V2::new(1.0 + text_length + half_height, half_height),
				V2::new(1.0 + half_height, half_height),
				V2::new(1.0, 0.0),
			],
			draw.styles().color_foreground
		);
		let probe_text_start: f32 = match draw.get_draw_data().direction {
			FourWayDir::E => text_length/2.0,
			FourWayDir::N => text_length,
			FourWayDir::W => text_length/2.0,
			FourWayDir::S => 0.0
		};
		draw.text(&self.name, V2::new(1.0 + half_height + probe_text_start, 0.0), GenericAlign2::CENTER_CENTER, draw.styles().text_color, 1.0, !self.ui_data.direction.is_horizontal());
		*self.text_len.borrow_mut() = text_length;
	}
	#[cfg(feature = "using_egui")]
	fn get_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::PositionX(self.ui_data.position.0),
			SelectProperty::PositionY(self.ui_data.position.1),
			SelectProperty::Direction(self.ui_data.direction),
			SelectProperty::Name(self.name.clone())
		]
	}
	#[cfg(feature = "using_egui")]
	fn set_property(&mut self, property: SelectProperty) {
		match property {
			SelectProperty::PositionX(x) => {
				self.ui_data.position.0 = x;
			},
			SelectProperty::PositionY(y) => {
				self.ui_data.position.1 = y;
			},
			SelectProperty::Direction(direction) => {
				self.ui_data.direction = direction;
			},
			SelectProperty::Name(new_name) => {
				self.name = new_name;
			}
			_ => {}
		}
	}
	#[cfg(feature = "using_egui")]
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::Probe(self.save())
	}
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let local_bb = (V2::new(1.0, -1.0), V2::new(2.0 + *self.text_len.borrow(), 1.0));
		merge_points_to_bb(vec![grid_offset + self.ui_data.pos_to_parent_coords_float(local_bb.0), grid_offset + self.ui_data.pos_to_parent_coords_float(local_bb.1)])
	}
}

/// (Position, Direction, Name, Name length wrt grid)
pub type ProbeSave = (IntV2, FourWayDir, String, f32);