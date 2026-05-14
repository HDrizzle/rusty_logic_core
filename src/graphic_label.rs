//! Graphic label

use std::{cell::RefCell, default::Default, fmt::Debug};
use serde::{Deserialize, Serialize};
use crate::prelude::*;

#[derive(Debug, Clone)]
pub struct GraphicLabel {
	ui_data: UIData,
	text: String,
	/// Relative to parent circuit
	vertical: bool,
	text_size: RefCell<V2>
}

impl GraphicLabel {
	pub fn new() -> Self {
		Self {
			ui_data: UIData::new(IntV2(0, 0), FourWayDir::default(), (V2::zeros(), V2::zeros())),
			text: "New Label".to_owned(),
			vertical: false,
			text_size: RefCell::new(V2::zeros())
		}
	}
	pub fn save(&self) -> GraphicLabelSave {
		GraphicLabelSave {
			pos: self.ui_data.position,
			dir: self.ui_data.direction,
			text: self.text.clone(),
			vertical: self.vertical
		}
	}
	pub fn load(save: GraphicLabelSave) -> Self {
		Self {
			ui_data: UIData::new(save.pos, save.dir, (V2::zeros(), V2::zeros())),
			text: save.text,
			vertical: save.vertical,
			text_size: RefCell::new(V2::zeros())
		}
	}
}

impl GraphicSelectableItem for GraphicLabel {
	fn draw<'a>(&self, draw_parent: &Box<dyn DrawInterface>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.ui_data.position, self.ui_data.direction);
		let text_size_grid: f32 = draw.styles().text_size_grid;
		draw.text(&self.text, V2::zeros(), GenericAlign2::CENTER_CENTER, draw.styles().text_color, text_size_grid, false);
		{
			*self.text_size.borrow_mut() = V2::new(draw.text_size(&self.text, text_size_grid).x, text_size_grid);
		}
	}
	fn get_ui_data(&self) -> &UIData {
		&self.ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.ui_data
	}
	#[cfg(feature = "using_egui")]
	fn get_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::PositionX(self.ui_data.position.0),
			SelectProperty::PositionY(self.ui_data.position.1),
			SelectProperty::Direction(self.ui_data.direction),
			SelectProperty::Name(self.text.clone())
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
			SelectProperty::Name(new_text) => {
				self.text = new_text;
			}
			_ => {}
		}
	}
	#[cfg(feature = "using_egui")]
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::GraphicLabel(self.save())
	}
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let text_half_size: V2 = *self.text_size.borrow() / 2.0;
		let local_bb = (-text_half_size, text_half_size);
		merge_points_to_bb(vec![grid_offset + self.ui_data.pos_to_parent_coords_float(local_bb.0), grid_offset + self.ui_data.pos_to_parent_coords_float(local_bb.1)])
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphicLabelSave {
	pos: IntV2,
	dir: FourWayDir,
	text: String,
	/// Relative to parent circuit
	vertical: bool
}