use canvas::{Cache, Fill, Frame, Geometry, Path, Program};
use iced::widget::{canvas, Canvas};
use iced::{mouse, Element, Length, Rectangle, Renderer, Theme};

use crate::IcedMessage;

use super::ElementPtr;

mod path;
mod path_builder;

// type CanvasPtr = *mut Canvas<CanvasState, IcedMessage>;
type Draw = unsafe extern "C" fn(frame: *mut Frame);

pub struct CanvasState {
    cache: Cache,
    draw_hs: Draw,
}

fn view(state: &CanvasState) -> Element<IcedMessage> {
    Canvas::new(state)
        .width(Length::Fill)
        .height(Length::Fill)
        .into()
}

impl<Message> Program<Message> for CanvasState {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<Geometry> {
        let geometry = self.cache.draw(renderer, bounds.size(), |frame| {
            unsafe { (self.draw_hs)(frame) };
        });
        vec![geometry]
    }
}

#[no_mangle]
pub extern "C" fn new_canvas_state(draw: Draw) -> *mut CanvasState {
    let state = CanvasState {
        cache: Cache::default(),
        draw_hs: draw,
    };
    Box::into_raw(Box::new(state))
}

#[no_mangle]
pub extern "C" fn free_canvas_state(pointer: *mut CanvasState) {
    let _ = unsafe { Box::from_raw(pointer) };
}

#[no_mangle]
pub extern "C" fn canvas_view(state: &'static CanvasState) -> ElementPtr {
    let element = view(state);
    Box::into_raw(Box::new(element))
}

#[no_mangle]
pub extern "C" fn frame_fill(frame: &mut Frame, path_pointer: *mut Path) {
    let path = unsafe { Box::from_raw(path_pointer) };
    frame.fill(&path, Fill::default());
}
