use canvas::{Cache, Fill, Frame, Geometry, Path, Program};
use iced::widget::{canvas, Canvas};
use iced::{mouse, Length, Rectangle, Renderer, Theme};

use super::{ElementPtr, IcedMessage};

mod path;
mod path_builder;

type SelfPtr = *mut Canvas<&'static CanvasState, IcedMessage>;
type Draw = unsafe extern "C" fn(frame: *mut Frame);

pub struct CanvasState {
    cache: Cache,
    draw_hs: Option<Draw>,
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
        let Some(draw) = self.draw_hs else {
            return vec![];
        };
        let geometry = self.cache.draw(renderer, bounds.size(), |frame| {
            unsafe { draw(frame) };
        });
        vec![geometry]
    }
}

#[no_mangle]
pub extern "C" fn canvas_state_new(draw: Draw) -> *mut CanvasState {
    let state = CanvasState {
        cache: Cache::default(),
        draw_hs: Some(draw),
    };
    Box::into_raw(Box::new(state))
}

#[no_mangle]
pub extern "C" fn canvas_set_draw(state: &mut CanvasState, draw: Draw) {
    state.draw_hs = Some(draw);
}

#[no_mangle]
pub extern "C" fn canvas_remove_draw(state: &mut CanvasState) {
    state.draw_hs = None;
}

#[no_mangle]
pub extern "C" fn canvas_clear_cache(state: &mut CanvasState) {
    state.cache.clear();
}

#[no_mangle]
pub extern "C" fn canvas_state_free(pointer: *mut CanvasState) {
    let _ = unsafe { Box::from_raw(pointer) };
}

#[no_mangle]
pub extern "C" fn canvas_new(state: &'static CanvasState) -> SelfPtr {
    let canvas = canvas(state);
    Box::into_raw(Box::new(canvas))
}

#[no_mangle]
pub extern "C" fn canvas_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let canvas = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(canvas.width(width)))
}

#[no_mangle]
pub extern "C" fn canvas_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let canvas = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(canvas.height(height)))
}

#[no_mangle]
pub extern "C" fn canvas_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let canvas = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(canvas.into()))
}

#[no_mangle]
pub extern "C" fn frame_fill(frame: &mut Frame, path_pointer: *mut Path) {
    let path = unsafe { Box::from_raw(path_pointer) };
    frame.fill(&path, Fill::default());
}
