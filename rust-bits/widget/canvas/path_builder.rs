use std::ffi::c_float;

use iced::widget::canvas;
use iced::{Point, Radians, Size, Vector};

use canvas::path::arc::Elliptical;
use canvas::path::{Arc, Builder, Path};

#[no_mangle]
extern "C" fn path_builder_new() -> *mut Builder {
    Box::into_raw(Box::new(Builder::new()))
}

// using path_builder_build is preferred
#[no_mangle]
extern "C" fn path_builder_free(pointer: *mut Builder) {
    _ = unsafe { Box::from_raw(pointer) };
}

#[no_mangle]
extern "C" fn path_builder_arc(
    builder: &mut Builder,
    x: c_float,
    y: c_float,
    radius: c_float,
    start_angle: c_float,
    end_angle: c_float,
) {
    builder.arc(Arc {
        center: Point { x, y },
        radius,
        start_angle: Radians(start_angle),
        end_angle: Radians(end_angle),
    });
}

#[no_mangle]
extern "C" fn path_builder_arc_to(
    builder: &mut Builder,
    a_x: c_float,
    a_y: c_float,
    b_x: c_float,
    b_y: c_float,
    radius: c_float,
) {
    builder.arc_to(Point::new(a_x, a_y), Point::new(b_x, b_y), radius);
}

#[no_mangle]
extern "C" fn path_builder_bezier_curve_to(
    builder: &mut Builder,
    control_a_x: c_float,
    control_a_y: c_float,
    control_b_x: c_float,
    control_b_y: c_float,
    to_x: c_float,
    to_y: c_float,
) {
    builder.bezier_curve_to(
        Point::new(control_a_x, control_a_y),
        Point::new(control_b_x, control_b_y),
        Point::new(to_x, to_y),
    );
}

#[no_mangle]
extern "C" fn path_builder_build(pointer: *mut Builder) -> *mut Path {
    let builder = unsafe { Box::from_raw(pointer) };
    let path = builder.build();
    Box::into_raw(Box::new(path))
}

#[no_mangle]
extern "C" fn path_builder_circle(builder: &mut Builder, x: c_float, y: c_float, radius: c_float) {
    builder.circle(Point { x, y }, radius);
}

#[no_mangle]
extern "C" fn path_builder_close(builder: &mut Builder) {
    builder.close()
}

#[no_mangle]
extern "C" fn path_builder_ellipse(
    builder: &mut Builder,
    center_x: c_float,
    center_y: c_float,
    radii_x: c_float,
    radii_y: c_float,
    rotation: c_float,
    start_angle: c_float,
    end_angle: c_float,
) {
    builder.ellipse(Elliptical {
        center: Point::new(center_x, center_y),
        radii: Vector::new(radii_x, radii_y),
        rotation: Radians(rotation),
        start_angle: Radians(start_angle),
        end_angle: Radians(end_angle),
    });
}

#[no_mangle]
extern "C" fn path_builder_line_to(builder: &mut Builder, x: c_float, y: c_float) {
    builder.line_to(Point { x, y });
}

#[no_mangle]
extern "C" fn path_builder_move_to(builder: &mut Builder, x: c_float, y: c_float) {
    builder.move_to(Point { x, y });
}

#[no_mangle]
extern "C" fn path_builder_quadratic_curve_to(
    builder: &mut Builder,
    control_x: c_float,
    control_y: c_float,
    to_x: c_float,
    to_y: c_float,
) {
    builder.quadratic_curve_to(Point::new(control_x, control_y), Point::new(to_x, to_y));
}

#[no_mangle]
extern "C" fn path_builder_rectangle(
    builder: &mut Builder,
    top_left_x: c_float,
    top_left_y: c_float,
    width: c_float,
    height: c_float,
) {
    builder.rectangle(Point::new(top_left_x, top_left_y), Size { width, height });
}
