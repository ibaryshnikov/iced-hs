pub type CommandCallback = unsafe extern "C" fn() -> *const u8;

pub struct UpdateResult {
    pub model_ptr: *const u8,
    pub command: Option<CommandCallback>,
}

#[no_mangle]
pub extern "C" fn update_result_new(model_ptr: *const u8) -> *mut UpdateResult {
    Box::into_raw(Box::new(UpdateResult {
        model_ptr,
        command: None,
    }))
}

#[no_mangle]
pub extern "C" fn update_result_add_command(
    result: &mut UpdateResult,
    maybe_command: Option<CommandCallback>,
) {
    let Some(command) = maybe_command else {
        return;
    };
    result.command = Some(command);
}
