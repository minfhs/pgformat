open Core

(* Types for formatter state *)
type formatter_state = {
  indent_level: int ref;
  indent_stack: int Stack.t ref;
  values_mode: bool ref;
  references_mode: bool ref;
}

(* Create initial formatter state *)
let create_formatter_state () = {
  indent_level = ref 0;
  indent_stack = ref (Stack.create ());
  values_mode = ref false;
  references_mode = ref false;
}

(* State management functions *)
module StateHelpers = struct
  let get_indent_level state = !(state.indent_level)

  let set_indent_level state level = state.indent_level := level

  let increment_indent state = 
    state.indent_level := !(state.indent_level) + 1

  let decrement_indent state = 
    state.indent_level := max 0 (!(state.indent_level) - 1)

  let push_indent state =
    Stack.push !(state.indent_stack) !(state.indent_level)

  let pop_indent state =
    let level = Option.value ~default:0 (Stack.pop !(state.indent_stack)) in
    state.indent_level := level

  let enable_values_mode state = state.values_mode := true
  let disable_values_mode state = state.values_mode := false
  let is_values_mode state = !(state.values_mode)

  let enable_references_mode state = state.references_mode := true
  let disable_references_mode state = state.references_mode := false
  let is_references_mode state = !(state.references_mode)
end
