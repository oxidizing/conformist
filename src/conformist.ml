module String_error : Core.ERROR with type error = string = struct
  type error = string

  let invalid_bool = "Invalid value provided"
  let invalid_float = "Invalid number provided"
  let invalid_int = "Invalid number provided"
  let invalid_string = "Invalid value provided"
  let invalid_date = "Invalid date provided"
  let invalid_datetime = "Invalid datetime provided"
  let no_value = "No value provided"
  let of_string s = s
end

module Make = Core.Make
include Core.Make (String_error)

type error_msg = string
