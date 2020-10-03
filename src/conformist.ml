type date = int * int * int
type 'a decoder = string -> ('a, string) result
type 'a encoder = 'a -> string
type 'a validator = 'a -> string option

let always_valid _ = None

module Field = struct
  type ('meta, 'a) t =
    { name : string
    ; meta : 'meta option
    ; default : 'a option
    ; decoder : 'a decoder
    ; encoder : 'a encoder
    ; type_ : string
    ; validator : 'a validator
    ; optional : bool
    }

  type (_, _, _) list =
    | [] : ('meta, 'ty, 'ty) list
    | ( :: ) : ('meta, 'a) t * ('meta, 'b, 'ty) list -> ('meta, 'a -> 'b, 'ty) list

  type _ any_field = AnyField : ('meta, 'a) t -> 'meta any_field

  let meta (AnyField field) = field.meta
  let name (AnyField field) = field.name

  let validate (AnyField field) input =
    match field.decoder input with
    | Ok value -> field.validator value
    | Error msg -> Some msg
  ;;

  let optional (AnyField field) = field.optional
  let type_ (AnyField field) = field.type_
  let encode_default (AnyField field) = Option.map field.encoder field.default

  let make name meta decoder encoder default type_ validator optional =
    { name; meta; default; decoder; encoder; type_; validator; optional }
  ;;

  let make_custom decoder encoder ?default ?type_ ?meta ?(validator = always_valid) name =
    let type_ = Option.value type_ ~default:name in
    make name meta decoder encoder default type_ validator false
  ;;

  let make_optional ?meta field =
    let decoder string =
      match string with
      | "" -> Ok None
      | string ->
        (match field.decoder string with
        | Ok result -> Ok (Some result)
        | Error msg -> Error msg)
    in
    let validator a =
      match a with
      | Some a -> field.validator a
      | None -> None
    in
    let encoder a =
      match a with
      | Some a -> field.encoder a
      | None -> "None"
    in
    make field.name meta decoder encoder (Some field.default) field.type_ validator true
  ;;

  let make_bool ?default ?meta ?(msg = "Invalid value provided") name =
    let decoder string =
      try Ok (bool_of_string string) with
      | _ -> Error msg
    in
    make name meta decoder string_of_bool default "bool" always_valid false
  ;;

  let make_float
      ?default
      ?meta
      ?(msg = "Invalid number provided")
      ?(validator = always_valid)
      name
    =
    let decoder string =
      try Ok (float_of_string string) with
      | _ -> Error msg
    in
    make name meta decoder string_of_float default "float" validator false
  ;;

  let make_int
      ?default
      ?meta
      ?(msg = "Invalid number provided")
      ?(validator = always_valid)
      name
    =
    let decoder string =
      try Ok (int_of_string string) with
      | _ -> Error msg
    in
    make name meta decoder string_of_int default "int" validator false
  ;;

  let make_string ?default ?meta ?(validator = always_valid) name =
    let decoder string = Ok string in
    make name meta decoder (fun id -> id) default "string" validator false
  ;;

  let make_date
      ?default
      ?meta
      ?(msg = "Invalid date provided")
      ?(validator = always_valid)
      name
    =
    let decoder string =
      match String.split_on_char '-' string with
      | [ y; m; d ] ->
        (match int_of_string_opt y, int_of_string_opt m, int_of_string_opt d with
        | Some y, Some m, Some d -> Ok (y, m, d)
        | _ -> Error msg)
      | _ -> Error msg
    in
    let encoder (y, m, d) = Format.sprintf "%d-%d-%d" y m d in
    make name meta decoder encoder default "date" validator false
  ;;
end

let custom = Field.make_custom
let optional = Field.make_optional
let bool = Field.make_bool
let float = Field.make_float
let int = Field.make_int
let string = Field.make_string
let date = Field.make_date

type ('meta, 'ctor, 'ty) t =
  { fields : ('meta, 'ctor, 'ty) Field.list
  ; ctor : 'ctor
  }

let empty = { fields = Field.[]; ctor = () }
let make fields ctor = { fields; ctor }

let rec fold_left'
    : type ty args.
      f:('res -> 'meta Field.any_field -> 'res)
      -> init:'res
      -> ('meta, args, ty) Field.list
      -> 'res
  =
 fun ~f ~init fields ->
  match fields with
  | [] -> init
  | field :: fields -> fold_left' ~f ~init:(f init (AnyField field)) fields
;;

let fold_left ~f ~init schema = fold_left' ~f ~init schema.fields

type validation_error = (string * string) list
type input = (string * string list) list

let validate schema input =
  let f errors field =
    let name = Field.name field in
    match List.assoc name input with
    | [ value_string ] ->
      (match Field.validate field value_string with
      | Some msg -> List.cons (name, msg) errors
      | None -> errors)
    | _ -> List.cons (name, "Multiple values provided") errors
    | exception Not_found ->
      (match Field.optional field, Field.encode_default field with
      | _, Some default ->
        (match Field.validate field default with
        | Some msg -> List.cons (name, msg) errors
        | None -> errors)
      | true, None -> errors
      | false, None -> List.cons (name, "No value provided") errors)
  in
  fold_left ~f ~init:[] schema |> List.rev
;;

let rec decode
    : type meta ctor ty.
      (meta, ctor, ty) t -> (string * string list) list -> (ty, string) Result.t
  =
 fun { fields; ctor } fields_assoc ->
  let open! Field in
  match fields with
  | [] -> Ok ctor
  | field :: fields ->
    (match List.assoc field.name fields_assoc with
    | [ value_string ] ->
      (match field.decoder value_string with
      | Ok value ->
        (match ctor value with
        | ctor -> decode { fields; ctor } fields_assoc
        | exception _ ->
          Error
            (Printf.sprintf
               "Failed to decode value '%s' of field '%s'"
               value_string
               field.name))
      | Error msg ->
        Error
          (Printf.sprintf
             "Failed to decode value '%s' of field '%s': %s"
             field.name
             value_string
             msg))
    | [] ->
      (match field.default with
      | Some value ->
        (match ctor value with
        | ctor -> decode { fields; ctor } fields_assoc
        | exception _ ->
          Error (Printf.sprintf "Failed to construct field '%s'" field.name))
      | None ->
        (match field.decoder "" with
        | Ok value ->
          (match ctor value with
          | ctor -> decode { fields; ctor } fields_assoc
          | exception _ ->
            Error
              (Printf.sprintf "Failed to decode value '%s' of field '%s'" "" field.name))
        | Error msg ->
          Error
            (Printf.sprintf
               "Failed to decode value '%s' of field '%s': %s"
               field.name
               ""
               msg)))
    | _ ->
      Error
        (Printf.sprintf
           "Failed to decode field '%s': Multiple values provided"
           field.name)
    | exception Not_found ->
      (match field.default with
      | Some value ->
        (match ctor value with
        | ctor -> decode { fields; ctor } fields_assoc
        | exception _ ->
          Error (Printf.sprintf "Failed to construct field '%s'" field.name))
      | None ->
        Error (Printf.sprintf "Failed to decode field '%s': No value provided" field.name)))
;;
