type 'a decoder = string list -> ('a, string) result
type 'a encoder = 'a -> string list
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
    | ( :: ) :
        ('meta, 'a) t * ('meta, 'b, 'ty) list
        -> ('meta, 'a -> 'b, 'ty) list

  type _ any_field = AnyField : ('meta, 'a) t -> 'meta any_field

  let meta (AnyField field) = field.meta
  let name (AnyField field) = field.name

  let validate (AnyField field) input =
    match field.decoder input with
    | Ok value -> field.validator value
    | Error msg -> Some msg
  ;;

  let optional (AnyField field) = field.optional
  let is_optional (AnyField field) = field.optional
  let type_ (AnyField field) = field.type_

  let encode_default (AnyField field) : string List.t =
    match field.default with
    | Some v -> field.encoder v
    | None -> []
  ;;

  let make name meta decoder encoder default type_ validator optional =
    { name; meta; default; decoder; encoder; type_; validator; optional }
  ;;

  let make_custom
      decoder
      encoder
      ?default
      ?type_
      ?meta
      ?(validator = always_valid)
      name
    =
    let type_ = Option.value type_ ~default:name in
    make name meta decoder encoder default type_ validator false
  ;;

  let make_optional ?meta field =
    let decoder string =
      match field.decoder string with
      | Ok result -> Ok (Some result)
      | Error msg -> Error msg
    in
    let validator a =
      match a with
      | Some a -> field.validator a
      | None -> None
    in
    let encoder a =
      match a with
      | Some a -> field.encoder a
      | None -> [ "None" ]
    in
    make
      field.name
      meta
      decoder
      encoder
      (Some field.default)
      field.type_
      validator
      true
  ;;

  let make_list ?default ?meta field =
    let decoder (l : string List.t) : ('a List.t, string) result =
      List.fold_left
        (fun res (el : string) ->
          match res, field.decoder [ el ] with
          | Ok result, Ok el -> Ok (List.cons el result)
          | Ok _, Error msg -> Error msg
          | Error msg, _ -> Error msg)
        (Ok [])
        l
      |> Result.map List.rev
    in
    let validator l =
      List.fold_left
        (fun res el ->
          match res, field.validator el with
          | None, None -> None
          | None, Some msg -> Some msg
          | Some msg, _ -> Some msg)
        None
        l
    in
    let encoder (a : 'a List.t) = List.map field.encoder a |> List.concat in
    make field.name meta decoder encoder default field.type_ validator true
  ;;

  let make_bool ?default ?meta ?(msg = "Invalid value provided") name =
    let decoder input =
      try Ok (bool_of_string (List.hd input)) with
      | _ -> Error msg
    in
    let encoder input = List.[ string_of_bool input ] in
    make name meta decoder encoder default "bool" always_valid false
  ;;

  let make_float
      ?default
      ?meta
      ?(msg = "Invalid number provided")
      ?(validator = always_valid)
      name
    =
    let decoder string =
      try Ok (float_of_string (List.hd string)) with
      | _ -> Error msg
    in
    let encoder input = List.[ string_of_float input ] in
    make name meta decoder encoder default "float" validator false
  ;;

  let make_int
      ?default
      ?meta
      ?(msg = "Invalid number provided")
      ?(validator = always_valid)
      name
    =
    let decoder string =
      try Ok (int_of_string (List.hd string)) with
      | _ -> Error msg
    in
    let encoder input = List.[ string_of_int input ] in
    make name meta decoder encoder default "int" validator false
  ;;

  let make_string ?default ?meta ?(validator = always_valid) name =
    let decoder input =
      try Ok (List.hd input) with
      | _ -> Error "Invalid input provided"
    in
    let encoder id = List.[ id ] in
    make name meta decoder encoder default "string" validator false
  ;;

  let make_date
      ?default
      ?meta
      ?(msg = "Invalid date provided")
      ?(validator = always_valid)
      name
    =
    let decoder input =
      try
        match String.split_on_char '-' (List.hd input) with
        | [ y; m; d ] ->
          (match
             int_of_string_opt y, int_of_string_opt m, int_of_string_opt d
           with
          | Some y, Some m, Some d -> Ok (y, m, d)
          | _ -> Error msg)
        | _ -> Error msg
      with
      | _ -> Error msg
    in
    let encoder (y, m, d) = List.[ Format.sprintf "%d-%d-%d" y m d ] in
    make name meta decoder encoder default "date" validator false
  ;;

  let make_datetime
      ?default
      ?meta
      ?(msg = "Invalid datetime provided")
      ?(validator = always_valid)
      name
    =
    let decoder string =
      try
        match Ptime.of_rfc3339 (List.hd string) with
        | Ok (timestamp, _, _) -> Ok timestamp
        | Error (`RFC3339 (_, _)) -> Error msg
      with
      | _ -> Error msg
    in
    let encoder ptime = List.[ Ptime.to_rfc3339 ptime ] in
    make name meta decoder encoder default "time" validator false
  ;;
end

let custom = Field.make_custom
let optional = Field.make_optional
let list = Field.make_list
let bool = Field.make_bool
let float = Field.make_float
let int = Field.make_int
let string = Field.make_string
let date = Field.make_date
let datetime = Field.make_datetime

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

type error = string * string list * string
type input = (string * string list) list

let validate
    (schema : ('meta, 'ctor, 'ty) t)
    (input : (string * string list) list)
    : error list
  =
  let f errors field =
    let name = Field.name field in
    match List.assoc name input with
    | values ->
      (match Field.validate field values with
      | Some msg -> List.cons (name, values, msg) errors
      | None -> errors)
    | exception Not_found ->
      (match Field.is_optional field, Field.encode_default field with
      | true, List.[] -> errors
      | false, List.[] -> List.cons (name, [], "No value provided") errors
      | _, default ->
        (match Field.validate field default with
        | Some msg -> List.cons (name, [], msg) errors
        | None -> errors))
  in
  fold_left ~f ~init:[] schema |> List.rev
;;

let rec decode
    : type meta ctor ty.
      (meta, ctor, ty) t -> (string * string list) list -> (ty, error) Result.t
  =
 fun { fields; ctor } fields_assoc ->
  let open! Field in
  match fields with
  | [] -> Ok ctor
  | field :: fields ->
    (match List.assoc field.name fields_assoc with
    | [] ->
      (match field.default with
      | Some value ->
        (match ctor value with
        | ctor -> decode { fields; ctor } fields_assoc
        | exception exn ->
          let msg = Printexc.to_string exn in
          Error (field.name, [], msg))
      | None ->
        (match field.decoder [] with
        | Ok value ->
          (match ctor value with
          | ctor -> decode { fields; ctor } fields_assoc
          | exception exn ->
            let msg = Printexc.to_string exn in
            Error (field.name, [], msg))
        | Error msg -> Error (field.name, [], msg)))
    | values ->
      (match field.decoder values with
      | Ok value ->
        (match ctor value with
        | ctor -> decode { fields; ctor } fields_assoc
        | exception exn ->
          let msg = Printexc.to_string exn in
          Error (field.name, values, msg))
      | Error msg -> Error (field.name, values, msg))
    | exception Not_found ->
      (match field.default with
      | Some value ->
        (match ctor value with
        | ctor -> decode { fields; ctor } fields_assoc
        | exception exn ->
          let msg = Printexc.to_string exn in
          let values =
            match field.default with
            | Some default -> field.encoder default
            | None -> []
          in
          Error (field.name, values, msg))
      | None -> Error (field.name, [], "No value provided")))
;;

let decode_and_validate schema input =
  let validation_errors = validate schema input in
  match decode schema input, validation_errors with
  | Ok value, [] -> Ok value
  | Ok _, validation_errors -> Error validation_errors
  | Error (field_name, value, msg), validation_errors ->
    validation_errors
    |> List.filter (fun (name, _, _) -> not (String.equal name field_name))
    |> List.cons (field_name, value, msg)
    |> Result.error
;;
