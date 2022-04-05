module type CONFORMIST = sig
  type error_msg

  (** {1 Fields}

      Every member of the list in the example is a field. Use the provided
      [fold_left] to traverse the list of fiels. Helper functions are provided
      that operate on fields. *)

  module Field : sig
    (** A field of type [('meta, 'a) t] represents the static type ['a] and it
        can hold arbitrary meta data of type ['meta]. That meta data can be used
        to build functionality on top of conformist. *)
    type ('meta, 'a) t

    type (_, _, _) list =
      | [] : ('meta, 'ty, 'ty) list
      | ( :: ) :
          ('meta, 'a) t * ('meta, 'b, 'ty) list
          -> ('meta, 'a -> 'b, 'ty) list
          (** A [list] is a list of fields. Note that this is not the list from
              [List.t] so make sure to open this scope locally when defining a
              list of fields. *)

    type _ any_field = AnyField : ('meta, 'a) t -> 'meta any_field

    (** [meta field] returns an optional meta data of a [field]. This can be
        used to store arbitrary meta data in each field. Note that the type of
        the meta data has to be the same for all fields. *)
    val meta : 'a any_field -> 'a option

    (** [name field] returns the name of the [field], which uniquely identifies
        the field within one schema. *)
    val name : 'a any_field -> string

    (** [validate field values] decodes [values] and runs the [field]'s
        validation logic on the decoded values. Both decoding and validation
        might fail, which results in an error string. *)
    val validate : 'a any_field -> string List.t -> error_msg option

    (** [optional field] returns [true] if the [field] is optional and [false]
        otherwise. *)
    val optional : 'a any_field -> bool
      [@@deprecated "Please use is_optional instead"]

    (** [is_optional field] returns [true] if the [field] is optional and
        [false] otherwise. *)
    val is_optional : 'a any_field -> bool

    (** [type_ field] returns a string representation of the type of [field]. *)
    val type_ : 'a any_field -> string

    (** [encode_default field] tries to encode the default value if present and
        to return it as string. *)
    val encode_default : 'a any_field -> string List.t
  end

  (** A ['a decoder] tries to turn values into a value of type ['a]. It returns
      a descriptive errors message upon failure. *)
  type 'a decoder = string list -> ('a, error_msg) result

  (** A ['a encoder] encodes a value of type ['a] into a list of strings. *)
  type 'a encoder = 'a -> string list

  (** A ['a validator] takes something of type ['a] and returns an error string
      if validation fails, [None] if everything is ok *)
  type 'a validator = 'a -> error_msg option

  (** Use [custom decoder encoder ?default ?type_ ?meta ?validator field_name]
      to create a field with a custom type that is not supported out-of-the box.
      Provide a custom [decoder] with a descriptive error message so conformist
      knows how to turn a string into your custom value.

      A string representation of the static [type_] can also be provided, by
      default the [field_name] is taken.

      A [default] value can be provided. *)
  val custom
    :  'a decoder
    -> 'a encoder
    -> ?default:'a
    -> ?type_:string
    -> ?meta:'b
    -> ?validator:'a validator
    -> string
    -> ('b, 'a) Field.t

  (** [optional ?meta field] turns a [field] into an optional field. If the
      field does not exist in the input data or if the associated value in the
      input data is an empty list, the value is [None]. If the data is not
      provided in the input at all, no validation logic is executed.

      Example:

      {[
        let make name address = { name; address } in
        let schema =
          Conformist.(make [ string "name"; optional (string "address") ] make)
        in
        (* Decoding fails *)
        let decoded = Conformist.decode schema [] in
        (* Validation fails *)
        let validated = Conformist.validate [] in
        (* Decoding succeeds, address is [None] *)
        let decoded = Conformist.decode schema [ "name", [ "Walter" ] ] in
        let decoded =
          Conformist.decode schema [ "name", [ "Walter" ]; "address", [] ]
        in
        (* Validation succeeds *)
        let validated = Conformist.validate [ "name", [ "Walter" ] ] in
        ()
      ]} *)
  val optional : ?meta:'a -> ('b, 'c) Field.t -> ('a, 'c option) Field.t

  (** [list ?default ?meta field] returns a field that decodes to a list of
      [field].

      [default] is an optional default value for the field.

      [meta] is optional meta data that is attached to the field. This is useful
      when implementing features on top of conformist. *)
  val list
    :  ?default:'c list
    -> ?meta:'a
    -> ('b, 'c) Field.t
    -> ('a, 'c list) Field.t

  (** [bool ?default ?meta ?msg field_name] returns a field with name
      [field_name] that decodes to a [bool].

      [default] is an optional default value for the field.

      [meta] is optional meta data that is attached to the field. This is useful
      when implementing features on top of conformist.

      [msg] is the decode error message that is returned if {!decode} fails. *)
  val bool
    :  ?default:bool
    -> ?meta:'a
    -> ?msg:error_msg
    -> string
    -> ('a, bool) Field.t

  (** [float ?default ?meta ?msg ?validator field_name] returns a field with
      name [field_name] that decodes to [float].

      [default] is an optional default value for the field.

      [meta] is optional meta data that is attached to the field. This is useful
      when implementing features on top of conformist.

      [msg] is the decode error message that is returned if {!decode} fails.

      [validator] is an optional validator that is run when calling {!validate}.
      By default, no validation logic is executed. This means that if a value
      decodes, it is valid. *)
  val float
    :  ?default:float
    -> ?meta:'a
    -> ?msg:error_msg
    -> ?validator:float validator
    -> string
    -> ('a, float) Field.t

  (** [int ?meta ?msg ?validator field_name] returns a field with name
      [field_name] that decodes to [int].

      [default] is an optional default value for the field.

      [meta] is optional meta data that is attached to the field. This is useful
      when implementing features on top of conformist.

      [msg] is the decode error message that is returned if {!decode} fails.

      [validator] is an optional validator that is run when calling {!validate}.
      By default, no validation logic is executed. This means that if a value
      decodes, it is valid. *)
  val int
    :  ?default:int
    -> ?meta:'a
    -> ?msg:error_msg
    -> ?validator:int validator
    -> string
    -> ('a, int) Field.t

  (** [string ?meta ?validator field_name] return a field with name [field_name]
      that decodes to [string].

      [default] is an optional default value for the field.

      [meta] is optional meta data that is attached to the field. This is useful
      when implementing features on top of conformist.

      [msg] is the decode error message that is returned if {!decode} fails.

      [validator] is an optional validator that is run when calling {!validate}.
      By default, no validation logic is executed. This means that if a value
      decodes, it is valid. *)
  val string
    :  ?default:string
    -> ?meta:'a
    -> ?msg:error_msg
    -> ?validator:string validator
    -> string
    -> ('a, string) Field.t

  (** Don't use [date], use {!datetime} instead.*)
  val date
    :  ?default:Ptime.date
    -> ?meta:'a
    -> ?msg:error_msg
    -> ?validator:(int * int * int) validator
    -> string
    -> ('a, Ptime.date) Field.t
    [@@ocaml.deprecated "Use [Conformist.datetime] instead."]

  (** [datetime ?default ?meta ?validator field_name] returns a field with name
      [field_name] that decodes to [datetime].

      [default] is an optional default value for the field.

      [meta] is optional meta data that is attached to the field. This is useful
      when implementing features on top of conformist.

      [msg] is the decode error message that is returned if {!decode} fails.

      [validator] is an optional validator that is run when calling {!validate}.
      By default, no validation logic is executed. This means that if a value
      decodes, it is valid. *)
  val datetime
    :  ?default:Ptime.t
    -> ?meta:'a
    -> ?msg:error_msg
    -> ?validator:Ptime.t validator
    -> string
    -> ('a, Ptime.t) Field.t

  (** {1 Schema}

      A schema is a list of fields. Input data can be decoded and validated
      using a schema. *)

  (** [t] is a conformist schema. *)
  type ('meta, 'ctor, 'ty) t

  (** [empty] creates an empty schema. *)
  val empty : ('a, unit, unit) t

  (** [make fields constructor] create a schema. *)
  val make : ('a, 'b, 'c) Field.list -> 'b -> ('a, 'b, 'c) t

  (** [fold_left ~f ~init schema] traverses the list of fields of [schema]. Use
      the functions in {!Field} to work with a generic field. *)
  val fold_left
    :  f:('res -> 'meta Field.any_field -> 'res)
    -> init:'res
    -> ('meta, 'args, 'ty) t
    -> 'res

  (** An error [(field, value, error_message)] is used to for decoding errors
      and validation errors.

      [field] is the field name of the input that failed to decode or validate,
      [values] are the input values (if they were provided) and [error_message]
      is the decoding or validation error message.

      An empty list of [error] means that the schema is valid. *)
  type error = string * string list * error_msg

  (** The [input] represents unsafe data that needs to be decoded and validated.
      This is typically some user input. *)
  type input = (string * string list) list

  (** [decode schema input] returns the decoded value of type ['ty] by decoding
      the [input] using the [schema].

      No validation logic is executed in this step. *)
  val decode : ('meta, 'ctor, 'ty) t -> input -> ('ty, error) result

  (** [validate schema input] returns a list of validation errors by running the
      validators defined in [schema] on the [input] data. An empty list implies
      that there are no validation errors and that the input is valid according
      to the schema.

      Note that [input] that has no validation errors might still fail to
      decode, depending on the validation functions specified in [schema]. *)
  val validate : ('meta, 'ctor, 'ty) t -> input -> error list

  (** [decode_and_validate schema input] returns the decoded and validated value
      of type ['ty] by decoding the [input] using the [schema] and running its
      validators.

      Use [decode_and_validate] to combine the functions [decode] and [validate]
      and to either end up with the decoded value or all errors that happened
      during the decoding and validation steps. *)
  val decode_and_validate
    :  ('meta, 'ctor, 'ty) t
    -> input
    -> ('ty, error list) Result.t
end

module type ERROR = sig
  type error

  val invalid_bool : error
  val invalid_float : error
  val invalid_int : error
  val invalid_string : error
  val invalid_date : error
  val invalid_datetime : error
  val no_value : error
  val of_string : string -> error
end

module Make (Error : ERROR) = struct
  type error_msg = Error.error
  type 'a decoder = string list -> ('a, Error.error) result
  type 'a encoder = 'a -> string list
  type 'a validator = 'a -> Error.error option

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
      let decoder strings =
        match field.decoder strings, strings with
        (* Decoding succeeds with nothing when no strings provided *)
        | _, [] -> Ok None
        | Ok result, _ -> Ok (Some result)
        | Error msg, _ -> Error msg
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
      let default =
        match field.default with
        | Some d -> Some (Some d)
        | None -> None
      in
      make field.name meta decoder encoder default field.type_ validator true
    ;;

    let make_list ?default ?meta field =
      let decoder (l : string List.t) : ('a List.t, Error.error) result =
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

    let make_bool ?default ?meta ?(msg = Error.invalid_bool) name =
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
        ?(msg = Error.invalid_float)
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
        ?(msg = Error.invalid_int)
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

    let make_string
        ?default
        ?meta
        ?(msg = Error.invalid_string)
        ?(validator = always_valid)
        name
      =
      let decoder input =
        try Ok (List.hd input) with
        | _ -> Error msg
      in
      let encoder id = List.[ id ] in
      make name meta decoder encoder default "string" validator false
    ;;

    let make_date
        ?default
        ?meta
        ?(msg = Error.invalid_date)
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
        ?(msg = Error.invalid_datetime)
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

  type error = string * string list * Error.error
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
        | false, List.[] -> List.cons (name, [], Error.no_value) errors
        | _, default ->
          (match Field.validate field default with
          | Some msg -> List.cons (name, [], msg) errors
          | None -> errors))
    in
    fold_left ~f ~init:[] schema |> List.rev
  ;;

  let rec decode
      : type meta ctor ty.
        (meta, ctor, ty) t
        -> (string * string list) list
        -> (ty, error) Result.t
    =
   fun { fields; ctor } fields_assoc ->
    let open! Field in
    match fields with
    | [] -> Ok ctor
    | field :: fields ->
      let handle_missing () =
        match field.decoder [] with
        | Ok value ->
          (match ctor value with
          | ctor -> decode { fields; ctor } fields_assoc
          | exception exn ->
            let msg = Error.of_string (Printexc.to_string exn) in
            Error (field.name, [], msg))
        | Error msg -> Error (field.name, [], msg)
      in
      (match List.assoc field.name fields_assoc with
      | [] ->
        (match field.default with
        | Some value ->
          (match ctor value with
          | ctor -> decode { fields; ctor } fields_assoc
          | exception exn ->
            let msg = Error.of_string (Printexc.to_string exn) in
            Error (field.name, [], msg))
        | None -> handle_missing ())
      | values ->
        (match field.decoder values with
        | Ok value ->
          (match ctor value with
          | ctor -> decode { fields; ctor } fields_assoc
          | exception exn ->
            let msg = Error.of_string (Printexc.to_string exn) in
            Error (field.name, values, msg))
        | Error msg -> Error (field.name, values, msg))
      | exception Not_found ->
        (match field.default, Field.is_optional @@ AnyField field with
        | Some value, _ ->
          (match ctor value with
          | ctor -> decode { fields; ctor } fields_assoc
          | exception exn ->
            let msg = Error.of_string (Printexc.to_string exn) in
            let values =
              match field.default with
              | Some default -> field.encoder default
              | None -> []
            in
            Error (field.name, values, msg))
        | None, false -> Error (field.name, [], Error.no_value)
        | None, true -> handle_missing ()))
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
end
