(** Conformist is a library for creating and validating schemas. It provides
    run-time types without the use of ppx. It can be used to validate incoming
    data and to translate it to static types for safe usage. *)

(** {1 Example}

    Let's start with an example. We have a static type that represents a user.

    {[
      type occupation =
        | Mathematician
        | Engineer

      type user =
        { occupation : occupation
        ; email : string
        ; birthday : Ptime.t
        ; nr_of_siblings : int
        ; comment : string option
        ; wants_premium : bool
        }
    ]}

    In order to create a conformist schema, we need a constructor that takes all
    the record fields and create a [user].

    {[
      let user occupation email birthday nr_of_siblings comment wants_premium =
        { occupation; email; birthday; nr_of_siblings; comment; wants_premium }
      ;;
    ]}

    Now we can create a schema.

    {[
      let occupation_decoder = function
        | "mathematician" -> Ok Mathematician
        | "engineer" -> Ok Engineer
        | _ -> Error "Unknown occupation provided"
      ;;

      let occupation_encoder = function
        | Mathematician -> "mathematician"
        | Engineer -> "engineer"
      ;;

      let user_schema =
        Conformist.(
          make
            Field.
              [ custom
                  occupation_decoder
                  occupation_encoder
                  "occupation"
                  ~meta:()
              ; string "email"
              ; date "birthday"
              ; int ~default:0 "nr_of_siblings"
              ; optional (string "comment")
              ; bool "wants_premium"
              ]
            user)
      ;;
    ]}

    Try to delete/swap lines of that list, to change the constructor or the
    [user] type. The code doesn't compile anymore!

    [user_schema] showcases the creation of a custom type and optional types.

    This is how you can decode a user given some input:

    {[
      let user =
        let input =
          [ "occupation", [ "engineer" ]
          ; "email", [ "test@example.com" ]
          ; "birthday", [ "2020-12-01T00:00:00.00Z" ]
          ; "nr_of_siblings", [ "3" ]
          ; "comment", [ "hello" ]
          ; "wants_premium", [ "true" ]
          ]
        in
        Conformist.decode Schema.user_schema input
      ;;
    ]}

    Decoding doesn't validate the data, it just makes sure that the types are
    correct and translates strings to the correct static types.

    We can validate data based on each field's validators.

    {[
      let validation_errors =
        let input =
          [ "occupation", [ "engineer" ]
          ; "email", [ "test@example.com" ]
          ; "birthday", [ "2020-12-01T00:00:00.00Z" ]
          ; "nr_of_siblings", [ "3" ]
          ; "comment", [ "hello" ]
          ; "wants_premium", [ "true" ]
          ]
        in
        Conformist.validate Schema.user_schema input
      ;;
    ]}

    Note that if decoding of a field fails, validation fails as well since
    before a field is validated it gets decoded.

    Use {!decode_and_validate} to do both steps. *)

(** {1 Fields}

    Every member of the list in the example is a field. Use the provided
    [fold_left] to traverse the list of fiels. Helper functions are provided
    that operate on fields. *)

module Field : sig
  (** A field of type [('meta, 'a) t] represents the static type ['a] and it can
      hold arbitrary meta data of type ['meta]. That meta data can be used to
      build functionality on top of conformist. *)
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

  (** [meta field] returns an optional meta data of a [field]. This can be used
      to store arbitrary meta data in each field. Note that the type of the meta
      data has to be the same for all fields. *)
  val meta : 'a any_field -> 'a option

  (** [name field] returns the name of the [field], which uniquely identifies
      the field within one schema. *)
  val name : 'a any_field -> string

  (** [validate field string] decodes a [string] and runs the [field]'s
      validation logic on the decoded value. Both decoding and validation might
      fail, which results in an error string. *)
  val validate : 'a any_field -> string -> string option

  (** [optional field] turns a [field] into an optional field. This means that
      input that doesn't contain a value for the field will yield in a valid
      field. *)
  val optional : 'a any_field -> bool

  (** [type_ field] returns a string representation of the type of [field]. *)
  val type_ : 'a any_field -> string

  (** [encode_default field] tries to encode the default value if present and to
      return it as string. *)
  val encode_default : 'a any_field -> string option
end

(** A ['a decoder] tries to turn a string into a value of type ['a]. It returns
    a descriptive errors message upon failure. *)
type 'a decoder = string -> ('a, string) result

(** A ['a encoder] encodes a value of type ['a] into a string. *)
type 'a encoder = 'a -> string

(** A ['a validator] takes something of type ['a] and returns an error string if
    validation fails, [None] if everything is ok *)
type 'a validator = 'a -> string option

(** Use [custom decoder encoder ?default ?type_ ?meta ?validator field_name] to
    create a field with a custom type that is not supported out-of-the box.
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

(** [optional ?meta field] turns a [field] into an optional field. If the field
    does not exist in the input data or if the associated value in the input
    data is an empty list, the value is [None]. If the data is not provided in
    the input at all, no validation logic is executed.

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

(** [bool ?default ?meta ?msg field_name] returns a field with name [field_name]
    that decodes to a [bool].

    [default] is an optional default value for the field.

    [meta] is optional meta data that is attached to the field. This is useful
    when implementing features on top of conformist.

    [msg] is the decode error message that is returned if {!decode} fails. *)
val bool
  :  ?default:bool
  -> ?meta:'a
  -> ?msg:string
  -> string
  -> ('a, bool) Field.t

(** [float ?default ?meta ?msg ?validator field_name] returns a field with name
    [field_name] that decodes to [float].

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
  -> ?msg:string
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
  -> ?msg:string
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
  -> ?validator:string validator
  -> string
  -> ('a, string) Field.t

(** Don't use [date], use {!datetime} instead.*)
val date
  :  ?default:Ptime.date
  -> ?meta:'a
  -> ?msg:string
  -> ?validator:(int * int * int) validator
  -> string
  -> ('a, Ptime.date) Field.t
  [@@ocaml.deprecated "Use [Conformist.datetime] instead."]

(** [datetime ?default ?meta ?validator field_name] returns a field with name
    [field_name] that decodes to [string].

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
  -> ?msg:string
  -> ?validator:Ptime.t validator
  -> string
  -> ('a, Ptime.t) Field.t

(** {1 Schema}

    A schema is a list of fields. Input data can be decoded and validated using
    a schema. *)

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

(** An error [(field, value, error_message)] is used to for decoding errors and
    validation errors.

    [field] is the field name of the input that failed to decode or validate,
    [value] is the input value (if one was provided) and [error_message] is the
    decoding or validation error message.

    An empty list of [error] means that the schema is valid. *)
type error = string * string option * string

(** The [input] represents unsafe data that needs to be decoded and validated.
    This is typically some user input. *)
type input = (string * string list) list

(** [decode schema input] returns the decoded value of type ['ty] by decoding
    the [input] using the [schema].

    No validation logic is executed in this step. *)
val decode : ('meta, 'ctor, 'ty) t -> input -> ('ty, error) result

(** [validate schema input] returns a list of validation errors by running the
    validators defined in [schema] on the [input] data. An empty list implies
    that there are no validation errors and that the input is valid according to
    the schema.

    Note that [input] that has no validation errors might still fail to decode,
    depending on the validation functions specified in [schema]. *)
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
