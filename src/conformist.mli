(** Conformist is a library for creating and validating schemas. It provides run-time types without using any ppx. It can be used to validate incoming data and to translate it to static types for safe usage. *)

(** {1 Example}

Let's start with an example. We have a static type that represents a user.

{[
type gender = Male | Female | Other

type user = {
  gender : gender;
  email : string;
  birthday : int * int * int;
  nr_of_siblings : int;
  comment : string option;
  wants_premium : bool;
}
]}

In order to create a conformist schema, we need a constructor that takes all the record fields and create a [user].

{[
let user gender email birthday nr_of_siblings comment wants_premium
    =
  {
    gender;
    email;
    birthday;
    nr_of_siblings;
    comment;
    wants_premium;
  }
]}

Now we can create a schema.

{[
let gender_decoder = function
  | "male" -> Ok Male
  | "female" -> Ok Female
  | "other" -> Ok Other
  | _ -> Error "Unknown gender provided"

module C = Conformist

let user_schema =
  C.make
    C.Field.
      [
        C.custom "gender" gender_decoder ~meta:() ();
        C.string "email" ();
        C.date "birthday" ();
        C.int "nr_of_siblings" ();
        C.optional (C.string "comment" ());
        C.bool "wants_premium" ();
      ]
    user
]}

Try to delete/swap lines of that list, to change the constructor or the [user ] type. The code doesn't compile anymore!

[user_schema] showcases the creation of a custom type and optional types.

This is how you can decode a user given some input:

{[
let input =
  [
    ("gender", [ "male" ]);
    ("email", [ "test@example.com" ]);
    ("birthday", [ "2020-12-01" ]);
    ("nr_of_siblings", [ "3" ]);
    ("comment", [ "hello" ]);
    ("wants_premium", [ "true" ]);
  ]
  in
  C.decode Schema.user_schema input in
]}

Decoding doesn't validate the data, it just makes sure that the types are correct and translates strings to the correct static types.

We can validate data based on our validator per field.

{[
let input =
  [
    ("gender", [ "male" ]);
    ("email", [ "test@example.com" ]);
    ("birthday", [ "2020-12-01" ]);
    ("nr_of_siblings", [ "3" ]);
    ("comment", [ "hello" ]);
    ("wants_premium", [ "true" ]);
  ]
  in
  C.validate Schema.user_schema input in
]}

Note that if decoding of a field fails, validation fails as well since before a field is validated it gets decoded.
*)

(** {1 Fields}

Every member of the list in the example is a field. Use the provided [fold_left] to traverse the list of fiels. Helper functions are provided that operate on fields.

*)

module Field : sig
  type ('meta, 'a) t
  (** A field of type [('meta, 'a) t] represents the static type ['a] and it can hold arbitrary meta data of type ['meta]. That meta data can be used to build functionality on top of conformist. *)

  type (_, _, _) list =
    | [] : ('meta, 'ty, 'ty) list
    | ( :: ) :
        ('meta, 'a) t * ('meta, 'b, 'ty) list
        -> ('meta, 'a -> 'b, 'ty) list
        (** A [list] is a list of fields. Note that this is not the list from [List.t] so make sure to open this scope locally when defining a list of fields. *)

  type _ any_field = AnyField : ('meta, 'a) t -> 'meta any_field

  val meta : 'a any_field -> 'a option
  (** [meta field] returns an optional meta data of a [field]. This can be used to store arbitrary meta data in each field. Note that the type of the meta data has to be the same for all fields. *)

  val name : 'a any_field -> string
  (** [name field] returns the name of the [field], which uniquely identifies the field within one schema. *)

  val validate : 'a any_field -> string -> string option
  (** [validate field string] decodes a [string] and runs the [field]'s validation logic on the decoded value. Both decoding and validation might fail, which results in an error string. *)

  val optional : 'a any_field -> bool
  (** [optional field] turns a [field] into an optional field. This means that input that doesn't contain a value for the field will yield in a valid field. *)

  val fold_left :
    f:('res -> 'meta any_field -> 'res) ->
    init:'res ->
    ('meta, 'args, 'ty) list ->
    'res
  (** [fold_left ~f ~init fields] can be used to traverse a lit of fields. Use the functions [meta], [nae], [validate] and [optional] in f. *)
end

type 'a decoder = string -> ('a, string) result
(** A ['a decoder] tries to turn a string into a value of type ['a]. It returns a descriptive errors message upon failure. *)

type 'a validator = 'a -> string option
(** A ['a validator] takes something of type ['a] and returns an error string if validation fails, [None] if everything is ok *)

val custom :
  string ->
  'a decoder ->
  ?meta:'b ->
  ?validator:'a validator ->
  unit ->
  ('b, 'a) Field.t
(** Use [custom field_name decoder ?meta ?validator ()] to create a field with a custom type that is not supported out-of-the box. Provide a custom [decoder] with a descriptive error message so conformist knows how to turn a string into your custom value. *)

val optional : ?meta:'a -> ('b, 'c) Field.t -> ('a, 'c option) Field.t
(** Use [optional ?meta field] to turn any field into an optional value. Note that the field must still be contained in the final input (when decoding or validating), but it can be an empty list or an empty string. If the data is not provided in the input, no validation logic is executed. *)

val bool : string -> ?meta:'a -> ?msg:string -> unit -> ('a, bool) Field.t
(** [bool name ?meta ?msg ()] creates a field with [name] some [meta] data and a custom decode error message [msg] that decodes to a boolean. *)

val float :
  string ->
  ?meta:'a ->
  ?msg:string ->
  ?validator:float validator ->
  unit ->
  ('a, float) Field.t
(** [float name ?meta ?msg ?validator ()] creates a field that decodes to a float with [name] some [meta] data, a custom decode error message [msg] and a [validator]. *)

val int :
  string ->
  ?meta:'a ->
  ?msg:string ->
  ?validator:int validator ->
  unit ->
  ('a, int) Field.t
(** [int name ?meta ?msg ?validator ()] creates a field that decodes to a int with [name] some [meta] data, a custom decode error message [msg] and a [validator]. *)

val string :
  string ->
  ?meta:'a ->
  ?validator:string validator ->
  unit ->
  ('a, string) Field.t
(** [string name ?meta ?validator ()] creates a field that decodes to a string with [name] some [meta] data and a [validator]. Note that this field does not need to be decoded, but it can still be validated. *)

type date = int * int * int
(** Valid date example: 2020-11-25, this type is compatible with Ptime.date *)

val date :
  string ->
  ?meta:'a ->
  ?msg:string ->
  ?validator:(int * int * int) validator ->
  unit ->
  ('a, date) Field.t
(** [string name ?meta ?validator ()] creates a field that decodes to a date with [name] some [meta] data and a [validator]. *)

(** {1 Schema}

A schema is a list of fields. Input data can be decoded and validated using a schema.
*)

type ('meta, 'ctor, 'ty) t
(** [t] is a conformist schema. *)

val empty : ('a, unit, unit) t
(** [empty] creates an empty schema. *)

val make : ('a, 'b, 'c) Field.list -> 'b -> ('a, 'b, 'c) t
(** [make fields constructor] create a schema. *)

type validation_error = (string * string) list
(** An empty [validation_error] means that the schema is valid. *)

type input = (string * string list) list
(** The [input] represents unsafe data that needs to be validated and decoded. This is typically some user input or other data that needs to be sanitization. *)

val decode : ('meta, 'ctor, 'ty) t -> input -> ('ty, string) result
(** [decode schema input] tries to create a value of the static type ['ty]. Note that a successfully decoded value means that the strings contain the expected types, but no validation logic was executed. *)

val validate : ('meta, 'ctor, 'ty) t -> input -> validation_error
(** [validate schema input] runs the field validators on decoded data. Note that a field that fails to decode will also fail validation, but a decoded field might still fail validation. *)
