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

include Core.CONFORMIST with type error_msg = string

module Make : functor (M : Core.ERROR) ->
  Core.CONFORMIST with type error_msg = M.error
