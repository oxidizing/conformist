let testable_error = Alcotest.(triple string (list string) string)
let testable_errors = Alcotest.(list testable_error)

let testable_decode_result testable' =
  Alcotest.(result testable' testable_error)
;;

module C = Conformist

(* Testing optional fields *)

type schema_optional =
  { name : string
  ; address : string option
  }

let schema_option_to_sexp s =
  let open Sexplib0.Sexp_conv in
  let open Sexplib0.Sexp in
  List
    [ List [ Atom "name"; sexp_of_string s.name ]
    ; List [ Atom "address"; sexp_of_option sexp_of_string s.address ]
    ]
;;

let pp_schema_optional fmt t =
  Sexplib0.Sexp.pp_hum fmt (schema_option_to_sexp t)
;;

let equal_schema_optional s1 s2 =
  String.equal
    (Format.asprintf "%a" pp_schema_optional s1)
    (Format.asprintf "%a" pp_schema_optional s2)
;;

let testable_schema_optional =
  Alcotest.testable pp_schema_optional equal_schema_optional
;;

let decode_optional () =
  let make name address = { name; address } in
  let schema =
    C.make [ C.string "name"; C.optional (C.string "address") ] make
  in
  Alcotest.(
    check
      (testable_decode_result testable_schema_optional)
      "has no name"
      (Error ("name", [], "No value provided"))
      (C.decode schema []));
  Alcotest.(
    check
      (testable_decode_result testable_schema_optional)
      "decodes"
      (Ok (make "Walter" None))
      (C.decode schema [ "name", [ "Walter" ] ]));
  Alcotest.(
    check
      (testable_decode_result testable_schema_optional)
      "decodes"
      (Ok (make "Walter" (Some "")))
      (C.decode schema [ "name", [ "Walter" ]; "address", [ "" ] ]));
  Alcotest.(
    check
      (testable_decode_result testable_schema_optional)
      "decodes"
      (Ok (make "Walter" None))
      (C.decode schema [ "name", [ "Walter" ]; "address", [] ]));
  let expected = Ok (make "Walter" (Some "Pineapple Street 3")) in
  let actual =
    C.decode
      schema
      [ "name", [ "Walter" ]; "address", [ "Pineapple Street 3" ] ]
  in
  Alcotest.(
    check
      (testable_decode_result testable_schema_optional)
      "decodes"
      expected
      actual);
  let schema =
    C.make
      [ C.string "name"; C.optional (C.string ~default:"Default" "address") ]
      make
  in
  Alcotest.(
    check
      (testable_decode_result testable_schema_optional)
      "decodes"
      (Ok (make "Walter" (Some "Default")))
      (C.decode schema [ "name", [ "Walter" ] ]));
  Alcotest.(
    check
      (testable_decode_result testable_schema_optional)
      "decodes"
      (Ok (make "Walter" (Some "Pineapple Street")))
      (C.decode
         schema
         [ "name", [ "Walter" ]; "address", [ "Pineapple Street" ] ]))
;;

let decode_default () =
  let make name address = { name; address } in
  let schema =
    C.make
      [ C.string ~default:"Walter" "name"
      ; C.optional (C.string ~default:"Default address" "address")
      ]
      make
  in
  Alcotest.(
    check
      (testable_decode_result testable_schema_optional)
      "decodes"
      (Ok (make "Walter" (Some "Default address")))
      (C.decode schema []));
  Alcotest.(
    check
      (testable_decode_result testable_schema_optional)
      "decodes"
      (Ok (make "Walter" (Some "")))
      (C.decode schema [ "address", [ "" ] ]));
  Alcotest.(
    check
      (testable_decode_result testable_schema_optional)
      "decodes"
      (Ok (make "Jesse" (Some "Default address")))
      (C.decode schema [ "name", [ "Jesse" ] ]))
;;

(* Testing multiple fields *)

type schema_multi =
  { name : string
  ; age : int
  }

let schema_multi_to_sexp s =
  let open Sexplib0.Sexp_conv in
  let open Sexplib0.Sexp in
  List
    [ List [ Atom "name"; sexp_of_string s.name ]
    ; List [ Atom "age"; sexp_of_int s.age ]
    ]
;;

let pp_schema_multi fmt t = Sexplib0.Sexp.pp_hum fmt (schema_multi_to_sexp t)

let equal_schema_multi s1 s2 =
  String.equal
    (Format.asprintf "%a" pp_schema_multi s1)
    (Format.asprintf "%a" pp_schema_multi s2)
;;

let testable_schema_multi = Alcotest.testable pp_schema_multi equal_schema_multi

let decode_multi () =
  let make name age = { name; age } in
  let schema = C.make [ C.string "name"; C.int "age" ] make in
  let expected = Error ("name", [], "No value provided") in
  let actual = C.decode schema [] in
  Alcotest.(
    check
      (testable_decode_result testable_schema_multi)
      "has no values"
      expected
      actual);
  let expected = Error ("name", [], "No value provided") in
  let actual = C.decode schema [ "age", [ "foo" ] ] in
  Alcotest.(
    check
      (testable_decode_result testable_schema_multi)
      "has no values"
      expected
      actual);
  let expected = Ok (make "Walter" 33) in
  let actual = C.decode schema [ "name", [ "Walter" ]; "age", [ "33" ] ] in
  Alcotest.(
    check
      (testable_decode_result testable_schema_multi)
      "decodes"
      expected
      actual)
;;

(* Testing complex example from schema.ml *)

let testable_user_form = Alcotest.testable Schema.pp_user Schema.equal_user

let decode_complete_and_invalid_input () =
  let input =
    [ "gender", [ "male" ]
    ; "name", [ "walter" ]
    ; "birthday", [ "2020-12-01T00:00:00.00Z" ]
    ; "email", [ "test@example.com" ]
    ; "country", [ "Switzerland" ]
    ; "nr_of_siblings", [ "fail" ]
    ; "wants_premium", [ "true" ]
    ]
  in
  let actual = C.decode Schema.user_schema input in
  Alcotest.(
    check
      (testable_decode_result testable_user_form)
      "has one error"
      (Error ("nr_of_siblings", List.[ "fail" ], "Invalid number provided"))
      actual)
;;

let decode_complete_and_valid_input () =
  let birthday =
    match Ptime.of_rfc3339 "2020-12-01T00:00:00.00Z" with
    | Ok (time, _, _) -> time
    | Error _ -> failwith "Invalid date provided"
  in
  let input =
    [ "gender", [ "male" ]
    ; "name", [ "walter" ]
    ; "birthday", [ "2020-12-01T00:00:00.00Z" ]
    ; "email", [ "test@example.com" ]
    ; "country", [ "Switzerland" ]
    ; "nr_of_siblings", [ "3" ]
    ; "comment", [ "hello" ]
    ; "favorite_shows", [ "breaking bad"; "better call saul" ]
    ; "wants_premium", [ "true" ]
    ]
  in
  let actual = C.decode Schema.user_schema input in
  let expected =
    Schema.user
      Schema.Male
      "walter"
      "test@example.com"
      birthday
      "Switzerland"
      3
      (Some "hello")
      [ "breaking bad"; "better call saul" ]
      true
  in
  Alcotest.(
    check
      (testable_decode_result testable_user_form)
      "can decode"
      (Ok expected)
      actual)
;;

let validate_default () =
  let make name address = { name; address } in
  let schema =
    C.make [ C.string "name"; C.optional (C.string "address") ] make
  in
  Alcotest.(
    check
      testable_errors
      "no name"
      [ "name", [], "No value provided" ]
      (C.validate schema []));
  Alcotest.(
    check
      testable_errors
      "validates"
      []
      (C.validate schema [ "name", [ "Walter" ] ]));
  Alcotest.(
    check
      testable_errors
      "validates"
      []
      (C.validate
         schema
         [ "name", [ "Walter" ]; "address", [ "Pineapple Street" ] ]))
;;

let validate_incomplete_input () =
  let actual = C.validate Schema.user_schema [] in
  Alcotest.(
    check
      testable_errors
      "has error"
      [ "name", [], "No value provided"
      ; "email", [], "No value provided"
      ; "birthday", [], "No value provided"
      ; "country", [], "No value provided"
      ])
    actual;
  let actual =
    C.validate
      Schema.user_schema
      [ "gender", [ "foo" ]; "birthday", [ "2020-12-01T00:00:00.00Z" ] ]
  in
  Alcotest.(
    check
      testable_errors
      "has error"
      [ "gender", List.[ "foo" ], "Unknown gender provided"
      ; "name", [], "No value provided"
      ; "email", [], "No value provided"
      ; "country", [], "No value provided"
      ]
      actual)
;;

let validate_complete_input () =
  let input =
    [ "gender", [ "male" ]
    ; "name", [ "walter" ]
    ; "birthday", [ "2020-12-01T00:00:00.00Z" ]
    ; "email", [ "test@example.com" ]
    ; "country", [ "Switzerland" ]
    ; "nr_of_siblings", [ "3" ]
    ; "comment", [ "hello" ]
    ; "wants_premium", [ "true" ]
    ]
  in
  let actual = C.validate Schema.user_schema input in
  Alcotest.(check testable_errors "can validate" [] actual)
;;

let decode_and_validate_incomplete_input () =
  let actual = C.decode_and_validate Schema.user_schema [] in
  Alcotest.(
    check
      (result testable_user_form testable_errors)
      "has error"
      (Error
         [ "name", [], "No value provided"
         ; "email", [], "No value provided"
         ; "birthday", [], "No value provided"
         ; "country", [], "No value provided"
         ]))
    actual;
  let actual =
    C.decode_and_validate
      Schema.user_schema
      [ "gender", [ "foo" ]; "birthday", [ "2020-12-01T00:00:00.00Z" ] ]
  in
  Alcotest.(
    check
      (result testable_user_form testable_errors)
      "has error"
      (Error
         [ "gender", List.[ "foo" ], "Unknown gender provided"
         ; "name", [], "No value provided"
         ; "email", [], "No value provided"
         ; "country", [], "No value provided"
         ])
      actual)
;;

let decode_and_validate_complete_and_valid_input () =
  let input =
    [ "gender", [ "male" ]
    ; "name", [ "walter" ]
    ; "birthday", [ "2020-12-01T00:00:00.00Z" ]
    ; "email", [ "test@example.com" ]
    ; "country", [ "Switzerland" ]
    ; "nr_of_siblings", [ "3" ]
    ; "comment", [ "hello" ]
    ; "favorite_shows", [ "breaking bad"; "better call saul" ]
    ; "wants_premium", [ "true" ]
    ]
  in
  let actual = C.decode_and_validate Schema.user_schema input in
  let birthday =
    match Ptime.of_rfc3339 "2020-12-01T00:00:00.00Z" with
    | Ok (time, _, _) -> time
    | Error _ -> failwith "Invalid date provided"
  in
  Alcotest.(
    check
      (result testable_user_form testable_errors)
      "can validate"
      (Ok
         Schema.
           { gender = Schema.Male
           ; name = "walter"
           ; email = "test@example.com"
           ; birthday
           ; country = "Switzerland"
           ; nr_of_siblings = 3
           ; comment = Some "hello"
           ; favorite_shows = [ "breaking bad"; "better call saul" ]
           ; wants_premium = true
           })
      actual)
;;

type datetime = { datetime : Ptime.t }

let create_datetime datetime = { datetime }
let datetime = C.make [ C.datetime ~meta:() "datetime" ] create_datetime

let decode_and_validate_datetime () =
  let input = [ "datetime", [ "invalid datetime" ] ] in
  let expected =
    "datetime", [ "invalid datetime" ], "Invalid datetime provided"
  in
  let actual = C.decode datetime input |> Result.get_error in
  Alcotest.(check testable_error "invalid date" expected actual);
  let input = [ "datetime", [ "2020-12-01" ] ] in
  let expected = "datetime", [ "2020-12-01" ], "Invalid datetime provided" in
  let actual = C.decode datetime input |> Result.get_error in
  Alcotest.(check testable_error "invalid date" expected actual);
  let input = [ "datetime", [ "2020-12-01T00:00:00.00Z" ] ] in
  let actual = Result.is_ok (C.decode datetime input) in
  Alcotest.(check bool "valid date" true actual)
;;

module Custom_error = struct
  type error = bool

  let invalid_bool = false
  let invalid_float = false
  let invalid_int = false
  let invalid_string = false
  let invalid_date = false
  let invalid_datetime = false
  let no_value = false
  let of_string _ = false
end

module C_custom = Conformist.Make (Custom_error)

type some_type = { field : bool }

let create_some_type field = { field }

let some_type =
  C_custom.make [ C_custom.bool ~meta:() "field" ] create_some_type
;;

let fail_with_custom_error () =
  let actual =
    C_custom.decode_and_validate some_type [ "field", [ "true" ] ]
    |> Result.get_ok
  in
  Alcotest.(check bool "decoded type" true actual.field);
  let _, _, actual =
    C_custom.decode_and_validate some_type [ "field", [ "invalid" ] ]
    |> Result.get_error
    |> List.hd
  in
  Alcotest.(check bool "custom error type" false actual)
;;

let () =
  let open Alcotest in
  run
    "conformist"
    [ ( "decode"
      , [ test_case "optional" `Quick decode_optional
        ; test_case "default" `Quick decode_default
        ; test_case "multi" `Quick decode_multi
        ; test_case "invalid input" `Quick decode_complete_and_invalid_input
        ; test_case "valid input" `Quick decode_complete_and_valid_input
        ] )
    ; ( "validate"
      , [ test_case "default" `Quick validate_default
        ; test_case "incomplete input" `Quick validate_incomplete_input
        ; test_case "complete input" `Quick validate_complete_input
        ] )
    ; ( "decode and validate"
      , [ test_case
            "incomplete input"
            `Quick
            decode_and_validate_incomplete_input
        ; test_case
            "complete and valid input"
            `Quick
            decode_and_validate_complete_and_valid_input
        ; test_case "datetime" `Quick decode_and_validate_datetime
        ] )
    ; ( "decode and validate with custom error type"
      , [ test_case "custom error type" `Quick fail_with_custom_error ] )
    ]
;;
