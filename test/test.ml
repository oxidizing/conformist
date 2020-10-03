module C = Conformist

(* Tell alcotest how to pretty print and check for equality *)

let testable_form = Alcotest.testable Schema.pp_user Schema.equal_user

type validation_error = (string * string) list [@@deriving show, eq]

let testable_validation_error =
  Alcotest.testable pp_validation_error equal_validation_error
;;

let testable_decode_result = Alcotest.result testable_form Alcotest.string

(* Testing optional fields *)

type schema_optional =
  { name : string
  ; address : string option
  }
[@@deriving show, eq]

let testable_schema_optional = Alcotest.testable pp_schema_optional equal_schema_optional

let decode_optional () =
  let make name address = { name; address } in
  let schema = C.make [ C.string "name"; C.optional (C.string "address") ] make in
  Alcotest.(
    check
      (result testable_schema_optional string)
      "has no name"
      (Error "Failed to decode field 'name': No value provided")
      (C.decode schema []));
  Alcotest.(
    check
      (result testable_schema_optional string)
      "decodes"
      (Ok (make "Walter" None))
      (C.decode schema [ "name", [ "Walter" ]; "address", [ "" ] ]));
  Alcotest.(
    check
      (result testable_schema_optional string)
      "decodes"
      (Ok (make "Walter" None))
      (C.decode schema [ "name", [ "Walter" ]; "address", [] ]));
  let expected = Ok (make "Walter" (Some "Pineapple Street 3")) in
  let actual =
    C.decode schema [ "name", [ "Walter" ]; "address", [ "Pineapple Street 3" ] ]
  in
  Alcotest.(check (result testable_schema_optional string) "decodes" expected actual)
;;

let decode_default () =
  let make name address = { name; address } in
  let schema =
    C.make [ C.string ~default:"Walter" "name"; C.optional (C.string "address") ] make
  in
  Alcotest.(
    check
      (result testable_schema_optional string)
      "decodes"
      (Ok (make "Walter" None))
      (C.decode schema [ "address", [ "" ] ]));
  Alcotest.(
    check
      (result testable_schema_optional string)
      "decodes"
      (Ok (make "Jesse" None))
      (C.decode schema [ "name", [ "Jesse" ]; "address", [ "" ] ]))
;;

(* Testing multiple fields *)

type schema_multi =
  { name : string
  ; age : int
  }
[@@deriving show, eq]

let testable_schema_multi = Alcotest.testable pp_schema_multi equal_schema_multi

let decode_multi () =
  let make name age = { name; age } in
  let schema = C.make [ C.string "name"; C.int "age" ] make in
  let expected = Error "Failed to decode field 'name': No value provided" in
  let actual = C.decode schema [] in
  Alcotest.(check (result testable_schema_multi string) "has no values" expected actual);
  let expected = Error "Failed to decode field 'name': No value provided" in
  let actual = C.decode schema [ "age", [ "foo" ] ] in
  Alcotest.(check (result testable_schema_multi string) "has no values" expected actual);
  let expected = Ok (make "Walter" 33) in
  let actual = C.decode schema [ "name", [ "Walter" ]; "age", [ "33" ] ] in
  Alcotest.(check (result testable_schema_multi string) "decodes" expected actual)
;;

(* Testing complex example from schema.ml *)

let decode_complete_and_invalid_input () =
  let input =
    [ "gender", [ "male" ]
    ; "name", [ "walter" ]
    ; "birthday", [ "2020-12-01" ]
    ; "email", [ "test@example.com" ]
    ; "country", [ "Switzerland" ]
    ; "nr_of_siblings", [ "fail" ]
    ; "wants_premium", [ "true" ]
    ]
  in
  let actual = C.decode Schema.user_schema input in
  Alcotest.(
    check
      testable_decode_result
      "has one error"
      (Error
         "Failed to decode value 'nr_of_siblings' of field 'fail': Invalid number \
          provided")
      actual)
;;

let decode_complete_and_valid_input () =
  let input =
    [ "gender", [ "male" ]
    ; "name", [ "walter" ]
    ; "birthday", [ "2020-12-01" ]
    ; "email", [ "test@example.com" ]
    ; "country", [ "Switzerland" ]
    ; "nr_of_siblings", [ "3" ]
    ; "comment", [ "hello" ]
    ; "wants_premium", [ "true" ]
    ]
  in
  let actual = C.decode Schema.user_schema input in
  let expected =
    Schema.user
      Schema.Male
      "walter"
      "test@example.com"
      (2020, 12, 01)
      "Switzerland"
      3
      (Some "hello")
      true
  in
  Alcotest.(check testable_decode_result "can decode" (Ok expected) actual)
;;

let validate_incomplete_input () =
  let actual = C.validate Schema.user_schema [] in
  Alcotest.(
    check
      testable_validation_error
      "has error"
      [ "name", "No value provided"
      ; "email", "No value provided"
      ; "birthday", "No value provided"
      ; "country", "No value provided"
      ])
    actual;
  let actual =
    C.validate Schema.user_schema [ "gender", [ "foo" ]; "birthday", [ "2000-10-23" ] ]
  in
  Alcotest.(
    check
      testable_validation_error
      "has error"
      [ "gender", "Unknown gender provided"
      ; "name", "No value provided"
      ; "email", "No value provided"
      ; "country", "No value provided"
      ]
      actual)
;;

let validate_complete_input () =
  let input =
    [ "gender", [ "male" ]
    ; "name", [ "walter" ]
    ; "birthday", [ "2020-12-01" ]
    ; "email", [ "test@example.com" ]
    ; "country", [ "Switzerland" ]
    ; "nr_of_siblings", [ "3" ]
    ; "comment", [ "hello" ]
    ; "wants_premium", [ "true" ]
    ]
  in
  let actual = C.validate Schema.user_schema input in
  Alcotest.(check testable_validation_error "can validate" [] actual)
;;

let () =
  let open Alcotest in
  run
    "conformist"
    [ ( "decode data"
      , [ test_case "optional" `Quick decode_optional
        ; test_case "default" `Quick decode_default
        ; test_case "multi" `Quick decode_multi
        ; test_case "invalid input" `Quick decode_complete_and_invalid_input
        ; test_case "valid input" `Quick decode_complete_and_valid_input
        ] )
    ; ( "validate date"
      , [ test_case "incomplete input" `Quick validate_incomplete_input
        ; test_case "complete input" `Quick validate_complete_input
        ] )
    ]
;;
