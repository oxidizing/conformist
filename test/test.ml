module C = Conformist

(* Tell alcotest how to pretty print and check for equality *)

let testable_form = Alcotest.testable Schema.pp_user Schema.equal_user

type validation_error = (string * string) list

let validation_error_to_sexp e =
  let open Sexplib0.Sexp_conv in
  let open Sexplib0.Sexp in
  List
    (List.map
       (fun (k, v) ->
         List
           [ List [ Atom "key"; sexp_of_string k ]
           ; List [ Atom "error"; sexp_of_string v ]
           ])
       e)
;;

let pp_validation_error fmt t =
  Sexplib0.Sexp.pp_hum fmt (validation_error_to_sexp t)
;;

let equal_validation_error e1 e2 =
  String.equal
    (Format.asprintf "%a" pp_validation_error e1)
    (Format.asprintf "%a" pp_validation_error e2)
;;

let testable_validation_error =
  Alcotest.testable pp_validation_error equal_validation_error
;;

let testable_decode_result = Alcotest.result testable_form Alcotest.string

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
      (result testable_schema_optional string)
      "has no name"
      (Error "Failed to decode field 'name': No value provided")
      (C.decode schema []));
  Alcotest.(
    check
      (result testable_schema_optional string)
      "decodes"
      (Ok (make "Walter" None))
      (C.decode schema [ "name", [ "Walter" ] ]));
  Alcotest.(
    check
      (result testable_schema_optional string)
      "decodes"
      (Ok (make "Walter" (Some "")))
      (C.decode schema [ "name", [ "Walter" ]; "address", [ "" ] ]));
  Alcotest.(
    check
      (result testable_schema_optional string)
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
    check (result testable_schema_optional string) "decodes" expected actual);
  let schema =
    C.make
      [ C.string "name"; C.optional (C.string ~default:"Default" "address") ]
      make
  in
  Alcotest.(
    check
      (result testable_schema_optional string)
      "decodes"
      (Ok (make "Walter" (Some "Default")))
      (C.decode schema [ "name", [ "Walter" ] ]));
  Alcotest.(
    check
      (result testable_schema_optional string)
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
      (result testable_schema_optional string)
      "decodes"
      (Ok (make "Walter" (Some "Default address")))
      (C.decode schema []));
  Alcotest.(
    check
      (result testable_schema_optional string)
      "decodes"
      (Ok (make "Walter" (Some "")))
      (C.decode schema [ "address", [ "" ] ]));
  Alcotest.(
    check
      (result testable_schema_optional string)
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
  let expected = Error "Failed to decode field 'name': No value provided" in
  let actual = C.decode schema [] in
  Alcotest.(
    check (result testable_schema_multi string) "has no values" expected actual);
  let expected = Error "Failed to decode field 'name': No value provided" in
  let actual = C.decode schema [ "age", [ "foo" ] ] in
  Alcotest.(
    check (result testable_schema_multi string) "has no values" expected actual);
  let expected = Ok (make "Walter" 33) in
  let actual = C.decode schema [ "name", [ "Walter" ]; "age", [ "33" ] ] in
  Alcotest.(
    check (result testable_schema_multi string) "decodes" expected actual)
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
         "Failed to decode value 'nr_of_siblings' of field 'fail': Invalid \
          number provided")
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

let validate_default () =
  let make name address = { name; address } in
  let schema =
    C.make [ C.string "name"; C.optional (C.string "address") ] make
  in
  Alcotest.(
    check
      testable_validation_error
      "no name"
      [ "name", "No value provided" ]
      (C.validate schema []));
  Alcotest.(
    check
      testable_validation_error
      "validates"
      []
      (C.validate schema [ "name", [ "Walter" ] ]));
  Alcotest.(
    check
      testable_validation_error
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
      testable_validation_error
      "has error"
      [ "name", "No value provided"
      ; "email", "No value provided"
      ; "birthday", "No value provided"
      ; "country", "No value provided"
      ])
    actual;
  let actual =
    C.validate
      Schema.user_schema
      [ "gender", [ "foo" ]; "birthday", [ "2000-10-23" ] ]
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
    ]
;;
