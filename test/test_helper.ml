type validation_error = (string * string) list

let validation_error_to_sexp e =
  let open Sexplib0.Sexp_conv in
  let open Sexplib0.Sexp in
  List
    (List.map
       (fun (k, v) ->
         List
           [ List [ Atom "name"; sexp_of_string k ]
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

let testable_decode_result testable' =
  Alcotest.(result testable' (triple string (option string) string))
;;
