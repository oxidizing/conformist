type gender =
  | Male
  | Female
  | Other
[@@deriving eq, show]

let equal_gender = function
  | Male, Male -> true
  | Female, Female -> true
  | Other, Other -> true
  | _ -> false
;;

let sexp_of_gender g =
  let open Sexplib0.Sexp_conv in
  match g with
  | Male -> sexp_of_string "Male"
  | Female -> sexp_of_string "Female"
  | Other -> sexp_of_string "Other"
;;

type user =
  { gender : gender
  ; name : string
  ; email : string
  ; birthday : Ptime.t
  ; country : string
  ; nr_of_siblings : int
  ; comment : string option
  ; wants_premium : bool
  }

let user_to_sexp
    { gender
    ; name
    ; email
    ; birthday
    ; country
    ; nr_of_siblings
    ; comment
    ; wants_premium
    }
  =
  let open Sexplib0.Sexp_conv in
  let open Sexplib0.Sexp in
  List
    [ List [ Atom "gender"; sexp_of_gender gender ]
    ; List [ Atom "name"; sexp_of_string name ]
    ; List [ Atom "email"; sexp_of_string email ]
    ; List [ Atom "birthday"; sexp_of_string (Ptime.to_rfc3339 birthday) ]
    ; List [ Atom "country"; sexp_of_string country ]
    ; List [ Atom "nr_of_siblings"; sexp_of_int nr_of_siblings ]
    ; List [ Atom "comment"; sexp_of_option sexp_of_string comment ]
    ; List [ Atom "wants_premium"; sexp_of_bool wants_premium ]
    ]
;;

let pp_user fmt t = Sexplib0.Sexp.pp_hum fmt (user_to_sexp t)

let equal_user u1 u2 =
  String.equal
    (Format.asprintf "%a" pp_user u1)
    (Format.asprintf "%a" pp_user u2)
;;

let user gender name email birthday country nr_of_siblings comment wants_premium
  =
  { gender
  ; name
  ; email
  ; birthday
  ; country
  ; nr_of_siblings
  ; comment
  ; wants_premium
  }
;;

let gender_decoder = function
  | "male" -> Ok Male
  | "female" -> Ok Female
  | "other" -> Ok Other
  | _ -> Error "Unknown gender provided"
;;

let gender_encoder = function
  | Male -> "male"
  | Female -> "female"
  | Other -> "other"
;;

let user_schema =
  let module C = Conformist in
  Conformist.make
    Conformist.Field.
      [ C.custom gender_decoder gender_encoder ~default:Female "gender" ~meta:()
      ; C.string "name"
      ; C.string "email"
      ; C.datetime "birthday"
      ; C.string "country"
      ; C.int ~default:0 "nr_of_siblings"
      ; C.optional (C.string "comment")
      ; C.bool ~default:false "wants_premium"
      ]
    user
;;
