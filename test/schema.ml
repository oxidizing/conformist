module C = Conformist

type gender = Male | Female | Other [@@deriving eq, show]

type user = {
  gender : gender;
  name : string;
  email : string;
  birthday : int * int * int;
  country : string;
  nr_of_siblings : int;
  comment : string option;
  wants_premium : bool;
}
[@@deriving eq, show]

let user gender name email birthday country nr_of_siblings comment wants_premium
    =
  {
    gender;
    name;
    email;
    birthday;
    country;
    nr_of_siblings;
    comment;
    wants_premium;
  }

let gender_decoder = function
  | "male" -> Ok Male
  | "female" -> Ok Female
  | "other" -> Ok Other
  | _ -> Error "Unknown gender provided"

let user_schema =
  C.make
    C.Field.
      [
        C.custom "gender" gender_decoder ~meta:() ();
        C.string "name" ();
        C.string "email" ();
        C.date "birthday" ();
        C.string "country" ();
        C.int "nr_of_siblings" ();
        C.optional (C.string "comment" ());
        C.bool "wants_premium" ();
      ]
    user