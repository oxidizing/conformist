type ('meta, 'a) t = {
  actual : 'a;
  meta : 'meta;
  counter : int;
  validate : string -> bool;
}

type (_, _, _) list =
  | [] : ('meta, 'ty, 'ty) list
  | ( :: ) :
      ('meta, 'a) t * ('meta, 'b, 'ty) list
      -> ('meta, 'a -> 'b, 'ty) list

type _ any_field = AnyField : ('meta, 'a) t -> 'meta any_field

let meta (AnyField field) = field.meta

let counter (AnyField field) = field.counter

let validate (AnyField field) string = field.validate string

let rec fold_left :
    type ty args.
    f:('res -> 'meta any_field -> 'res) ->
    init:'res ->
    ('meta, args, ty) list ->
    'res =
 fun ~f ~init fields ->
  match fields with
  | [] -> init
  | field :: fields -> fold_left ~f ~init:(f init (AnyField field)) fields

let foo =
  [
    { actual = "foo"; meta = true; counter = 1; validate = (fun _ -> false) };
    { actual = 4; meta = false; counter = 2; validate = (fun _ -> true) };
  ]

let bool = fold_left ~f:(fun acc next -> acc && meta next) ~init:false foo

let meta = fold_left ~f:(fun acc next -> acc + counter next) ~init:0 foo
