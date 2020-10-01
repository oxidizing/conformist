<br />
<p align="center">
  <!-- <a href="https://github.com/oxidizing/conformist"> -->
  <!--   <img src="images/logo.jpg" alt="Logo" width="400" height="240"> -->
  <!-- </a> -->
  <h3 align="center">Conformist</h3>

  <p align="center">
    Schema definition and validation with support for decoding to bridge the gap between runtime types and static types.
    <br />
    <a href="https://oxidizing.github.io/conformist/conformist/Conformist/index.html"><strong>Explore the docs »</strong></a>
  </p>
</p>

<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About](#about)
* [Installation](#installation)
* [Usage](#usage)
* [Documentation](#documentation)
* [License](#license)
* [Acknowledgements](#acknowledgements)

## About

Conformist allows you to define schemas to decode, validate and sanitize input data declaratively. It comes with runtime types for primitive OCaml types such as `int`, `string`, `bool` and `float` but also `Ptime.date`, optional and custom types. Re-use business rules in validators and run it on the client side with [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml/). Arbitrary meta data can be stored in schemas which is useful to build functionality on top of conformist.

Typical use cases are enforcing invariants of models or user input sanitization.

In essence, conformist helps you to keep your runtime types/contracts in sync with your static types.

## Installation

```sh
opam install conformist
```

In your `dune` file:

```
(executable
  (name app)
  (libraries
   ...
   conformist))
```

## Usage

Let's look at an example.

```ocaml
module C = Conformist

type gender = Male | Female | Other

type user = {
  gender : gender;
  email : string;
  birthday : int * int * int;
  nr_of_siblings : int;
  comment : string option;
  wants_premium : bool;
}

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

let gender_decoder = function
  | "male" -> Ok Male
  | "female" -> Ok Female
  | "other" -> Ok Other
  | _ -> Error "Unknown gender provided"

let gender_encoder = function
  | Male -> "male"
  | Female -> "female"
  | Other -> "other"

let user_schema =
  C.make
    C.Field.
      [
        C.custom gender_decoder ~meta:() "gender";
        C.string "email";
        C.date "birthday";
        C.int "nr_of_siblings";
        C.optional (C.string "comment");
        C.bool "wants_premium";
      ]
    user

let input =
  [
    ("gender", [ "male" ]);
    ("email", [ "test@example.com" ]);
    ("birthday", [ "2020-12-01" ]);
    ("nr_of_siblings", [ "3" ]);
    ("comment", [ "hello" ]);
    ("wants_premium", [ "true" ]);
  ]

let user =
  C.decode Schema.user_schema input

let validation_errors =
  C.validate Schema.user_schema input
```

Try to delete/swap some lines of the list of fields, to change the constructor or the user type. The compiler forces you to keep these three things in sync.

Decoding doesn't validate the data, it just makes sure that the types are correct and translates strings to the correct static types.

Note that if decoding of a field fails, validation fails as well. Before a field is validated, it gets decoded.

## Documentation

The documentation for the latest released version can be found [here](https://oxidizing.github.io/conformist/conformist/Conformist/index.html).

## License

Copyright (c) 2020 [Oxidizing Systems](https://oxidizing.io/)

Distributed under the MIT License. See `LICENSE` for more information.

## Acknowledgements

The implementation of this project was inspired by [archi](https://github.com/anmonteiro/archi) and [re-web](https://github.com/yawaramin/re-web).
