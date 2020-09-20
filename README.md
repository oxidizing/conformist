[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![Release date][release-date]][release-date]


<br />
<p align="center">
  <!-- <a href="https://github.com/oxidizing/conformist"> -->
  <!--   <img src="images/logo.jpg" alt="Logo" width="400" height="240"> -->
  <!-- </a> -->
  <h3 align="center">Conformist</h3>

  <p align="center">
    Runtime types, schema validation and decoding
    <br />
    <a href="https://oxidizing.github.io/conformist/conformist/Conformist/index.html"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    ·
    <a href="https://github.com/oxidizing/conformist/issues">Report Bug</a>
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

## Usage

dune:

```
(executable
  (name app)
  (libraries
   conformist))
```

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

let user =
  [
    ("gender", [ "male" ]);
    ("email", [ "test@example.com" ]);
    ("birthday", [ "2020-12-01" ]);
    ("nr_of_siblings", [ "3" ]);
    ("comment", [ "hello" ]);
    ("wants_premium", [ "true" ]);
  ]
in
  C.decode Schema.user_schema input

let validation_erorrs =
  [
    ("gender", [ "male" ]);
    ("email", [ "test@example.com" ]);
    ("birthday", [ "2020-12-01" ]);
    ("nr_of_siblings", [ "3" ]);
    ("comment", [ "hello" ]);
    ("wants_premium", [ "true" ]);
  ]
in
  C.validate Schema.user_schema input
```

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

## Documentation

The documentation for the latest released version can be found [here](https://oxidizing.github.io/conformist/conformist/Conformist/index.html).

## License

Copyright (c) 2020 [Oxidizing Systems](https://oxidizing.io/)

Distributed under the MIT License. See `LICENSE` for more information.

## Acknowledgements

The implementation of this project was inspired by [archi](https://github.com/anmonteiro/archi) and [re-web](https://github.com/yawaramin/re-web).

[contributors-shield]: https://img.shields.io/github/contributors/oxidizing/conformist.svg?style=flat-square
[contributors-url]: https://github.com/oxidizing/conformist/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/oxidizing/conformist.svg?style=flat-square
[forks-url]: https://github.com/oxidizing/conformist/network/members
[stars-shield]: https://img.shields.io/github/stars/oxidizing/conformist.svg?style=flat-square
[stars-url]: https://github.com/oxidizing/conformist/stargazers
[issues-shield]: https://img.shields.io/github/issues/oxidizing/conformist.svg?style=flat-square
[issues-url]: https://github.com/oxidizing/conformist/issues
[license-shield]: https://img.shields.io/github/license/oxidizing/conformist.svg?style=flat-square
[license-url]: https://github.com/oxidizing/conformist/blob/master/LICENSE.txt
[release-date]: https://img.shields.io/github/release-date/oxidizing/conformist
