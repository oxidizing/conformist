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

## Installation

Follow the steps to get started with a minimal running web server.

### Prerequisites

* Basic understanding of OCaml
* Installation of [opam](https://opam.ocaml.org/doc/Install.html)

To initialize opam:
```sh
opam init
```

To install dune (the build system):
```sh
opam install dune
```

### Installation

To create the switch with the proper compiler version:
```sh
opam switch create 4.08.1
opam switch 4.08.1
```

To install the database driver dependencies for MariaDB and PostgreSQL:
```sh
(Ubuntu)
sudo apt-get install -y libmariadbclient-dev libpq-dev

(Arch)
pacman -S mariadb-libs postgresql-libs
```

To install `inotifywait` to watch your build:
```sh
(Ubuntu)
sudo apt-get install -y inotify-tools

(Arch)
pacman -S inotify-tools
```

To install all dependencies and Sihl:
```sh
opam install .
opam install caqti-driver-mariadb caqti-driver-postgresql
opam install sihl
```

## Usage

Let's a simple Sihl app, that is a simple web app with a HTTP route.

We are using [https://github.com/ocaml/dune](dune) to build the project. Create a `dune` file that specifies an executable depending on Sihl.

dune:

```
(executable
  (name app)
  (libraries
   sihl
  )
)
```

```
.
├── service
│   ├── dune
│   ├── service.ml
├── app
│   ├── dune
│   ├── app.ml
├── components
│   ├── pizza-delivery
│   │   ├── model.ml
│   │   ├── service.ml
│   │   ├── repo.ml
│   ├── pizza-order-taking
│   │   ├── model.ml
│   │   ├── service.ml
│   │   ├── repo.ml
│   │   ├── cmd.ml
├── web
│   ├── routes.ml
│   ├── middlewares.ml
├── cli
│   ├── cmd.ml
```

There is a strong emphasis on the separation of business logic from everything else. In this example, the domain layer is split up into two parts `pizza-delivery` and `pizza-order-taking`. Note that all the business rules live in that layer.

A set of services, models and repos on its own is not that useful. In order to make it useful, we need to expose it to users. A typical web app does that through HTTP and a few CLI commands, which are primary used for development.

Everything regarding HTTP, routing, GraphQL, REST, JSON, middlewares lives in `web`. `web` is allowed to use any service.

The folder `app` contains `app.ml` which describes a Sihl app.

In the folder `service` contains the service configuration `service.ml`. This is the static setup of all services that are usable throughout the project.

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
