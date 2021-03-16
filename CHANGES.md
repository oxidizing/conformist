## 0.2.1 - 2021-03-16
### Changed
- Replace `ppx_deriving` with `sexplib`

## [0.2.0] - 2021-03-07
### Changed
- A field that is `optional` can be missing from the input data or the value can be `[]`. Decoding and validation will still work as expected. The decoded value is `None`.

## [0.1.0] - 2020-09-29
### Added
- `Conformist.Field.type_` can be used to retrieve a string representation of the type
- Support for custom `encoders` for custom types
- Support for encoding default values if present, this can be used to print schemas

### Fixed
- Move mandatory field name parameter to the end of create functions in order to get rid of the trailing () argument

## [0.0.2] - 2020-09-29
### Fixed
- Move `fold_left` to top level module and accept `Conformist.t` as input

## [0.0.1] - 2020-09-20
### Added
- Initial release supporting `int`, `float`, `string`, `bool`, `Ptime.date` and custom types
