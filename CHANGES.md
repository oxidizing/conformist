## 0.7.0 - 2021-11-18
### Added
- `Make` functor to customize the error type and the error messages, which be default English `string`s

## 0.6.0 - 2021-05-30
### Added
- Add `Conformist.list` to support decoding list of `'a`

### Changed
- `Conformist.Field.optional` is now `Conformist.Field.is_optional` to avoid confusion with `Conformist.optional`
- The encoded value is assumed to be `string list` instead of `string`. This is preparation to support decoding of a list of strings.

## 0.5.0 - 2021-04-12
### Added
- Conformist type `datetime` that decodes to `Ptime.t`. This replaces `date` which has been deprecated.

## 0.4.0 - 2021-03-26
### Changed
- `decode`, `validate` and `decode_and_validate` all return the same `error` type

### Added
- Improve error reporting by printing provided input values (list of values)

## 0.3.0 - 2021-03-26
### Changed
- `decode` returns a triple containing `(field_name, input, error_msg)` instead of a concatenated string. This makes it easier to extract information.

### Added
- `decode_and_validate` combines `decode` and `validate` where the returned value is either the decoded value or a list of errors. When using `decode_and_validate`, one can forget about the difference between `decode` and `validate` and simply forward the list of errors. This covers a common use case.

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
