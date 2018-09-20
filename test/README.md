These test cases are run from the parent directory with a command like:

  SWATJS=path/to/jsshell cargo test

where the JS shell must have support for all the features used by the test
cases, currently anyref and some ref.whatever operations, eventually more.
