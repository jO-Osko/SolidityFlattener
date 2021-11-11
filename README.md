# jO-Osko's flattener

Flattens your solidity files into a single file needed for verification on blocksout or testing on remix.

## Features

- Supports flattening of files with cyclical dependencies.
- In a cyclical dependency situation, contents are inlinded in the order of inheritance so that compiler can resolve the dependencies.
- Import from remote providers is supported (currently only github and gitlab).
- Supports imports from `node_modules` folder (`import @openzeppelin` etc.).
- Fast and efficient inlining (sub second invocation).

## Usage

Once you install the package invoke it with `yarn jooskos_flattener <path/to/input_file.sol>`.
The flattened output will be written to stdout. Flattener automatically looks for `node_modules` in the current and parent directory to be used for `@` imports.
Available options:

- `-o <path/to/output_file.sol>`: write the flattened output to the specified file (default: stdout)
- `--node-modules <path/to/node_modules>`: specify the node_modules directory to be used for `@` imports (default: current directory or parent directory if they contain `node_modules`) folder
- `--contract-libs <lib1>,<lib2>,<lib3>`: Comma seperated list of `@` imports to be searched in `node_modules` (default: `@openzeppelin,@gnosis.pm`)

## Roadmap

- More support for inline assembly
- Support for `unchecked` in solidity `0.8.+`
- Better documentation and error reporting.
- More autmatic configurations.

## Contributing

Any contributions are welcome (PR, Issues, documentation, comments).

## Acknowledgements

This work builds greatly on [ocaml solidity](https://github.com/OCamlPro/ocaml-solidity) developed by ocamlpro and modified [a bit](https://github.com/jO-Osko/ocaml-solidity/tree/parser-upgrade).

AFLabs and Flare foundation for testing and bug reporting.

----------

jO-Osko
