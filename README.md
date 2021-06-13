# cfgeq

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![nix build](https://github.com/chuahou/cfgeq/actions/workflows/nix-build.yml/badge.svg)](https://github.com/chuahou/cfgeq/actions/workflows/nix-build.yml)

Unsound checker for checking CFG equality on strings generated up to a specified
length. Inefficient and unsound as it can only find counterexamples and not
prove the lack of a longer counterexample.

It works by compiling the given CFGs into Chomsky normal form, and enumerating
all strings each CFG generates up to the given length, then checking if the
generated sets are equal or not. If not, a counterexample is given. If they are,
we can only conclude there are no counterexamples *yet*, but cannot be confident
the languages are equal.

## Usage

Once built (see section below), run the executable giving it a file to check and
the length to enumerate up to. If your CFGs are in `cfgs.txt` and you would like
to find if there are any counterexamples up to length 10, run

```bash
cfgeq cfgs.txt 10
```

The file format consists of a prelude and 2 CFGs, each separated by a `%%`.
Identifiers are made of 1 or more alphanumeric characters or `_`. Strings must
have letters (terminals/variables) separated by whitespace.

The start token is given by the `%start` directive, and the list of terminals
must be specified using `%token`. All identifiers that are not specified to be
terminals are assumed to be variables.

Epsilon is written `epsilon`.

Each rule in each CFG must be terminated by a semicolon.

An example file would be

```
%start S
%token a

%%

S -> A A ;
A -> a | S | epsilon;

%%

S -> a S | epsilon;
```

## Building

Simply run one of the following depending on your system:

```bash
nix build
cabal build
```

Alternatively, you can run directly by issuing

```bash
nix run .# -- [args]
cabal run exe:cfgeq -- [args]
```
