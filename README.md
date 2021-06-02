# cfgeq

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![nix build](https://github.com/chuahou/cfgeq/actions/workflows/nix-build.yml/badge.svg)](https://github.com/chuahou/cfgeq/actions/workflows/nix-build.yml)

A library for writing CFGs using a DSL that can be checked for equality on
strings generated up to a specified length. Inefficient and unsound as it cannot
differentiate CFGs that generate different strings of longer length.
