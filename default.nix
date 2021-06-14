# SPDX-License-Identifier: MIT
# Copyright (c) 2021 Chua Hou

let
  inherit ((builtins.fromJSON
    (builtins.readFile ./flake.lock)).nodes.flake-compat) locked;
  tarball = builtins.fetchTarball {
    url =
      "https://github.com/edolstra/flake-compat/archive/${locked.rev}.tar.gz";
    sha256 = locked.narHash;
  };
  flake-compat = import tarball { src = ./.; };

in
  flake-compat.defaultNix // { inherit (flake-compat) shellNix; }
