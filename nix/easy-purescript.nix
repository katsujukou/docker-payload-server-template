{ pkgs ? import <nixpkgs> {} }:

import (pkgs.fetchFromGitHub {
  owner = "justinwoo";
  repo = "easy-purescript-nix";
  rev = "3630943b74f681289ed87a0ed6c3e502556ddebb";
  sha256 = "1i7zqda52npklj3d7pq80zw5rfjjzdqpl5bdrsp6vchg5frgj6ky";
}) {
  inherit pkgs;
}
