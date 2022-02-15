{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    frontend = ./frontend;
    Naproche-SAD = ./frontend/Naproche-SAD;
  };

  shells = {
    ghc = ["frontend"];
    ghcjs = ["frontend" "Naproche-SAD"];
  };
})
