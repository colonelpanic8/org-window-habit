{
  description = "org-window-habit - Time window based habits for org-mode";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        emacsWithPackages = pkgs.emacsPackages.emacsWithPackages (epkgs: [
          epkgs.dash
        ]);

        emacsBin = "${emacsWithPackages}/bin/emacs";

        srcDir = ./.;

      in {
        checks = {
          byte-compile = pkgs.runCommand "byte-compile" {} ''
            # Copy source to writable location
            cp ${srcDir}/org-window-habit.el .
            ${emacsBin} --batch \
              --eval "(require 'package)" \
              --eval "(package-initialize)" \
              --eval "(require 'org)" \
              --eval "(require 'org-habit)" \
              --eval "(require 'dash)" \
              --eval "(setq byte-compile-error-on-warn t)" \
              -f batch-byte-compile org-window-habit.el
            touch $out
          '';

          test = pkgs.runCommand "test" {} ''
            ${emacsBin} --batch \
              --eval "(require 'package)" \
              --eval "(package-initialize)" \
              --eval "(require 'org)" \
              --eval "(require 'org-habit)" \
              --eval "(require 'dash)" \
              --load ${srcDir}/org-window-habit.el \
              --load ${srcDir}/test/org-window-habit-test.el \
              -f ert-run-tests-batch-and-exit
            touch $out
          '';
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [ emacsWithPackages ];

          shellHook = ''
            echo "org-window-habit development environment"
            echo "Emacs version: $(emacs --version | head -1)"
            echo ""
            echo "Commands:"
            echo "  nix flake check    - Run all checks (byte-compile, tests)"
            echo "  emacs              - Start Emacs with dependencies"
          '';
        };
      });
}
