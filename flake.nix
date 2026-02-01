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
          epkgs.package-lint
        ]);

        emacsBin = "${emacsWithPackages}/bin/emacs";

        srcDir = ./.;

        # All elisp source files (order matters for byte-compilation)
        elispFiles = [
          "org-window-habit-time.el"
          "org-window-habit-config.el"
          "org-window-habit-logbook.el"
          "org-window-habit-core.el"
          "org-window-habit-computation.el"
          "org-window-habit-instance.el"
          "org-window-habit-graph.el"
          "org-window-habit-advice.el"
          "org-window-habit-meta.el"
          "org-window-habit.el"
        ];

      in {
        checks = {
          byte-compile = pkgs.runCommand "byte-compile" {} ''
            # Copy all source files to writable location
            ${builtins.concatStringsSep "\n" (map (f: "cp ${srcDir}/${f} .") elispFiles)}
            ${emacsBin} --batch \
              --eval "(require 'package)" \
              --eval "(package-initialize)" \
              --eval "(require 'org)" \
              --eval "(require 'org-habit)" \
              --eval "(require 'dash)" \
              --eval "(add-to-list 'load-path \".\")" \
              --eval "(setq byte-compile-error-on-warn t)" \
              -f batch-byte-compile ${builtins.concatStringsSep " " elispFiles}
            touch $out
          '';

          checkdoc = pkgs.runCommand "checkdoc" {} ''
            ${emacsBin} --batch \
              --eval "(require 'checkdoc)" \
              --eval "(setq sentence-end-double-space nil)" \
              --eval "(setq checkdoc-verb-check-experimental-flag nil)" \
              --eval "(setq checkdoc-spellcheck-documentation-flag nil)" \
              --eval "(with-current-buffer (find-file-noselect \"${srcDir}/org-window-habit.el\")
                        (checkdoc-current-buffer t)
                        (let ((warnings (get-buffer \"*Warnings*\")))
                          (when (and warnings (> (buffer-size warnings) 0))
                            (with-current-buffer warnings
                              (princ (buffer-string)))
                            (kill-emacs 1))))"
            touch $out
          '';

          package-lint = pkgs.runCommand "package-lint" {} ''
            ${emacsBin} --batch \
              --eval "(require 'package)" \
              --eval "(package-initialize)" \
              --eval "(require 'package-lint)" \
              --eval "(setq package-lint-main-file \"${srcDir}/org-window-habit.el\")" \
              --eval "(let ((errors (package-lint-buffer
                                      (find-file-noselect \"${srcDir}/org-window-habit.el\"))))
                        (when errors
                          (dolist (err errors)
                            (message \"%s:%s: %s: %s\"
                                     (nth 0 err) (nth 1 err)
                                     (pcase (nth 2 err)
                                       ('error \"error\")
                                       ('warning \"warning\")
                                       (_ \"info\"))
                                     (nth 3 err)))
                          (when (cl-some (lambda (e) (eq (nth 2 e) 'error)) errors)
                            (kill-emacs 1))))"
            touch $out
          '';

          test = pkgs.runCommand "test" {
            # Include tzdata so DST-related tests can use set-time-zone-rule
            TZDIR = "${pkgs.tzdata}/share/zoneinfo";
          } ''
            ${emacsBin} --batch \
              --eval "(require 'package)" \
              --eval "(package-initialize)" \
              --eval "(require 'org)" \
              --eval "(require 'org-habit)" \
              --eval "(require 'dash)" \
              --eval "(add-to-list 'load-path \"${srcDir}\")" \
              --load ${srcDir}/org-window-habit.el \
              --load ${srcDir}/test/org-window-habit-test.el \
              -f ert-run-tests-batch-and-exit
            touch $out
          '';
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [ emacsWithPackages pkgs.just ];

          shellHook = ''
            echo "org-window-habit development environment"
            echo "Emacs version: $(emacs --version | head -1)"
            echo ""
            echo "Commands:"
            echo "  nix flake check    - Run all checks (byte-compile, checkdoc, package-lint, tests)"
            echo "  emacs              - Start Emacs with dependencies"
          '';
        };
      });
}
