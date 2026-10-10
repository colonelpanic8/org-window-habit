{
  description = "org-window-habit - Time window based habits for org-mode";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # Oldest supported Emacs (Package-Requires: emacs 29.1)
    nixpkgs-emacs29.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-emacs29, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        emacsWithPackages = pkgs.emacsPackages.emacsWithPackages (epkgs: [
          epkgs.package-lint
        ]);

        emacsBin = "${emacsWithPackages}/bin/emacs";

        # Only used to run the checks, so its known vulnerabilities don't matter.
        emacs29 = (import nixpkgs-emacs29 {
          inherit system;
          config.permittedInsecurePackages = [ "emacs-29.4" ];
        }).emacs29;

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

        mkByteCompile = name: emacs: pkgs.runCommand name {} ''
          # Copy all source files to writable location
          ${builtins.concatStringsSep "\n" (map (f: "cp ${srcDir}/${f} .") elispFiles)}
          ${emacs}/bin/emacs --batch \
            --eval "(require 'package)" \
            --eval "(package-initialize)" \
            --eval "(require 'org)" \
            --eval "(require 'org-habit)" \
            --eval "(add-to-list 'load-path \".\")" \
            --eval "(setq byte-compile-error-on-warn t)" \
            -f batch-byte-compile ${builtins.concatStringsSep " " elispFiles}
          touch $out
        '';

        mkTest = name: emacs: pkgs.runCommand name {
          # Include tzdata so DST-related tests can use set-time-zone-rule
          TZDIR = "${pkgs.tzdata}/share/zoneinfo";
        } ''
          ${emacs}/bin/emacs --batch \
            --eval "(require 'package)" \
            --eval "(package-initialize)" \
            --eval "(require 'org)" \
            --eval "(require 'org-habit)" \
            --eval "(add-to-list 'load-path \"${srcDir}\")" \
            --eval "(add-to-list 'load-path \"${srcDir}/test\")" \
            --load ${srcDir}/org-window-habit.el \
            --eval "(mapc #'load (directory-files \"${srcDir}/test\" t \"-test\\\\.el\\\\'\"))" \
            -f ert-run-tests-batch-and-exit
          touch $out
        '';

      in {
        checks = {
          byte-compile = mkByteCompile "byte-compile" emacsWithPackages;
          byte-compile-emacs29 = mkByteCompile "byte-compile-emacs29" emacs29;
          test = mkTest "test" emacsWithPackages;
          test-emacs29 = mkTest "test-emacs29" emacs29;

          checkdoc = pkgs.runCommand "checkdoc" {} ''
            ${emacsBin} --batch \
              --eval "(require 'checkdoc)" \
              --eval "(setq sentence-end-double-space nil)" \
              --eval "(setq checkdoc-verb-check-experimental-flag nil)" \
              --eval "(setq checkdoc-spellcheck-documentation-flag nil)" \
              --eval '(defvar checkdoc-files (list ${builtins.concatStringsSep " " (map (f: ''"${srcDir}/${f}"'') elispFiles)}))' \
              --eval "(let ((all-warnings nil))
                        (dolist (file checkdoc-files)
                          (with-current-buffer (find-file-noselect file)
                            (checkdoc-current-buffer t)))
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
