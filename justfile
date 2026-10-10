# Run all checks (byte-compile, checkdoc, package-lint, tests)
check:
    nix flake check -L

# Run the ERT test suite
test:
    nix build .#checks.x86_64-linux.test -L

# Run the ERT test suite on Emacs 29, the oldest supported version
test-emacs29:
    nix build .#checks.x86_64-linux.test-emacs29 -L

# Byte-compile with warnings as errors
byte-compile:
    nix build .#checks.x86_64-linux.byte-compile -L

# Check documentation with checkdoc
checkdoc:
    nix build .#checks.x86_64-linux.checkdoc -L

# Run package-lint
package-lint:
    nix build .#checks.x86_64-linux.package-lint -L

# Enter development shell
dev:
    nix develop
