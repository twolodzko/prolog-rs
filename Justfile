# run linter and tests
test: fmt
    cargo clippy
    cargo test -q

# autoformat the code
fmt:
    cargo fmt

# evaluate the FILE and launch REPL
run $FILE:
    cargo run -- $FILE

# build the standalone binary
build:
    cargo build --profile optimized
    rm -rf ./prolog
    mv target/optimized/prologrs ./prolog

# launch REPL
repl:
    cargo run

# cleanup the build files
clean:
    rm -rf ./target/ ./prolog
