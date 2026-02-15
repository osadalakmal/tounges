# tounges
Experimental Language for LLM Ease of Use.

## Toy embedded DSL compiler/interpreter (Rust)

This repo now includes a toy DSL implementation in Rust.

### Features

- Arithmetic operators: `+`, `-`, `*`, `/`
- Boolean operators: `not`, `and`, `or`
- Control structures:
  - `if { ... } else { ... }`
  - `while { ... }`
- Variables and assignment (`name = expression`)
- Output statements (`output expression`)

### JSON I/O contract

The CLI reads JSON from `stdin`:

```json
{
  "program": "x = 1 + 2; output x;",
  "input": {
    "seed": 10
  }
}
```

- `program` (string): DSL source code
- `input` (object): initial variable bindings injected into the program

It writes JSON to `stdout`:

```json
{
  "variables": {
    "x": 3,
    "seed": 10
  },
  "outputs": [3]
}
```

On failure, it writes:

```json
{
  "error": "...message..."
}
```

### Example

```bash
echo '{"program":"n = start; sum = 0; while n > 0 { sum = sum + n; n = n - 1; } output sum;","input":{"start":4}}' \
  | cargo run --quiet
```

### Run tests

```bash
cargo test
```

## CI/CD and Security

GitHub Actions workflow is defined at `.github/workflows/ci-cd.yml` and includes:

- **Build stage**: format check, compile, tests, and release artifact upload.
- **SAST stage**: CodeQL, `clippy -D warnings`, and RustSec advisory scan.
- **DAST stage**: runtime abuse tests (`scripts/dast_smoke.sh`) that validate safe failure behavior for malformed/hostile inputs and expected behavior for valid inputs.

