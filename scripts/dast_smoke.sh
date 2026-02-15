#!/usr/bin/env bash
set -euo pipefail

BINARY="./target/release/tounges"

if [[ ! -x "$BINARY" ]]; then
  echo "Binary missing: $BINARY" >&2
  exit 1
fi

# 1) malformed JSON should fail gracefully and return an error payload.
set +e
MALFORMED_OUT=$(printf '{"program": "x = 1;"' | "$BINARY")
MALFORMED_RC=$?
set -e

if [[ $MALFORMED_RC -eq 0 ]]; then
  echo "Expected non-zero exit code for malformed JSON" >&2
  exit 1
fi

echo "$MALFORMED_OUT" | grep -q '"error"'

# 2) semantic runtime error should be handled safely (undefined variable).
set +e
UNDEFINED_OUT=$(printf '{"program":"output missing;","input":{}}' | "$BINARY")
UNDEFINED_RC=$?
set -e

if [[ $UNDEFINED_RC -eq 0 ]]; then
  echo "Expected non-zero exit code for undefined variable" >&2
  exit 1
fi

echo "$UNDEFINED_OUT" | grep -q '"error"'

# 3) valid input still succeeds.
VALID_OUT=$(printf '{"program":"n = start; sum = 0; while n > 0 { sum = sum + n; n = n - 1; } output sum;","input":{"start":4}}' | "$BINARY")

echo "$VALID_OUT" | grep -q '"outputs"'
echo "$VALID_OUT" | grep -q '"variables"'

echo "DAST runtime checks passed"
