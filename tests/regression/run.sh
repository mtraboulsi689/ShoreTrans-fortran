#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
exe="${1:-$root/bin/shoretrans}"

if [[ ! -x "$exe" ]]; then
  echo "missing executable: $exe" >&2
  exit 1
fi

for case_dir in "$root"/tests/regression/cases/*; do
  [[ -d "$case_dir" ]] || continue
  rm -rf "$case_dir/outputs"
  "$exe" "$case_dir" > "$case_dir/run.log"
  test -s "$case_dir/outputs/z_final.out"
  test -s "$case_dir/outputs/initial_profile.out"
  if grep -q "ERROR:" "$case_dir/run.log"; then
    echo "case failed: $(basename "$case_dir")" >&2
    cat "$case_dir/run.log" >&2
    exit 1
  fi
done

echo "regression cases passed"
