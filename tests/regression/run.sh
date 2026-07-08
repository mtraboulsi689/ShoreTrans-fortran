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
  if [[ -f "$case_dir/expect_initial_equal" ]]; then
    python3 - "$case_dir" <<'PY'
import sys
from pathlib import Path

case = Path(sys.argv[1])
initial = [line.split() for line in (case / "outputs" / "initial_profile.out").read_text().splitlines()]
final = [line.split() for line in (case / "outputs" / "z_final.out").read_text().splitlines()]
if initial != final:
    raise SystemExit("final profile differs from initial profile")
PY
  fi
  if [[ -f "$case_dir/expect_profile_min" ]]; then
    python3 - "$case_dir" <<'PY'
import sys
from pathlib import Path

case = Path(sys.argv[1])
final = [line.split() for line in (case / "outputs" / "z_final.out").read_text().splitlines()]
for line in (case / "expect_profile_min").read_text().splitlines():
    if not line.strip():
        continue
    point, z_min = line.split()
    point = int(point)
    z_min = float(z_min)
    z_value = float(final[point - 1][1])
    if z_value < z_min:
        raise SystemExit(f"profile point {point} is {z_value}, below {z_min}")
PY
  fi
  if grep -q "ERROR:" "$case_dir/run.log"; then
    echo "case failed: $(basename "$case_dir")" >&2
    cat "$case_dir/run.log" >&2
    exit 1
  fi
done

echo "regression cases passed"
