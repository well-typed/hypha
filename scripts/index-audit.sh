#!/usr/bin/env bash
# Audit a built hypha index for the row shapes this branch set out to kill.
# Every counter must be 0.
set -euo pipefail
DB="${1:-$HOME/.cache/hypha/hypha.db}"

fail=0
check() {
  local label=$1 sql=$2 n
  n=$(sqlite3 "$DB" "$sql")
  printf '%-46s %s\n' "$label" "$n"
  [ "$n" = "0" ] || fail=1
}

echo "auditing $DB"
check "module names that are entirely lowercase" \
  "SELECT count(*) FROM pkg_index WHERE mod = lower(mod);"
# A Haskell module name has an upper-case initial letter in every segment,
# so a lower-case one means a directory leaked into the name -- which is
# exactly what path-derived naming produced (src-bench.bench-sha256,
# compiler.GHC.Data.Word64Map.Internal).  GLOB, not LIKE: LIKE is
# case-insensitive here and would flag Test.HUnit.
check "module names with a lower-case segment" \
  "SELECT count(*) FROM pkg_index WHERE mod GLOB '[a-z]*' OR mod GLOB '*.[a-z]*';"
check "rows with an empty definition module" \
  "SELECT count(*) FROM pkg_index WHERE def_mod = '';"
check "rows with an unrecognised visibility" \
  "SELECT count(*) FROM pkg_index WHERE visibility NOT IN ('exposed','internal');"
check "IntMap symbols carrying a Map signature" \
  "SELECT count(*) FROM pkg_index WHERE mod LIKE 'Data.IntMap%' \
   AND sig LIKE '%Map k a%' AND sig NOT LIKE '%IntMap%';"

echo
echo "index format: $(sqlite3 "$DB" "SELECT v FROM kv WHERE k = 'index_format';")"
echo "rows:         $(sqlite3 "$DB" "SELECT count(*) FROM pkg_index;")"
exit $fail
