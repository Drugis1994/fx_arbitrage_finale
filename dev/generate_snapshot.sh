#!/usr/bin/env bash
ROOT="$(cd "$(dirname "$0")/.."; pwd)"
TS=$(date -u +"%Y%m%d_%H%M%SZ")
OUT="snapshot_${TS}"
mkdir -p "$OUT"

# 1) Lag filindeks
echo "path,provides,requires,owner" > "$OUT/FILE_INDEX.csv"
find . -type f \( -name '*.R' -o -name '*.cpp' -o -name '*.h' -o -name '*.md' \) \
  -not -path "./.git/*" -not -path "./${OUT}/*" | while read f; do
  p=$(grep -m1 -E 'Provides:' "$f" 2>/dev/null || echo "")
  r=$(grep -m1 -E 'Requires:' "$f" 2>/dev/null || echo "")
  o=$(grep -m1 -E 'Owner:' "$f" 2>/dev/null || echo "")
  echo "\"$f\",\"${p#*:}\",\"${r#*:}\",\"${o#*:}\"" >> "$OUT/FILE_INDEX.csv"
done

# 2) Lag avhengighetsgraf (.dot) og eventuelt PNG
echo "digraph G { rankdir=LR;" > "$OUT/deps.dot"
awk -F',' 'NR>1{gsub(/"/,"",$0); if($3!="") print "\"" $3 "\" -> \"" $1 "\";"}' "$OUT/FILE_INDEX.csv" >> "$OUT/deps.dot"
echo "}" >> "$OUT/deps.dot"
command -v dot >/dev/null && dot -Tpng "$OUT/deps.dot" -o "$OUT/graph.png" || true

# 3) Legg med litt git-metadata
git rev-parse --verify HEAD > "$OUT/git_head.txt" 2>/dev/null || true
git log -n 5 --oneline > "$OUT/git_last5.txt" 2>/dev/null || true

# 4) Pakk snapshotet
tar -czf "${OUT}.tar.gz" "$OUT"
echo "Snapshot created: ${OUT}.tar.gz"
