#!/usr/bin/env bash

set -eux -o pipefail

here="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

src="${here}/../README.md"
output=${here}/readme-snippet.md

input="$(mktemp)"
cat "${src}" \
  | sed -n '/^## Example$/,$ p' \
  | awk '/```/ { if (!doPrint) { doPrint = 1; next; } else { doPrint = 0 } } doPrint' \
  > "${input}"

templette-markdown --render "${input}" > "${output}"
