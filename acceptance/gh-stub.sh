#!/bin/sh

# gh-stub.sh — GitHub CLI Stub for OTT Acceptance Tests

# OTT (https://github.com/foretspaisibles/ott)
# This file is part of OTT.
#
# Copyright © 2026 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# The stub records each `gh secret set NAME' invocation in the file
# `state/gh-log' next to the script, as `ARGUMENTS=VALUE' where VALUE is the
# standard input.  When the file `state/fail-gh' exists the stub fails.

statedir="$(dirname "$0")/state"
mkdir -p "${statedir}"

if [ -e "${statedir}/fail-gh" ]; then
    echo "gh: HTTP 403" >&2
    exit 1
fi

printf '%s=%s\n' "$*" "$(cat)" >> "${statedir}/gh-log"

# End of file `gh-stub.sh'
