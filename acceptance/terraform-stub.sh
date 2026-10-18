#!/bin/sh

# terraform-stub.sh — Terraform Stub for OTT Acceptance Tests

# OTT (https://github.com/foretspaisibles/ott)
# This file is part of OTT.
#
# Copyright © 2026 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# The stub logs its invocations in the file `state/log' next to the
# script, without the leading `-chdir' argument.  It fails for the
# subcommand SUBCOMMAND when the file `state/fail-SUBCOMMAND' exists.

statedir="$(dirname "$0")/state"
mkdir -p "${statedir}"
shift
printf '%s\n' "$*" >> "${statedir}/log"

if [ -f "${statedir}/fail-$1" ]; then
    1>&2 printf 'Error: terraform %s failed\n' "$1"
    exit 1
fi

if [ "$1" = 'output' ]; then
    printf 'https://sqs.stub/123456789012/ott-continuation'
fi

# End of file `terraform-stub.sh'
