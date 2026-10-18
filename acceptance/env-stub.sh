#!/bin/sh

# env-stub.sh — AWS CLI Stub Reporting its Environment for OTT Acceptance Tests

# OTT (https://github.com/foretspaisibles/ott)
# This file is part of OTT.
#
# Copyright © 2026 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# The stub prints the variables OTT_TEST_PROFILE, OTT_TEST_HOME and
# OTT_TEST_EXTRA, as `profile=… home=… extra=…', and fails on the
# command `configure get region'.

if [ "$1 $2 $3" = 'configure get region' ]; then
    exit 255
fi
printf 'profile=%s home=%s extra=%s\n' \
       "${AWS_PROFILE}" "${OTT_TEST_HOME}" "${OTT_TEST_EXTRA}"

# End of file `env-stub.sh'
