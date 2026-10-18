#!/bin/sh

# pre-commit.sh — Git pre-commit hook for OTT

# OTT (https://github.com/foretspaisibles/ott)
# This file is part of OTT.
#
# Copyright © 2026 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

: ${TOPLEVELDIR:=$(git rev-parse --show-toplevel)}

cd "${TOPLEVELDIR}" && sbcl --noinform --disable-debugger\
    --eval "(pushnew #p\"${TOPLEVELDIR}/\" asdf:*central-registry* :test #'equal)"\
    --eval '(ql:quickload "ott/acceptance" :silent t)'\
    --eval '(ott/acceptance:run-all-tests)'\
    --eval '(org.melusina.confidence:quit)'

# End of file `pre-commit.sh'
