#!/bin/sh

# aws-stub.sh — AWS CLI Stub for OTT Acceptance Tests

# OTT (https://github.com/foretspaisibles/ott)
# This file is part of OTT.
#
# Copyright © 2026 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# The stub emulates the few AWS CLI commands used by the OTT setup.
# Its state lives in the directory `state' next to the script:
#
#   log               one line for each invocation
#   fail-sts          when present, `sts get-caller-identity' fails
#   account           the account reported by STS (default 123456789012)
#   bucket, role, oidc, policy-NAME
#                     present when the resource exists
#   RESOURCE.expiry   the ExpiryDate label of the resource
#   manifest.json     the last object stored in the state bucket

statedir="$(dirname "$0")/state"
mkdir -p "${statedir}"
printf '%s\n' "$*" >> "${statedir}/log"

stub_option()
{
    stub_option_name="$1"
    shift
    while [ $# -gt 0 ]; do
	if [ "$1" = "${stub_option_name}" ]; then
	    printf '%s' "$2"
	    return
	fi
	shift
    done
}

stub_expiry()
{
    printf '%s\n' "$*" | sed -n 's/.*Key=ExpiryDate,Value=\([^,} ]*\).*/\1/p'
}

stub_require()
{
    [ -f "${statedir}/$1" ] || exit 254
}

stub_main()
{
    command="$1 $2"
    shift 2
    case "${command}" in
	'sts get-caller-identity')
	    if [ -f "${statedir}/fail-sts" ]; then
		1>&2 printf 'Unable to locate credentials.\n'
		exit 255
	    fi
	    if [ -f "${statedir}/account" ]; then
		cat "${statedir}/account"
	    else
		printf '123456789012\n'
	    fi
	    ;;
	's3api head-bucket')
	    stub_require bucket
	    ;;
	's3api create-bucket')
	    touch "${statedir}/bucket"
	    ;;
	's3api put-bucket-tagging')
	    stub_expiry "$@" > "${statedir}/bucket.expiry"
	    ;;
	's3api get-bucket-tagging')
	    stub_require bucket
	    cat "${statedir}/bucket.expiry"
	    ;;
	's3api delete-bucket')
	    rm -f "${statedir}/bucket" "${statedir}/bucket.expiry"
	    ;;
	's3api put-object')
	    cp "$(stub_option --body "$@")" "${statedir}/manifest.json"
	    ;;
	's3api put-'*|'s3api list-object-versions'|'s3api delete-object')
	    ;;
	'iam get-open-id-connect-provider')
	    stub_require oidc
	    ;;
	'iam create-open-id-connect-provider')
	    touch "${statedir}/oidc"
	    ;;
	'iam get-role')
	    stub_require role
	    ;;
	'iam create-role')
	    touch "${statedir}/role"
	    stub_expiry "$@" > "${statedir}/role.expiry"
	    ;;
	'iam tag-role')
	    stub_require role
	    stub_expiry "$@" > "${statedir}/role.expiry"
	    ;;
	'iam list-role-tags')
	    stub_require role
	    cat "${statedir}/role.expiry"
	    ;;
	'iam delete-role')
	    rm -f "${statedir}/role" "${statedir}/role.expiry"
	    ;;
	'iam update-assume-role-policy'|'iam attach-role-policy'|'iam detach-role-policy')
	    ;;
	'iam get-policy')
	    stub_require "policy-$(stub_option --policy-arn "$@" | sed 's#.*/##')"
	    ;;
	'iam create-policy')
	    name="$(stub_option --policy-name "$@")"
	    touch "${statedir}/policy-${name}"
	    stub_expiry "$@" > "${statedir}/policy-${name}.expiry"
	    ;;
	'iam tag-policy')
	    name="$(stub_option --policy-arn "$@" | sed 's#.*/##')"
	    stub_require "policy-${name}"
	    stub_expiry "$@" > "${statedir}/policy-${name}.expiry"
	    ;;
	'iam list-policy-tags')
	    name="$(stub_option --policy-arn "$@" | sed 's#.*/##')"
	    stub_require "policy-${name}"
	    cat "${statedir}/policy-${name}.expiry"
	    ;;
	'iam delete-policy')
	    name="$(stub_option --policy-arn "$@" | sed 's#.*/##')"
	    rm -f "${statedir}/policy-${name}" "${statedir}/policy-${name}.expiry"
	    ;;
	'iam list-policy-versions'|'iam create-policy-version'|'iam delete-policy-version')
	    ;;
	*)
	    1>&2 printf 'aws-stub: unsupported command: %s\n' "${command}"
	    exit 2
	    ;;
    esac
}

stub_main "$@"

# End of file `aws-stub.sh'
