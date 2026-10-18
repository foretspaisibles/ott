#!/bin/sh

# sqs-stub.sh — AWS CLI Stub for the OTT Queue Acceptance Tests

# OTT (https://github.com/foretspaisibles/ott)
# This file is part of OTT.
#
# Copyright © 2026 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# The stub emulates the AWS CLI commands used to send and receive
# messages.  Its state lives in the directory `state' next to the script:
#
#   log        one line for each invocation, `[role]' marks the calls
#              made with the credentials of the assumed role
#   queue/     one file for each message of the queue, with the markers
#              NAME.hidden (in flight) and NAME.count (receive count)
#   dlq/       the messages moved to the dead letter queue
#
# A message is moved to the dead letter queue by the receive that
# follows its fifth receive.

statedir="$(dirname "$0")/state"
queue="${statedir}/queue"
dlq="${statedir}/dlq"
mkdir -p "${queue}" "${dlq}"

stub_role='[role]'
[ "${AWS_SESSION_TOKEN}" = 'stub-token' ] || stub_role=''
printf '%s %s\n' "$*" "${stub_role}" >> "${statedir}/log"

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

stub_require_role()
{
    [ -n "${stub_role}" ] && return
    1>&2 printf 'An error occurred (AccessDenied): not authorized to perform sqs\n'
    exit 255
}

stub_json_escape()
{
    sed -e 's/\\/\\\\/g' -e 's/"/\\"/g'
}

stub_receive()
{
    for file in "${queue}"/m.*; do
	[ -f "${file}" ] || continue
	case "${file}" in *.count) continue ;; esac
	mkdir "${file}.hidden" 2>/dev/null || continue
	name="$(basename "${file}")"
	count=0
	[ -f "${file}.count" ] && count="$(cat "${file}.count")"
	if [ "${count}" -ge 5 ]; then
	    mv "${file}" "${dlq}/${name}"
	    rm -rf "${file}.hidden" "${file}.count"
	    continue
	fi
	count=$((count + 1))
	printf '%s' "${count}" > "${file}.count"
	# Give the other listeners a chance to claim the next message.
	sleep 0.1
	printf '{"Messages":[{"MessageId":"%s","ReceiptHandle":"%s","Body":"%s",' \
	       "${name}" "${name}" "$(stub_json_escape < "${file}")"
	printf '"Attributes":{"ApproximateReceiveCount":"%s"}}]}\n' "${count}"
	return
    done
}

stub_main()
{
    command="$1 $2"
    shift 2
    case "${command}" in
	'sts get-caller-identity')
	    printf '123456789012\n'
	    ;;
	'sts assume-role')
	    stub_arn='arn:aws:iam::123456789012:role/OTTContinuedFunction'
	    [ "$(stub_option --role-arn "$@")" = "${stub_arn}" ] || exit 254
	    printf '{"AccessKeyId":"stub-key","SecretAccessKey":"stub-secret",'
	    printf '"SessionToken":"stub-token"}\n'
	    ;;
	'sqs get-queue-url')
	    stub_require_role
	    printf 'https://sqs.stub/123456789012/%s\n' "$(stub_option --queue-name "$@")"
	    ;;
	'sqs send-message')
	    stub_require_role
	    file="$(mktemp "${queue}/m.XXXXXX")"
	    printf '%s' "$(stub_option --message-body "$@")" > "${file}"
	    basename "${file}"
	    ;;
	'sqs receive-message')
	    stub_require_role
	    stub_receive
	    ;;
	'sqs delete-message')
	    stub_require_role
	    name="$(stub_option --receipt-handle "$@")"
	    rm -rf "${queue}/${name}" "${queue}/${name}.hidden" "${queue}/${name}.count"
	    ;;
	*)
	    1>&2 printf 'sqs-stub: unsupported command: %s\n' "${command}"
	    exit 2
	    ;;
    esac
}

stub_main "$@"

# End of file `sqs-stub.sh'
