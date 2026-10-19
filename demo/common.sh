# common.sh — Shared by the demo scripts, to be sourced.
# Usage: [LISP_FILE=FILE] run_lisp EXPRESSION SYSTEM...
# Load SYSTEMs and LISP_FILE, evaluate EXPRESSION, exit with its value, 0 when NIL.

demo_directory=$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)
project_directory=$(dirname "${demo_directory}")

find_quicklisp() {
    local candidate
    for candidate in "${QUICKLISP_SETUP:-}" "${HOME}/quicklisp/setup.lisp" "${HOME}/Library/QuickLisp/setup.lisp"; do
	if [ -n "${candidate}" ] && [ -r "${candidate}" ]; then
	    printf '%s\n' "${candidate}"
	    return 0
	fi
    done
    echo "Cannot find Quicklisp, set QUICKLISP_SETUP to its setup.lisp." >&2
    return 1
}

run_lisp() {
    local expression=$1; shift
    local setup system
    setup=$(find_quicklisp) || exit 1
    local arguments=(--noinform --non-interactive --load "${setup}"
		     --eval "(pushnew (truename \"${project_directory}/\") asdf:*central-registry* :test #'equal)")
    for system in "$@"; do
	arguments+=(--eval "(ql:quickload \"${system}\" :silent t)")
    done
    if [ -n "${LISP_FILE:-}" ]; then
	arguments+=(--load "${LISP_FILE}")
    fi
    # exec, so that the signals sent to the script reach SBCL.
    exec sbcl "${arguments[@]}" --eval "(sb-ext:exit :code (or ${expression} 0))"
}
