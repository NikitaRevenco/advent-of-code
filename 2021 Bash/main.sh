function script_dir {
	dirname "$0"
}

function read_input {
	local day="$1"
	cat "$(script_dir)/inputs/day_${day}.txt"
}

function assert_eq {
	local expected="$1"
	local actual="$2"

	if [ "$expected" == "$actual" ]; then
		return 0
	else
		printf '%s\n' "Incorrect result for assertion: ${expected} = ${actual}" >&2
		exit 1
	fi
}

read_input 1
