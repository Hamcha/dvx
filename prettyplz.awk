#!/usr/bin/awk -f
# Prettifies a Haskell list

function print_indent(ind,    i) {
	for (i = 0; i < ind; ++i)
		printf "   "
}

{
	for (c = 1; c <= length($0); ++c) {
		ch = substr($0, c, 1)
		if (ch == "[") {
			if (pythonic)
				printf "\n"
			else
				printf "[\n"
			print_indent(++indent)
		} else if (ch == "(") {
			in_args = 1
			printf "("
		} else if (ch == ")") {
			in_args = 0
			printf ")"
		} else if (ch == ",") {
			if (in_args)
				printf ", "
			else {
				if (!pythonic) printf ","
				print ""
				print_indent(indent)
			}
		} else if (ch == "]") {
			if (pythonic) {
				--indent
			} else {
				print ""
				print_indent(--indent)
				printf "]"
			}
		} else {
			printf ch
		}
	}
	print ""
}
