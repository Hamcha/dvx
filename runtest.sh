#!/bin/sh

if [ ! -f dist/build/dvx/dvx ]; then
	echo "dvx binary not found. Have you compiled it?"
	exit 1
fi

SUCC=0
ERR=0
for FILE in $(ls tests); do
	dist/build/dvx/dvx tests/$FILE
	if [[ $? = 0 ]]; then
		echo "- $FILE OK"
		SUCC=$((SUCC+1))
	else
		echo "- $FILE ERR"
		ERR=$((ERR+1))
	fi
done

echo
echo "Tests OK: $SUCC - ERR: $ERR"
echo "No."
