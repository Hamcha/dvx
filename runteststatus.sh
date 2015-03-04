#!/bin/bash
# because Bash is master race, you little b*tch

EXE=dist/build/dvx/dvx
if [ ! -f $EXE ]; then
    # fall back to manually compiled executable
    EXE=./dvx
    if [ ! -f $EXE ]; then
	    echo "$EXE binary not found. Have you compiled it?"
	    exit 1
    fi
fi

echo -e "| :ok: | Test file | Notes |"
echo -e "|:----:|-----------|-------|"

SUCC=0
ERR=0
for FILE in $(ls tests); do
    RESULT=$($EXE tests/$FILE 2>&1)
    if [ $? = 0 ]; then
        echo -e  "| :white_check_mark: | $FILE | - |"
        SUCC=$((SUCC+1))
    else
        echo -e  "| :heavy_exclamation_mark: | $FILE | $RESULT |"
        ERR=$((ERR+1))
    fi
done
