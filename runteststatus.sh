#!/bin/sh

if [ ! -f dist/build/dvx/dvx ]; then
    echo "dvx binary not found. Have you compiled it?"
    exit 1
fi

echo -e "| :ok: | Test file | Notes |"
echo -e "|:----:|-----------|-------|"

SUCC=0
ERR=0
for FILE in $(ls tests); do
    RESULT=$(dist/build/dvx/dvx tests/$FILE)
    if [[ $? = 0 ]]; then
        echo -e  "| :white_check_mark: | $FILE | - |"
        SUCC=$((SUCC+1))
    else
        echo -e  "| :heavy_exclamation_mark: | $FILE | $RESULT |"
        ERR=$((ERR+1))
    fi
done