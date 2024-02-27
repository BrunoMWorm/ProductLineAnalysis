#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 filelist"
    exit 1
fi

fileList="$1"

if [ ! -f "$fileList" ]; then
    echo "File list $fileList does not exist."
    exit 1
fi

mkdir -p results

for analysis in RETURN CASE_TERMINATION RETURN_AVG GOTOS DANGLING_SWITCH CALL_DENSITY; do
    mkdir -p results/$analysis
    stack clean
    stack build --ghc-options -D$analysis
    while IFS= read -r file || [ -n "$file" ]; do
        timeout 15m stack run -- --filename="$file" --csv results/$analysis/stats.csv &> results/$analysis/$file.result ;
    done < "$fileList"
done 

