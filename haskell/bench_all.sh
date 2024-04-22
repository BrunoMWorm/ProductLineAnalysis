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

rm -f bdds/*
rm -f values/*

for analysis in RETURN_AVG CALL_DENSITY DANGLING_SWITCH CASE_TERMINATION RETURN; do
    stack clean
    stack build --ghc-options -D$analysis
    mkdir -p results/$analysis
    for version in 1_18_5 1_19_0; do 
        mkdir -p results/$analysis/$version
        while IFS= read -r file || [ -n "$file" ]; do
            timeout 15m stack run -- --filename="$file" --fileversion="$version" --csv results/$analysis/stats.csv &> results/$analysis/$version/$file.result ;
        done < "$fileList"
    done
    rm -f bdds/*
    rm -f values/*
done

