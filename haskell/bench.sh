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

while IFS= read -r file || [ -n "$file" ]; do
    timeout 120m stack run -- --filename="$file" &> results/$file.result;
done < "$fileList"
