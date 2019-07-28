#!/bin/bash
for filename in tests/*.c; do
	echo "File: $filename"
	./main.native -withoutWidening2 "$filename" | awk '/Return code:/{getline; print}'
	echo
done
