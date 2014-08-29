#!/bin/bash

for filename in Examples/*.xml; do
    file=$(basename "$filename")
    f=${file%.*}
    make r=$filename 2> Results/$f.txt
done
