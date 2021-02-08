#!/bin/bash


## Extracts worksheets from all xls or xlsx in current directory



shopt -s globstar  # enable recursive globbing


for file in **/*.xls{,x}  # find all .xls or .xlsx files
    do
        in2csv -n "$file" | # grab sheetnames
        xargs -I {} bash -c 'in2csv --sheet "$2" "$1" > "${1%.*}"-"$2".csv' _ "$file" {} # {} will be replaced with the sheetname
    done
