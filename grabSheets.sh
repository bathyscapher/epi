#!/bin/bash

## Download EPI 2020 xlxs (note: requires login into EOSDIS Earthdata)
wget https://sedac.ciesin.columbia.edu/downloads/data/epi/epi-environmental-performance-index-2018/2018-epi-xlsx.zip


## Move nzip
unzip ... -d data


## Extracts worksheets from all xls or xlsx in current directory
shopt -s globstar  # enable recursive globbing


for file in **/*.xls{,x}  # find all .xls or .xlsx files
    do
        in2csv -n "$file" | # grab sheetnames
        xargs -I {} bash -c 'in2csv --sheet "$2" "$1" > "${1%.*}"-"$2".csv' _ "$file" {} # {} will be replaced with the sheetname
    done


## Simpler, retrieve the csv files directly
wget -P 2020 https://epi.yale.edu/downloads/epi2020results20200604.csv https://epi.yale.edu/downloads/epi2020indicatortla20200604.csv https://epi.yale.edu/downloads/epi2020countryattributes20200604.csv https://epi.yale.edu/downloads/epi2020variableattributes20200604.csv


