#!/bin/bash

## Extract and process the data for offense segments for years
# 2000 to 2005 
# File format inside the offense_segment_csv_1991_2024.zip
# nibrs_offense_segment_<year>.csv
# Link where data was retrieved: https://www.openicpsr.org/openicpsr/project/118281/version/V11/view?path=/openicpsr/118281/fcr:versions/V11/batch_header_csv_1991_2024.zip&type=file 

unzip batch_header_csv_1991_2024.zip
awk -F ','  'NR==1 || $2 ==  2000' nibrs_batch_header_1991_2024.csv > ori_batch_header.csv.csv