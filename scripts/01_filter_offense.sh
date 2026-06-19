#!/bin/bash

## Extract and process the data for offense segments for years
# 2000 to 2005 
# File format inside the offense_segment_csv_1991_2024.zip
# nibrs_offense_segment_<year>.csv
# Link where data was retrieved: https://www.openicpsr.org/openicpsr/project/118281/version/V11/view?path=/openicpsr/118281/fcr:versions/V11/offense_segment_csv_1991_2024.zip&type=file

# unzip offense_segment_csv_1991_2024.zip "nibrs_offense_segment_200[0-5].csv"
unzip offense_segment_csv_1991_2024.zip "nibrs_offense_segment_200[0-5].csv"
cat nibrs_offense_segment_200[0-5].csv > nibrs_offense_segment_2000_2005.csv
