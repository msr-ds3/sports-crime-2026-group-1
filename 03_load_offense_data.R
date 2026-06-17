library(tidyverse)
library(readr)

## Create table joining city_name, school, state and OIR
batch_data <- read_csv("ori_batch_header.csv",
    col_names = c('ori','year','state','population','date_ori_was_added','date_ori_went_nibrs','city_name','state_abbreviation','population_group','country_division','country_region','agency_indicator','core_city'))
batch_data <- select(batch_data, ori, year, state, population, city_name, agency_indicator)

school_info_tb <- tibble(
    city_name = c("fayetteville","jonesboro","colorado springs","boise","moscow","ames","iowa city","lawrence","ann arbor","east lansing","kalamazoo","mount pleasant","akron","athens","columbus","clemson","columbia","murfreesboro","austin","denton","lubbock","logan","provo","blacksburg","huntington","morgantown"),
    home_team_full = c("Arkansas Razorbacks", "Arkansas State Red Wolves", "Air Force Falcons", "Boise State Broncos", "Idaho Vandals", "Iowa State Cyclones", "Iowa Hawkeyes", "Kansas Jayhawks", 
    "Michigan Wolverines", "Michigan State Spartans", "Western Michigan Broncos", "Eastern Michigan Eagles", "Akron Zips", "Ohio Bobcats", "Ohio State Buckeyes", "Clemson Tigers", "South Carolina Gamecocks",
     "Middle Tennessee Blue Raiders", "Texas Longhorns", "North Texas Mean Green", "Texas Tech Red Raiders", "Utah State Aggies", "BYU Cougars", "Virginia Tech Hokies", "Marshall Thundering Herd", 
     "West Virginia Mountaineers"), 
    school = c("University of Arkansas", "Arkansas State University", "United States Air Force Academy", "Boise State University", "The University of Idaho", "Iowa State University", "The University of Iowa", "The University of Kansas", "The University of Michigan", "Michigan State University", "Western Michigan University", "Eastern Michigan University", "The University of Akron", "Ohio University", "The Ohio State University", "Clemson University", "The University of South Carolina", "Middle Tennessee State University", "The University of Texas at Austin", "The University of North Texas", "Texas Tech University", "Utah State University", "Brigham Young University", "Virginia Polytechnic Institute and State University", "Marshall University", "West Virginia University"),
    state = c("arkansas", "arkansas", "colorado", "idaho", "idaho", "iowa", "iowa", "kansas", "michigan", "michigan", "michigan", "michigan", "ohio", "ohio", "ohio", "south carolina", "south carolina", "tennessee", "texas", "texas", "texas", "utah", "utah", "virginia", "west virginia", "west virginia")
)

ori_per_school <- inner_join(school_info_tb, batch_data, by = c("city_name", "state")) %>% 
    # filter(agency_indicator == "city") %>%
    select(city_name, school, state, ori, home_team_full) 

ori_schools <- tibble(ori_per_school$ori)

## Load Offense Data
test_crime_data <- read_csv("nibrs_offense_segment_2000_2005.csv")
# names(test_crime_data)
grade_a_offenses <-filter(test_crime_data, 
    ucr_offense_code == "destruction/damage/vandalism of property" |
    ucr_offense_code == "assault offenses - aggravated assault" |
    ucr_offense_code == "assault offenses - intimidation" | 
    ucr_offense_code == "assault offenses - simple assault"
) %>% 
select(ori, ucr_offense_code, incident_date)

a_offenses_per_ori <- left_join(ori_per_school, grade_a_offenses, by = "ori") %>%
    mutate(offense_type = ifelse(ucr_offense_code == "destruction/damage/vandalism of property", "vandalism", "assault")) %>%
    filter(year(incident_date) >= 2000 & year(incident_date) <= 2005) # %>%
    select(ori, school, home_team_full, offense_type, city_name, state, incident_date) %>%
    group_by(ori) %>% summarize()
