library(tidyverse)
library(readr)

## Create table joining city_name, school, state and ORI
batch_data <- read_csv("ori_batch_header.csv",
    col_names = c('ori','year','state','population','date_ori_was_added','date_ori_went_nibrs','city_name','state_abbreviation','population_group','country_division','country_region','agency_indicator','core_city'))
batch_data <- select(batch_data, ori, year, state, population, city_name, agency_indicator)

# Tibble with the 26 schools' information
school_info_tb <- tibble(
    ori = c("AR0720100", "AR0160100", "CO0210100", "ID0010100", "ID0290500", "IA0850100", "IA0520200", "KS0230100", "MI8121800", "MI3336400", "MI3949900", "MI3759900", "OH0770100", "OH0050100", "OHCOP0000", "SC0390200", "SC0400100", "TN0750100", "TX2270100", "TX0610200", "TX1520000", "UT0030100", "UT0250600", "VA0600100", "WV0060200", "WV0310100"),
    city_name = c("fayetteville","jonesboro","colorado springs","boise","moscow","ames","iowa city","lawrence","ann arbor","east lansing","kalamazoo","mount pleasant","akron","athens","columbus","clemson","columbia","murfreesboro","austin","denton","lubbock","logan","provo","blacksburg","huntington","morgantown"),
    school = c("University of Arkansas", "Arkansas State University", "United States Air Force Academy", "Boise State University", "The University of Idaho", "Iowa State University", "The University of Iowa", "The University of Kansas", "The University of Michigan", "Michigan State University", "Western Michigan University", "Eastern Michigan University", "The University of Akron", "Ohio University", "The Ohio State University", "Clemson University", "The University of South Carolina", "Middle Tennessee State University", "The University of Texas at Austin", "The University of North Texas", "Texas Tech University", "Utah State University", "Brigham Young University", "Virginia Polytechnic Institute and State University", "Marshall University", "West Virginia University"),
    state = c("arkansas", "arkansas", "colorado", "idaho", "idaho", "iowa", "iowa", "kansas", "michigan", "michigan", "michigan", "michigan", "ohio", "ohio", "ohio", "south carolina", "south carolina", "tennessee", "texas", "texas", "texas", "utah", "utah", "virginia", "west virginia", "west virginia")
)

# ori_per_school <- inner_join(school_info_tb, batch_data, by = c("city_name", "state")) %>% 
#     #filter(agency_indicator == "city") %>%
#     select(city_name, school, state, ori, home_team_full) 

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

a_offenses_per_ori <- inner_join(school_info_tb, grade_a_offenses, by = "ori") %>%
    mutate(offense_type = ifelse(ucr_offense_code == "destruction/damage/vandalism of property", "vandalism", "assault"), date = incident_date) %>%
    filter(year(incident_date) >= 2000 & year(incident_date) <= 2005) %>%
    select(ori, school, offense_type, city_name, state, date) 
