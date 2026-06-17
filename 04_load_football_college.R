install.packages("cfbfastR")
library(tidyverse)
library(cfbfastR)
library(purrr)
library(dplyr) 

years <- 2000:2005
schedules <- map_dfr(years, function(yr){
    df <- espn_cfb_schedule(year = yr )
    df$year = yr
    df
})

school_schedule <- tibble(
    home_team_location = c("Arkansas", "Arkansas State" ,"Air Force", "Boise State", "Idaho","Iowa State", "Iowa", "Kansas","Michigan", 
    "Michigan State","Western Michigan","Eastern Michigan","Akron", "Ohio","Ohio State","Clemson", "USC", "Middle Tennessee","Texas","North Texas","Texas Tech","Utah State","BYU","Virginia Tech","Marshall","West Virginia"),
    team_full_name = c("Akron Zips", "Iowa State Cyclones", "Michigan Wolverines", "Ohio Bobcats", "Texas Longhorns", "Virginia Tech Hokies", "Boise State Broncos", "Clemson Tigers", 
    "Air Force Falcons", "South Carolina Gamecocks", "Ohio State Buckeyes", "North Texas Mean Green", "Michigan State Spartans", "Arkansas Razorbacks", "Marshall Thundering Herd", 
    "Iowa Hawkeyes", "Arkansas State Red Wolves", "Western Michigan Broncos", "Kansas Jayhawks", "Utah State Aggies", "Texas Tech Red Raiders", "West Virginia Mountaineers", 
    "Idaho Vandals", "Eastern Michigan Eagles", "Middle Tennessee Blue Raiders", "BYU Cougars"), # not aligned with
    city_name = c("fayetteville","jonesboro","colorado springs","boise","moscow","ames","iowa city","lawrence","ann arbor","east lansing","kalamazoo","mount pleasant","akron","athens","columbus","clemson","columbia","murfreesboro","austin","denton","lubbock","logan","provo","blacksburg","huntington","morgantown"),
    school_name = c("University of Arkansas", "Arkansas State University", "United States Air Force Academy", "
    Boise State University", "The University of Idaho", "Iowa State University", "The University of Iowa", "The University of Kansas", "The University of Michigan", "Michigan State University", "Western Michigan University", "Eastern Michigan University", "The University of Akron", "Ohio University", 
    "The Ohio State University", "Clemson University", "The University of South Carolina", "Middle Tennessee State University", "The University of Texas at Austin", "The University of North Texas", "Texas Tech University", "Utah State University", 
    "Brigham Young University", "Virginia Polytechnic Institute and State University", "Marshall University", "West Virginia University"),
    state = c("arkansas", "arkansas", "colorado", "idaho", "idaho", "iowa", "iowa", "kansas", "michigan", "michigan", "michigan", "michigan", "ohio", "ohio", "ohio", "south carolina", "south carolina", "tennessee", "texas", "texas", "texas", "utah", "utah", "virginia", "west virginia", "west virginia")
)

schedule_teams_by_loc <- schedules %>%
    filter(home_team_location %in% school_schedule$home_team_location | away_team_location %in% school_schedule$home_team_location)

schedule_teams_by_name <- schedules %>%
    filter(home_team_full %in% school_schedule$team_full_name | away_team_full %in% school_schedule$team_full_name)

a_offenses_per_ori <- a_offenses_per_ori %>% rename(date = incident_date)

#Crime on game days vs. non-game days
schedules_extended <- schedules %>% rename(date = game_date) %>% 
    inner_join(school_schedule, by = c("home_team_location"))

data <- a_offenses_per_ori %>% left_join(schedules_extended, by = c("city_name", "date"))

# Daily counts per agency 
data %>% group_by(date,city_name)  %>% summarize (count = n())