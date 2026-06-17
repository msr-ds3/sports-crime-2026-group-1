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
    home_team_full = c("Arkansas Razorbacks", "Arkansas State Red Wolves", "Air Force Falcons", "Boise State Broncos", "Idaho Vandals", "Iowa State Cyclones", "Iowa Hawkeyes", "Kansas Jayhawks", 
    "Michigan Wolverines", "Michigan State Spartans", "Western Michigan Broncos", "Eastern Michigan Eagles", "Akron Zips", "Ohio Bobcats", "Ohio State Buckeyes", "Clemson Tigers", "South Carolina Gamecocks",
     "Middle Tennessee Blue Raiders", "Texas Longhorns", "North Texas Mean Green", "Texas Tech Red Raiders", "Utah State Aggies", "BYU Cougars", "Virginia Tech Hokies", "Marshall Thundering Herd", 
     "West Virginia Mountaineers"), 
    city_name = c("fayetteville","jonesboro","colorado springs","boise","moscow","ames","iowa city","lawrence","ann arbor","east lansing","kalamazoo","mount pleasant","akron","athens","columbus","clemson","columbia","murfreesboro","austin","denton","lubbock","logan","provo","blacksburg","huntington","morgantown"),
    school_name = c("University of Arkansas", "Arkansas State University", "United States Air Force Academy", "
    Boise State University", "The University of Idaho", "Iowa State University", "The University of Iowa", "The University of Kansas", "The University of Michigan", "Michigan State University", "Western Michigan University", "Eastern Michigan University", "The University of Akron", "Ohio University", 
    "The Ohio State University", "Clemson University", "The University of South Carolina", "Middle Tennessee State University", "The University of Texas at Austin", "The University of North Texas", "Texas Tech University", "Utah State University", 
    "Brigham Young University", "Virginia Polytechnic Institute and State University", "Marshall University", "West Virginia University")#,
    #state = c("arkansas", "arkansas", "colorado", "idaho", "idaho", "iowa", "iowa", "kansas", "michigan", "michigan", "michigan", "michigan", "ohio", "ohio", "ohio", "south carolina", "south carolina", "tennessee", "texas", "texas", "texas", "utah", "utah", "virginia", "west virginia", "west virginia")
)

schedule_teams_by_name <- schedules %>%
    filter(home_team_full %in% school_schedule$home_team_full) %>% 
    mutate(is_home = (home_team_full %in% school_schedule$home_team_full), date = game_date)%>%
    select(date, matchup, home_team_location, home_team_full, away_team_full, is_home)


data <- schedule_teams_by_name %>% inner_join(school_schedule, by = "home_team_location" )
data

a_offenses_per_ori <- a_offenses_per_ori %>% rename(date = incident_date)

a_offenses_per_ori %>% left_join(data)

a_offenses_per_ori

full_schedule <- a_offenses_per_ori %>% group_by(ori, city_name,date) %>% summarize(offense_count = n()) 
#%>% 
#inner_join()
join <-full_schedule %>% left_join(data, by=c("city_name","date") )
# pos_cor_2 <- left_join(a_offenses_per_ori, data, by = c("date", "city_name"))


join  %>% mutate(home = case_when(
    away_team_full %in% school_schedule$home_team_full ~ 0,
    !away_team_full %in% school_schedule$home_team_full & !is.na(away_team_full) ~ 1,
    !away_team_full %in% school_schedule$home_team_full & is.na(away_team_full) ~ NA
)) %>% select(away_team_full, home_team_full.x, home)


#possibly_correct_join <- group_by(a_offenses_per_ori, date, city_name) # %>% 
 #   summarize(offense_count = n()) #%>%
  #  left_join(schedule_teams_by_name, by = c("date", "home_team_full"))


# Daily counts per agency 
data %>% group_by(date,city_name)  %>% summarize (count = n())
