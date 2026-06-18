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

#joined schedules with tibble 
schedules_extended <- schedules  %>% rename(date = game_date) %>% 
filter((home_team_location %in% school_schedule$home_team_location) | (away_team_location %in% school_schedule$home_team_location) )

#home game and away games for home,away and none days 
home_games <- schedules_extended %>% inner_join(school_schedule, by="home_team_location" ) %>% mutate(is_home=TRUE) 
 

away_games <- schedules_extended %>% inner_join(school_schedule, by=c("away_team_location" = "home_team_location") ) %>%mutate(is_home=FALSE)

#join 
all_games <- bind_rows(home_games,away_games)

a_offenses_per_ori <- a_offenses_per_ori %>% rename(date = incident_date )

#conditon for duplicates to help many to many error for join 
all_games <- all_games %>% group_by(city_name, date) %>%summarize (
    home_game = any(is_home == TRUE), 
    away_game = any(is_home == FALSE),
    .groups = "drop")

#join with offense table 

join <-a_offenses_per_ori %>% left_join( all_games,by = c("city_name", "date") )

# observations total 
#join %>% count(home_game, away_game)

#mean offense counts 
offense_count <- join %>% mutate(game_type = case_when(home_game == TRUE ~ "home",
home_game == FALSE ~ "away",
is.na(home_game) ~ "none" ))%>% 
group_by(city_name,date,home_game,away_game,game_type) %>% summarize ( offense_counts = n()) 

offense_mean <- offense_count %>% group_by(game_type)%>% summarize(mean = mean(offense_counts))
offense_mean


#Total table

y = 2000:2005

date<- map_dfr(y, function(yr)(
    data.frame(date = seq(as.Date(sprintf('%d-08-20',yr)),
    as.Date(sprintf('%d-12-10',yr)),by ="day"))
)
)
names(date)

agency <- data.frame(
 
    ori = c("AR0720100", "AR0160100", "CO0210100", "ID0010100", "ID0290500", "IA0850100", "IA0520200", "KS0230100", "MI8121800", "MI3336400", "MI3949900", "MI3759900", "OH0770100", "OH0050100", "OHCOP0000", "SC0390200", "SC0400100", "TN0750100", "TX2270100", "TX0610200", "TX1520000", "UT0030100", "UT0250600", "VA0600100", "WV0060200", "WV0310100")
)
agency
table <- crossing(date,agency )
table <- table %>%  mutate(weekday = weekdays(date))
table
table %>% left_join(total_offenses_use) %>%  group_by(offense_type,home_or_away) %>% summarize(count = n()) 
table %>% left_join(total_offenses_use) %>% nrow
