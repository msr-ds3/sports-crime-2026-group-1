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
    ori = c("AR0720100", "AR0160100", "CO0210100", "ID0010100", "ID0290500", "IA0850100", "IA0520200", "KS0230100", "MI8121800", "MI3336400", "MI3949900", "MI3759900", "OH0770100", "OH0050100", "OHCOP0000", "SC0390200", "SC0400100", "TN0750100", "TX2270100", "TX0610200", "TX1520000", "UT0030100", "UT0250600", "VA0600100", "WV0060200", "WV0310100"),
    home_team_location = c("Arkansas", "Arkansas State" ,"Air Force", "Boise State", "Idaho","Iowa State", "Iowa", "Kansas","Michigan", 
    "Michigan State","Western Michigan","Eastern Michigan","Akron", "Ohio","Ohio State","Clemson", "USC", "Middle Tennessee","Texas","North Texas","Texas Tech","Utah State","BYU","Virginia Tech","Marshall","West Virginia"),
    ori_team = c("Arkansas Razorbacks", "Arkansas State Red Wolves", "Air Force Falcons", "Boise State Broncos", "Idaho Vandals", "Iowa State Cyclones", "Iowa Hawkeyes", "Kansas Jayhawks", 
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

# Each possible case of home/away games
# Case One: home team in set & away team not in set
# Case Two: away team in set & home team not in set
# Case Three home: home team in set & away team in set (but home team data)
# Case Three away: away team in set & away team in set (but away team data)
# case <- schedules %>%
#     filter(home_team_full %in% school_schedule$home_team_full | away_team_full %in% school_schedule$home_team_full) %>% 
#     mutate(home_or_away = "home", date = game_date) %>%
#     select(date, matchup, home_team_location, home_team_full, away_team_full, away_team_location, home_or_away) %>% 
#     mutate(ori_team = ifelse(home_or_away == "home", home_team_full, away_team_full))

#-------------------------------------------------
case_one <- schedules %>%
    filter(home_team_full %in% school_schedule$ori_team & !(away_team_full %in% school_schedule$ori_team)) %>% 
    mutate(home_or_away = "home", date = game_date) %>%
    select(date, matchup, game_id, home_team_location, home_team_full, away_team_full, away_team_location, home_or_away) %>%
    mutate(ori_team = ifelse(home_or_away == "home", home_team_full, away_team_full))

case_two <- schedules %>%
    filter(!(home_team_full %in% school_schedule$ori_team) & away_team_full %in% school_schedule$ori_team) %>% 
    mutate(home_or_away = "away", date = game_date) %>%
    select(date, matchup, game_id, home_team_location, home_team_full, away_team_full, away_team_location, home_or_away) %>%
    mutate(ori_team = ifelse(home_or_away == "home", home_team_full, away_team_full))

case_three_home <- schedules %>%
    filter(home_team_full %in% school_schedule$ori_team & away_team_full %in% school_schedule$ori_team) %>% 
    mutate(home_or_away = "home", date = game_date) %>%
    select(date, matchup, game_id, home_team_location, home_team_full, away_team_full, away_team_location, home_or_away) %>%
    mutate(ori_team = ifelse(home_or_away == "home", home_team_full, away_team_full))

case_three_away <- schedules %>%
    filter(home_team_full %in% school_schedule$ori_team & away_team_full %in% school_schedule$ori_team) %>% 
    mutate(home_or_away = "away", date = game_date) %>%
    select(date, matchup, game_id, home_team_location, home_team_full, away_team_full, away_team_location, home_or_away) %>%
    mutate(ori_team = ifelse(home_or_away == "home", home_team_full, away_team_full))
#--------------------------------------------------- 
nrow(case_one)
nrow(case_two)
nrow(case_three_home)
nrow(case_three_away)
# total games: 1104
# At this point, we have four dataframes, each with their unique game matches

case_one_game_data <- left_join(case_one, school_schedule, by = "ori_team") %>%
    inner_join(a_offenses_per_ori, by = c("ori", "date"))
# Case_one_game_data (all offenses on days with games by ori): 5,402

case_two_game_data <- left_join(case_two, school_schedule, by = "ori_team") %>%
    filter( !((date == as.Date("2000-09-23") & matchup == "Michigan Wolverines at James Madison Dukes") |
        (date == as.Date("2000-10-07") & matchup == "Akron Zips at Bowie State Bulldogs") |
        (date == as.Date("2000-10-14") & matchup == "Iowa Hawkeyes at James Madison Dukes"))  
    ) %>%
    inner_join(a_offenses_per_ori, by = c("ori", "date"))
# case_two_game_data (all offenses on days with games by ori by the away team): 4219

case_three_home_game_data <- left_join(case_three_home, school_schedule, by = "ori_team") %>%
    inner_join(a_offenses_per_ori, by = c("ori", "date"))
# case_three_home_game_data (all offenses on days with games by ori for the home team): 1658

case_three_away_game_data <- left_join(case_three_away, school_schedule, by = "ori_team") %>%
    inner_join(a_offenses_per_ori, by = c("ori", "date"))
# case_three_away_game_data (all offenses on days with games by ori by the away team): 1085

# ------------------------------------------------
# # Testing left joins
# case_one_game_data <- left_join(case_one, school_schedule, by = "ori_team") %>%
#     left_join(a_offenses_per_ori, by = c("ori", "date"))
# # Case_one_game_data (all offenses on days with games by ori): 5,402

# case_two_game_data <- left_join(case_two, school_schedule, by = "ori_team") %>%
#     filter( !((date == as.Date("2000-09-23") & matchup == "Michigan Wolverines at James Madison Dukes") |
#         (date == as.Date("2000-10-07") & matchup == "Akron Zips at Bowie State Bulldogs") |
#         (date == as.Date("2000-10-14") & matchup == "Iowa Hawkeyes at James Madison Dukes"))  
#     ) %>%
#     left_join(a_offenses_per_ori, by = c("ori", "date"))
# # case_two_game_data (all offenses on days with games by ori by the away team): 4219

# case_three_home_game_data <- left_join(case_three_home, school_schedule, by = "ori_team") %>%
#     left_join(a_offenses_per_ori, by = c("ori", "date"))
# # case_three_home_game_data (all offenses on days with games by ori for the home team): 1658

# case_three_away_game_data <- left_join(case_three_away, school_schedule, by = "ori_team") %>%
#     left_join(a_offenses_per_ori, by = c("ori", "date"))
# # case_three_away_game_data (all offenses on days with games by ori by the away team): 1085
# -------------------------------------------------------
nrow(case_one_game_data)
nrow(case_two_game_data)
nrow(case_three_home_game_data)
nrow(case_three_away_game_data)
# total offenses: 12364
# total lf offenses: 12625
# --------------------------------------------------
# Combining cases
total_games <- bind_rows(case_one, case_two, case_three_home, case_three_away)
names(total_games)
total_games_use <- select(total_games, date, game_id, ori_team, home_team_full, away_team_full, home_or_away)

total_offenses <- bind_rows(case_one_game_data, case_two_game_data, case_three_home_game_data, case_three_away_game_data)
names(total_offenses)
total_offenses_use <- select(total_offenses, date, ori, ori_team, game_id, home_or_away, offense_type)

# Appendix Table One Descriptive Statistics
# Maybe filter only games between (Aug 20 to Dec 10)
# For each team, get assault count + compute average over
# All Days (112 days by 6 years = 672 days)
# No Game (672 days - # of game days (home and away))
# Home Games (# of Home Games)
# Away Games (# of Away Games)
games_ranged <- filter(total_games_use, (yday(date) >= 233 & yday(date) <= 345))


group_by(total_offenses_use, ori, home_or_away, offense_type) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = offense_type, values_from = count)


# total home games
filter(games_ranged, home_or_away == "home")

# total away games
filter(games_ranged, home_or_away == "away")

# Table Two - done (tentative)
# Distribution of Game Days by Day of Week
# For the large df of all games and all offenses
mutate(total_offenses_use, day_of_week = wday(date)) %>%
    group_by(day_of_week) %>%
    summarise(count = n())

mutate(total_games_use, day_of_week = wday(date)) %>%
    group_by(day_of_week) %>%
    summarise(count = n())

# ______________________________________________________________________
# ______________________________________________________________________

# debugging
group_by(case_two_game_data, ori, date) # 381
# repeats
#   IA520200 2000 10 14 vs Illinois Fighting correct
#   MI8121800 2000 09 23 vs Bowling Green correct
#   OH0770100 2000 10 07 vs Illinois Fighting 
#   Removing 2000923 JamesMadisonWolverines, 20001007 BowieStateZips, 20001014 JamesMadisonHawkeyes
add_count(case_two_game_data, ori, date) %>% arrange(desc(n)) %>% view

# Old Work
# aligning_schedule_with_agency <- left_join(schedule_teams_by_name, school_schedule, by = "home_team_full")
# all_data_including_no_game <- left_join(a_offenses_per_ori, data, by = c("ori", "date"))
# only_game_data <- inner_join(a_offenses_per_ori, data, by = c("ori", "date"))