install.packages("rvest")
install.packages("sjmisc")
library(rvest)
library(dplyr)
library(sjmisc)
library(tidyverse)

##-----------------------------------------------------------------------##
##                           URL SOURCE                                  ##
##-----------------------------------------------------------------------##
# https://www.collegehockeynews.com/stats/ retrieves current season stats
# https://www.collegehockeynews.com/stats/?season=20142015 retrieves 
# 20142015 season stats
# season stat data available from 20202021 season to 20082009 season
# Be aware that column names represent what is specified on College Hockey 
# News Website
#--------------------------------------------------------------------------


##-----------------------------------------------------------------------##
##                           Prep Work                                   ##
##-----------------------------------------------------------------------##
# Set Team Name list
# Team Name list is in alphabetical order, this is important due 
# to the fact that the url's for each teams page includes a number that 
# represents its location (1-62) in an alphabetical list of all the teams

team_names_20202021  <- c("Air-Force","Alabama-Huntsville","American-Intl",
                         "Army","Bemidji-State","Bentley","Boston-College","Boston-University",
                         "Bowling-Green","Canisius","Clarkson", "Colgate","Colorado-College",
                         "Connecticut","Denver","Ferris-State","Holy-Cross","Lake-Superior","Maine",
                         "Mass-Lowell","Massachusetts","Mercyhurst","Merrimack","Miami","Michigan",
                         "Michigan-State","Michigan-Tech","Minnesota","Minnesota-State","Minnesota-Duluth",
                         "Nebraska-Omaha","New-Hampshire","Niagara","North-Dakota","Northeastern",
                         "Northern-Michigan","Notre-Dame","Ohio-State",
                         "Providence","Quinnipiac","RIT","Robert-Morris",
                         "Sacred-Heart","St-Cloud-State","St-Lawrence","Vermont",
                         "Western-Michigan","Wisconsin","Penn-State","Arizona-State","Long-Island")

# Excluded teams due to opt outs -> 3,4,12,18,19,22,45,48,54,59
# "Alaska-Anchorage", "Alaska","Brown","Cornell", "Dartmouth","Harvard","Princeton","Rensselaer","Union", "Yale"
# number 56 "Wayne State"is ignored as data only extends up to the year 2008


team_names_20152020 <- c("Air-Force","Alabama-Huntsville","Alaska-Anchorage","Alaska","American-Intl",
                        "Army","Bemidji-State","Bentley","Boston-College","Boston-University",
                        "Bowling-Green","Brown","Canisius","Clarkson", "Colgate","Colorado-College",
                        "Connecticut","Cornell","Dartmouth", "Denver","Ferris-State","Harvard",
                        "Holy-Cross","Lake-Superior","Maine","Mass-Lowell","Massachusetts","Mercyhurst",
                        "Merrimack","Miami","Michigan","Michigan-State","Michigan-Tech","Minnesota",
                        "Minnesota-State","Minnesota-Duluth","Nebraska-Omaha","New-Hampshire","Niagara",
                        "North-Dakota","Northeastern","Northern-Michigan","Notre-Dame","Ohio-State",
                        "Princeton","Providence","Quinnipiac","Rensselaer","RIT","Robert-Morris",
                        "Sacred-Heart","St-Cloud-State","St-Lawrence","Union","Vermont",
                        "Western-Michigan","Wisconsin","Yale","Penn-State","Arizona-State")
# number 62 long Island is ignored here as it was not a recognized div 1 program until 2020 season

team_names_20082015 <- c("Air-Force","Alabama-Huntsville","Alaska-Anchorage","Alaska","American-Intl",
                         "Army","Bemidji-State","Bentley","Boston-College","Boston-University",
                         "Bowling-Green","Brown","Canisius","Clarkson", "Colgate","Colorado-College",
                         "Connecticut","Cornell","Dartmouth", "Denver","Ferris-State","Harvard",
                         "Holy-Cross","Lake-Superior","Maine","Mass-Lowell","Massachusetts","Mercyhurst",
                         "Merrimack","Miami","Michigan","Michigan-State","Michigan-Tech","Minnesota",
                         "Minnesota-State","Minnesota-Duluth","Nebraska-Omaha","New-Hampshire","Niagara",
                         "North-Dakota","Northeastern","Northern-Michigan","Notre-Dame","Ohio-State",
                         "Princeton","Providence","Quinnipiac","Rensselaer","RIT","Robert-Morris",
                         "Sacred-Heart","St-Cloud-State","St-Lawrence","Union","Vermont",
                         "Western-Michigan","Wisconsin","Yale","Penn-State")

# number 61 Arizona State is ignored here as it was not a recognized div 1 program until 2015-2016 season
# number 62 long Island is ignored here as it was not a recognized div 1 program until 2020 season

team_names_20052008 <-c("Air-Force","Alabama-Huntsville","Alaska-Anchorage","Alaska","American-Intl",
                         "Army","Bemidji-State","Bentley","Boston-College","Boston-University",
                         "Bowling-Green","Brown","Canisius","Clarkson", "Colgate","Colorado-College",
                         "Connecticut","Cornell","Dartmouth", "Denver","Ferris-State","Harvard",
                         "Holy-Cross","Lake-Superior","Maine","Mass-Lowell","Massachusetts","Mercyhurst",
                         "Merrimack","Miami","Michigan","Michigan-State","Michigan-Tech","Minnesota",
                         "Minnesota-State","Minnesota-Duluth","Nebraska-Omaha","New-Hampshire","Niagara",
                         "North-Dakota","Northeastern","Northern-Michigan","Notre-Dame","Ohio-State",
                         "Princeton","Providence","Quinnipiac","Rensselaer","RIT","Robert-Morris",
                         "Sacred-Heart","St-Cloud-State","St-Lawrence","Union","Vermont", "Wayne-State",
                         "Western-Michigan","Wisconsin","Yale","Penn-State")
# number 56 "Wayne State"is included here as it was a Div1 hockey program until 2008_2009 SEASON
# number 61 Arizona State is ignored here as it was not a recognized div 1 program until 2015-2016 season
# number 62 long Island is ignored here as it was not a recognized div 1 program until 2020 season

#Create team numbers list
team_numbers <- c(1:62)

#Team name list that will be used to add team number to dataset 
team_names_off <- c("Air Force","Alabama-Huntsville","Alaska-Anchorage","Alaska","American Int'l",
                    "Army","Bemidji State","Bentley","Boston College","Boston University",
                    "Bowling Green","Brown","Canisius","Clarkson", "Colgate","Colorado College",
                    "Connecticut","Cornell","Dartmouth", "Denver","Ferris State","Harvard",
                    "Holy Cross","Lake Superior","Maine","Mass.-Lowell","Massachusetts","Mercyhurst",
                    "Merrimack","Miami","Michigan","Michigan State","Michigan Tech","Minnesota",
                    "Minnesota State","Minnesota-Duluth","Nebraska-Omaha","New Hampshire","Niagara",
                    "North Dakota","Northeastern","Northern Michigan","Notre Dame","Ohio State",
                    "Princeton","Providence","Quinnipiac","Rensselaer","RIT","Robert Morris",
                    "Sacred Heart","St. Cloud State","St. Lawrence","Union","Vermont","Wayne State",
                    "Western Michigan","Wisconsin","Yale","Penn State","Arizona State","Long Island")

# Create team table with team number and team
team_table<- data.frame(team_names_off,team_numbers)

team_table_20202021 <- team_table[-c(3,4,12,18,19,22,45,48,54,56,59),]
team_table_20152020 <- team_table[-c(56,62),]
team_table_20082015 <- team_table[-c(56,61,62),]
team_table_20052008 <- team_table[-c(61,62),]

# Rename columns
colnames(team_table_20202021) <- c("Team", "Team#")
colnames(team_table_20152020) <- c("Team", "Team#")
colnames(team_table_20082015) <- c("Team", "Team#")
colnames(team_table_20052008) <- c("Team", "Team#")

##-----------------------------------------------------------------------##
##                           Scraper Function Code                       ##
##-----------------------------------------------------------------------##

# Year format xxxxyyyy (example 20202021)
NCAA_TD_scrape <- function(year){
  ##-----------------------------------------------------------------------##
  ##                       SCRAPE TEAM DATA                                ##
  ##-----------------------------------------------------------------------##
  
  # IMPORTANT
  # Specify year(s) of team data you would like to pull
  #td_years <- c("20192020","20182019","20172018","20162017","20152016","20142015")
  td_years <- c(year)
  
  #----------------------------------------------------------------------------
  list.of.team.urls <- list() # create list where URL's for team stats will be stored
  
  # create a for loop to create URL's for each specified season
  for (i in td_years) {
    teampage <- paste0("https://www.collegehockeynews.com/stats/?season=",i)
    season <- paste0(i)
    list.of.team.urls[[season]] <- teampage
  }
  
  
  # Create empty master dataframes for Standard Stats, Advanced Stats, and Additional Stats
  master_std_ts <- data.frame()
  master_adv_ts <- data.frame()
  master_add_ts <- data.frame()
  
  
  # scrape team data
  for (i in list.of.team.urls) {
    statpage <- paste0(i) # set URL from list 
    page <- read_html(statpage) # read HTML in from URL selected 
    
    #Scrape Data
    tbls <- html_nodes(page, "table")
    tbls_table <- html_table(tbls,fill=TRUE)
    std_ts <- as.data.frame(tbls_table[1])
    adv_ts <- as.data.frame(tbls_table[2])
    add_ts <- as.data.frame(tbls_table[3])
    
    #table cleanup - delete header rows
    adv_ts =  adv_ts[-c(1:2), ]
    add_ts = add_ts[-c(1:2), ]
    
    #----------------------------------------------------------------------------#
    # Data cleanup - add column names & delete total row and column heading rows #
    #----------------------------------------------------------------------------#
    # Standard Stats
    colnames(std_ts) = std_ts[2, ] # the first row will be the header
    std_ts <- std_ts[-c(1:2), ]
    
    #Advanced Stats
    colnames(adv_ts) = adv_ts[1, ] # the first row will be the header
    adv_ts = adv_ts[-1, ] # delete row that was only column names
    
    #Additional Stats
    colnames(add_ts) = add_ts[1, ] # the second row will be the header
    add_ts = add_ts[,-c(22:27)] # delete N/A Columns
    add_ts = add_ts[-1, ] # delete row that was only column names
    
    
    # Add URL link column for each data frame
    adv_ts['URL'] = paste0(i)
    std_ts['URL'] = paste0(i)
    add_ts['URL'] = paste0(i)
    
    # Extract Year from URL
    adv_ts['Season'] = basename(paste0(i))
    std_ts['Season'] = basename(paste0(i))
    add_ts['Season'] = basename(paste0(i))
    
    # Make column season years only
    std_ts$Season<- sub(".*=","",std_ts$Season)
    adv_ts$Season<- sub(".*=","",adv_ts$Season)
    add_ts$Season<- sub(".*=","",add_ts$Season)
    
    # Append tables to their respective master tables
    master_std_ts <- rbind (std_ts,master_std_ts)
    master_adv_ts <- rbind (adv_ts,master_adv_ts)
    master_add_ts <- rbind (add_ts,master_add_ts)
    
  }
  
  # Convert columns to correct data type
  master_std_ts[ ,c(3:15)] <- apply(master_std_ts[ ,c(3:15)], 2,function(x) as.numeric(as.character(x)))
  master_adv_ts[ ,c(3:28)] <- apply(master_adv_ts[ ,c(3:15)], 2,function(x) as.numeric(as.character(x)))
  master_add_ts[ ,c(3:21)] <- apply(master_add_ts[ ,c(3:21)], 2,function(x) as.numeric(as.character(x)))
  
  
  ##-----------------------------------------------------------------------##
  ##                       SCRAPE TEAM W/L/T Data                          ##
  ##-----------------------------------------------------------------------##
  # https://www.collegehockeynews.com/reports/teamHistory/Air-Force/1
  
  if(year == "20202021" ){
    team_names <- team_names_20202021
    team_table <- team_table_20202021
  } else if ((year == "20152016") | (year == "20162017") | (year == "20172018") | (year == "20182019") | (year == "20192020") ){
    team_names <- team_names_20152020
    team_table <- team_table_20152020
  }else if ((year == "20142015") | (year == "20132014") | (year == "20122013") |
            (year == "20112012") | (year == "20102011")| (year == "20092010") | (year == "20082009") ){
    team_names <- team_names_20082015
    team_table <- team_table_20082015
  } else {
    team_names <- team_names_20052008
    team_table <- team_table_20052008
  }
  
  
  # Create For Loop to create list of all URL's that will be fed into the export for-loop
  list.of.urls <- list()
  j <- 0
  for (z in team_names) {
    j <- j + 1
    recordpage <- paste0("https://www.collegehockeynews.com/reports/teamHistory/",z,"/", team_table$`Team#`[j])
    pull.name <- paste0(z)
    list.of.urls[[pull.name]] <- recordpage
  }
  
  m <- 0 #set team number to zero 
  team_record <- data.frame() # create data frame for scraped team records
  for (k in list.of.urls) {
    m <- m + 1 #increment team number by one every loop 
    statpage <- paste0(k) # set URL from list 
    page <- read_html(statpage) # read HTML in from URL selected 
    
    #Scrape Data
    tbls <- html_nodes(page, "table")
    tbls_table <- html_table(tbls,fill=TRUE)
    record.table <- as.data.frame(tbls_table[1])
    record.table = record.table[,-c(11:100)] # delete N/A Columns
    record.table['Team#'] = team_table$`Team#`[m] # m represents team position in alphabetical list
    team_record<- rbind (record.table,team_record)
    
  }
  
  
  
  ##-----------------------------------------------------------------------##
  ##                     Create Master Team Dataset                        ##
  ##-----------------------------------------------------------------------##
  
  # Link datasets using unique link column that is season and team#
  # This will allow us to have W/L/T information 
  # add team number to dataframes
  
  # Add team number to STD Data 
  master_std_ts<- merge.data.frame(master_std_ts,team_table, by="Team")
  master_adv_ts <- merge.data.frame(master_adv_ts,team_table, by="Team")
  master_add_ts<- merge.data.frame(master_add_ts,team_table, by="Team")
  
  # Create Season Start Year Column in the two datasets 
  # Since the data has different formats for season we need to use start year only
  master_std_ts['Season Start YR'] <- substr(master_std_ts$Season, 1, 4)
  team_record['Season Start YR'] <- substr(team_record$Season, 1, 4)
  
  # Add Link column to both DF
  master_std_ts['Link Column']<- paste(master_std_ts$`Team#`,master_std_ts$`Season Start YR`)
  team_record['Link Column'] <- paste(team_record$`Team#`,team_record$`Season Start YR`)
  
  # merge dataframes
  master_std_ts_WLT <- merge.data.frame(master_std_ts,team_record, by="Link Column")
  
  # convert columns to correct data type
  master_std_ts_WLT[ ,c(24:26)] <- apply(master_std_ts_WLT[ ,c(24:26)], 2,function(x) as.numeric(as.character(x)))
  
  td_list <- list(master_std_ts_WLT,master_adv_ts,master_add_ts)
  return(td_list)
}
NCAA_PD_scrape <- function(year){
  ##-----------------------------------------------------------------------##
  ##                     SCRAPE PLAYER DATA                                ##
  ##-----------------------------------------------------------------------##
  
  if(year == "20202021" ){
    team_names <- team_names_20202021
    team_table <- team_table_20202021
  } else if ((year == "20152016") | (year == "20162017") | (year == "20172018") | (year == "20182019") | (year == "20192020") ){
    team_names <- team_names_20152020
    team_table <- team_table_20152020
  }else if ((year == "20142015") | (year == "20132014") | (year == "20122013") |
            (year == "20112012") | (year == "20102011")| (year == "20092010") | (year == "20082009") ){
    team_names <- team_names_20082015
    team_table <- team_table_20082015
  } else {
    team_names <- team_names_20052008
    team_table <- team_table_20052008
  }
  
  # Set Season list
  season_yr <- year
  
  # Create For Loop to create list of all URL's that will be fed into the export for-loop
  list.of.urls <- list()
  j <- 0
  for (z in team_names) {
    j <- j + 1 #increment team number by one every loop 
    statpage <- paste0("https://www.collegehockeynews.com/stats/team/",z,"/", team_table$`Team#`[j])
    pull.name <- paste0(z)
    list.of.urls[[pull.name]] <- statpage
  }
  
  # Scrape Skater and Goalie stats for all teams in a given Season
  m <- 0 #set team number to zero 
  player_data <- data.frame() # create data frame for scraped player data
  goalie_data <- data.frame()
  for (k in list.of.urls) {
    m <- m + 1 #increment team number by one every loop 
    statpage <- paste0(k) # set URL from list 
    page <- read_html(statpage) # read HTML in from URL selected 
    
    #Scrape Data
    tbls <- html_nodes(page, "table")
    tbls_table <- html_table(tbls)
    skater_table <- as.data.frame(tbls_table[1])
    goalie_table <- as.data.frame(tbls_table[2])
    
    #table cleanup - delete header row
    skater_table = skater_table[-1, ] 
    goalie_table = goalie_table[-1, ] 
    
    #Goalie table cleanup - add column names & delete total row and column heading rows
    colnames(goalie_table) = goalie_table[1, ] # the first row will be the header
    goalie_table = goalie_table[-1, ] # delete row that was only column names
    goalie_table <- goalie_table[-nrow(goalie_table),]
    goalie_table[ ,c(2:11)] <- apply(goalie_table[ ,c(2:11)], 2,function(x) as.numeric(as.character(x)))
    
    # Add season and Team Columns
    goalie_table['Team#'] = team_table$`Team#`[m]  # m represents team position in alphabetical list
    goalie_table['Season'] = season_yr #season data is being pulled for
    goalie_table$SA = goalie_table$GA + goalie_table$SV
    goalie_data <- rbind(goalie_table,goalie_data)
    
    #Skater Table cleanup - add column names & delete total row and column heading rows
    colnames(skater_table) = skater_table[1, ] # the first row will be the header
    skater_table = skater_table[-1, ] # delete row that was only column names
    skater_table <- skater_table[-nrow(skater_table),]
    
    # Add season and Team Columns
    skater_table['Team#'] = team_table$`Team#`[m] # m represents team position in alphabetical list
    skater_table['Season'] = season_yr #season data is being pulled for 
    
    # Append table to Master Data frame
    player_data <- rbind(skater_table,player_data)
  }
  
  # Change column values to be correct data type
  player_data[ ,c(2:14)] <- apply(player_data[ ,c(2:14)], 2,function(x) as.numeric(as.character(x)))
  
  # Create Player Position Column
  player_data['Position'] = stringr::str_extract(player_data$`Name, Yr`, '(?<=,)[^,]+(?=,)')
  # Remove Leading space 
  player_data$Position<-trimws(player_data$Position, "l")
  
  # clean non standard position entries
  number_player_rows <- NROW(player_data)
  list_1 <- c(1:number_player_rows)
  entry_list <- list()
  
  for (i in list_1 ){
    if(player_data$Position[i] == "RW"){
      player_data$Position[i] <- "F"
    }
    if(player_data$Position[i] == "LW"){
      player_data$Position[i] <- "F"
    }
    if(str_contains(player_data$Position[i],"C")){
      player_data$Position[i] <- "F"
    }
    if(str_contains(player_data$Position[i],"D")){
      player_data$Position[i] <- "D"
    }
    if(str_contains(player_data$Position[i],"F")){
      player_data$Position[i] <- "F"
    }
  }
  return(player_data)
}
NCAA_AS_Calculations <- function(year){
  
  ##Year Selection
  if(year == "20202021" ){
    team_names <- team_names_20202021
    team_table <- team_table_20202021
  } else if ((year == "20152016") | (year == "20162017") | (year == "20172018") | (year == "20182019") | (year == "20192020") ){
    team_names <- team_names_20152020
    team_table <- team_table_20152020
  }else if ((year == "20142015") | (year == "20132014") | (year == "20122013") |
            (year == "20112012") | (year == "20102011")| (year == "20092010") | (year == "20082009") ){
    team_names <- team_names_20082015
    team_table <- team_table_20082015
  } else {
    team_names <- team_names_20052008
    team_table <- team_table_20052008
  }
 
  
  attach(master_std_ts_WLT)
  os <- (7/12) 
  league_avg_goals <- mean(G) # Average goals per team over the time frame of data scraped
  league_avg_goals_pg <- mean(G/GP) # Average League Goals per game
  master_std_ts_WLT['MGF'] <- G - (os * (GP * league_avg_goals_pg)) # Marginal Goals For
  master_std_ts_WLT['MGA'] <- ((1+os) * GP * league_avg_goals_pg) - GA  # Marginal Goals Against
  master_std_ts_WLT['LP'] <- W + (T/2) # League Points
  master_std_ts_WLT['LPPG'] <- master_std_ts_WLT$LP/G #League Points Per Goal Scored
  master_std_ts_WLT['EXP'] <- master_std_ts_WLT$LPPG * (master_std_ts_WLT$MGF + master_std_ts_WLT$MGA) # Expected Points
  mae <- sum(abs(master_std_ts_WLT$EXP - master_std_ts_WLT$LPPG)/nrow(master_std_ts_WLT)) # mean absolute error for entire dataset
  mae_p34 <- mae/34 #mean absolute error per 34 game season
  detach(master_std_ts_WLT)
  
  ##----------------------------------------------------------------------------------------------##
  ## Offensive Point Shares ##
  ##----------------------------------------------------------------------------------------------##
  # Create Filtered Data sets for the season wanted
  master_std_ts_WLT_filter<- filter(master_std_ts_WLT, Season.x == year)
  master_std_ts_WLT_filter <- master_std_ts_WLT_filter[,-33]
  names(master_std_ts_WLT_filter)[21] <- "Team#"
  mgpp <- sum(master_std_ts_WLT_filter$G)/sum(master_std_ts_WLT_filter$LP) # Marginal goals per point across the season
  #Calculate Team Assists
  team_assists <- list()
  j <- 0
  for (i in team_names) {
    j <- j + 1
    tn <- filter(player_data, player_data$`Team#` ==  team_table$`Team#`[j])
    ta <- sum(tn$A)
    pull.name <- paste0(i)
    team_assists[[pull.name]]<-ta
  }
  team_assists_df <-as.data.frame(do.call(rbind, team_assists))
  team_assists_df['Team#'] <- team_table$`Team#`
  names(team_assists_df)[1]<- "TA"
  player_data <- merge.data.frame(player_data,master_std_ts_WLT_filter[c(5,21)],by = "Team#")
  player_data <- merge.data.frame(player_data,team_assists_df,by = "Team#")
  names(player_data)[17] <- "TG"
  names(player_data)[4] <- "G"
  # Potentially Add shots as an added offensive contribution? 
  player_data['GC'] <- (player_data$G + (.5*player_data$A))*(player_data$TG/(player_data$TG + (.5*player_data$TA)))  #Goals created = 1 for goal or .5 for Assist
  
  
  #Offensive Players
  player_data_f <- data.frame(filter(player_data, player_data$Position == 'F')) # Filter by only forwards
  fgc <- sum((player_data_f$G + (.5*player_data_f$A))*(player_data_f$TG/(player_data_f$TG + (.5*player_data_f$TA)))) # Total goals created by forwards
  fgp <- sum(player_data_f$GP) # Total games played by forwards
  player_data_f['MGC'] <- player_data_f$GC - os*player_data_f$GP *(fgc/fgp) # Marginal Goals created
  player_data_f['OPS'] <- player_data_f$MGC/mgpp #Offensive Point Share
  
  #Defensive Players
  player_data_d <- data.frame(filter(player_data, player_data$Position == 'D'))
  dgc <- sum((player_data_d$G + (.5*player_data_d$A))*(player_data_d$TG/(player_data_d$TG + (.5*player_data_d$TA)))) # Goals created by defenders
  dgp <- sum(player_data_d$GP) # Total games played by Defenders 
  player_data_d['MGC'] <- player_data_d$GC - os*player_data_d$GP *(dgc/dgp) # Marginal goals created 
  player_data_d['OPS'] <- player_data_d$MGC/mgpp #Offensive Point Share
  
  
  ##----------------------------------------------------------------------------------------------##
  ## Defensive Point Shares ##
  ##----------------------------------------------------------------------------------------------##
  
  #Filter advanced stats
  master_adv_ts_filter <- filter(master_adv_ts, master_add_ts$Season == year )
  
  # Create list of total team games played by team defenders and forwards
  # Also create plus minus by player gorup (F or D) for a given team
  team_games_played_d <- list()
  team_games_played_f <- list()
  team_f_pm <- list()
  team_d_pm <- list()
  for (i in 1:62) {
    #Forwards
    tn <- filter(player_data_f, player_data_f$Team. == paste0(i))
    total_f_gp <- sum(tn$GP)
    total_f_pm <- mean(tn$X...)
    pull.name <- paste0(i)
    team_games_played_f[[pull.name]] <- total_f_gp
    team_f_pm[[pull.name]] <- total_f_pm
    #Defenders
    tn1 <- filter(player_data_d, player_data_d$Team. == paste0(i))
    total_d_gp <- sum(tn1$GP)
    total_d_pm <- mean(tn1$X...)
    pull.name <- paste0(i)
    team_games_played_d[[pull.name]] <- total_d_gp
    team_d_pm[[pull.name]] <- total_d_pm
  }
  # Create DF for team games played and PM by team for defenders
  team_games_played_d_df <- as.data.frame(do.call(rbind, team_games_played_d))
  team_games_played_d_df['Team.'] <- team_numbers #create column that has team numbers
  team_games_played_d_df['Position'] <- "D" # create column that specifies position
  names(team_games_played_d_df)[1] <- "TGPD" # rename column 1 TGPD which means total games played defense
  team_d_pm_df <-as.data.frame(do.call(rbind, team_d_pm)) # create d plus minus DF
  team_d_pm_df['Team.'] <- team_numbers #create column that has team numbers
  names(team_d_pm_df)[1] <- "TPMD"
  team_games_played_d_df <- merge(team_games_played_d_df,team_d_pm_df, by = "Team.") # merge dataframes
  team_games_played_d_df <- na.omit(team_games_played_d_df)
  # Add Shots
  team_games_played_d_df['Team_Shots'] <- master_std_ts_WLT_filter$Sh
  # Create DF for team games played by team for forwards
  team_games_played_f_df <- as.data.frame(do.call(rbind, team_games_played_f))
  team_games_played_f_df['Team.'] <- team_numbers
  team_games_played_f_df['Position'] <- "F"
  names(team_games_played_f_df)[1] <- "TGPF"
  team_f_pm_df <-as.data.frame(do.call(rbind, team_f_pm)) # create d plus minus DF
  team_f_pm_df['Team.'] <- team_numbers #create column that has team numbers
  names(team_f_pm_df)[1] <- "TPMF"
  team_games_played_f_df <- merge(team_games_played_f_df,team_f_pm_df, by = "Team.") # merge dataframes
  team_games_played_f_df <- na.omit(team_games_played_f_df)
  team_games_played_f_df['TGPD'] <- team_games_played_d_df$TGPD
  team_games_played_d_df['TGPF'] <- team_games_played_f_df$TGPF
  
  # create team shot/goals DF
  team_shots <- data.frame(master_std_ts_WLT_filter$Sh,master_std_ts_WLT_filter$ShA,
                           master_std_ts_WLT_filter$`Team#`,master_std_ts_WLT_filter$GA,
                           master_std_ts_WLT_filter$GP)
  names(team_shots)[3] <- "Team#" # team number
  names(team_shots)[1] <- "TSF" # team shots for on net
  names(team_shots)[2] <- "TSA" # team shots against on net
  names(team_shots)[4] <- "TGA" # team goals against
  names(team_shots)[5] <- "TGP" # team games played
  
  master_adv_ts_filter_merge <- master_adv_ts_filter[,c(4,5,30)]
  names(master_adv_ts_filter_merge)[3] <- "Team#"
  team_shots <- merge.data.frame(team_shots,master_adv_ts_filter_merge,by = "Team#")
  names(team_shots)[1] <- "Team." # team games played
  
  # Add team games played and team shots for and against to player data
  # Forwards
  player_data_f <- merge.data.frame(player_data_f, team_games_played_f_df,by ="Team.") # team games played
  player_data_f <- merge.data.frame(player_data_f, team_shots,by ="Team.")# add team shots
  names(player_data_f)[14] <- "PM"
  player_data_f['C'] <- player_data_f$SAT - player_data_f$SATA
  
  # Defenders
  player_data_d <- merge.data.frame(player_data_d, team_games_played_d_df,by ="Team.") # team games played
  player_data_d <- merge.data.frame(player_data_d, team_shots,by ="Team.") # add team shots
  player_data_d['C'] <- player_data_d$SAT - player_data_d$SATA # Team Corsi
  player_data_d['C60'] <- (player_data_d$SAT - player_data_d$SATA)/(60*player_data_d$GP) # Team corsi per 60
  names(player_data_d)[14] <- "PM"
  
  # Calculations #
  LSAPM <- (sum(master_std_ts_WLT_filter$ShA)/sum(master_std_ts_WLT_filter$GP))/60 # League Shots against per minute
  LGPG <- sum(master_std_ts_WLT_filter$G)/sum(master_std_ts_WLT_filter$GP) # league goals per game
  PAF <- 5/7 # Point adjustment Forwards
  PAD <- 10/7 # Point adjustment Defenders
  
  
  #Offensive Players
  
  attach(player_data_f)
  player_data_f['PWTGP'] <- GP/(TGPF + 2 * TGPD) # Proportion of weighted team games for each forward skater
  player_data_f['PTMGA'] <- (7-2 *(((TSA/GP)/60)/LSAPM))/7 # Proportion of team marginal goals against that will be assigned to skaters
  player_data_f['TMGA'] <- (1 + (7 / 12)) * TGP * LGPG - TGA
  player_data_f['PMA'] <- (1/7)* PAF * (PM - GP * (TPMF/TGPF))
  player_data_f['SMGA'] <- player_data_f$PWTGP * player_data_f$PTMGA * PAD * player_data_f$TMGA + player_data_f$PMA
  player_data_f['DPS'] <- player_data_f$SMGA/mgpp
  player_data_f['PS'] <- player_data_f$OPS + player_data_f$DPS
  detach(player_data_f)
  
  #Defensive Players
  attach(player_data_d)
  player_data_d['PWTGP'] <- (2*GP)/(TGPF + 2 * TGPD) # Proportion of weighted team games for each defender skater
  player_data_d['PTMGA'] <- (7-2 *(((TSA/GP)/60)/LSAPM))/7 # Proportion of team marginal goals against that will be assigned to skaters
  player_data_d['TMGA'] <-  (1 + (7 / 12)) * TGP * LGPG - TGA
  player_data_d['PMA'] <- (1/7)* PAD * ((PM-GP) * (TPMD/TGPD))
  player_data_d['SMGA'] <- player_data_d$PWTGP *  player_data_d$PTMGA * PAD *  player_data_d$TMGA +  player_data_d$PMA
  player_data_d['DPS'] <- player_data_d$SMGA/mgpp
  player_data_d['PS'] <- player_data_d$OPS + player_data_d$DPS
  detach(player_data_d)
  adv_player_data <- list(player_data_d,player_data_f)
  return(adv_player_data)
}
  


##-----------------------------------------------------------------------##
##                   Example Code Full Scrape 2020                       ##
##-----------------------------------------------------------------------##
team_data <- NCAA_TD_scrape("20202021")
player_data <- NCAA_PD_scrape("20202021")
master_std_ts_WLT <- as.data.frame(team_data[1])
master_adv_ts <- as.data.frame(team_data[2])
master_add_ts <- as.data.frame(team_data[3])
advanced_stat <- NCAA_AS_Calculations("20202021")
