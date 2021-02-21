install.packages("rvest")
install.packages("sjmisc")
library(rvest)
library(dplyr)
library(sjmisc)

##-----------------------------------------------------------------------##
##                           Prep Work                                   ##
##-----------------------------------------------------------------------##
# Set Team Name list
# Team Name list is in alphabetical order, this is important due 
# to the fact that the url's for each teams page includes a number that 
# represents its location (1-62) in an alphabetical list of all the teams

team_names  <- c("Air-Force","Alabama-Huntsville","Alaska-Anchorage","Alaska","American-Intl",
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

# number 56 "Wayne State"is ignored as data only extends up to the year 2008
# number 62 "Long-Island" ignored for historical analysis as Long Island was just added as a D1 team in 2020
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
                 "Western Michigan","Wisconsin","Yale","Penn State","Arizona State","Long-Island")

# Create team table with team number and team
team_table<- data.frame(team_names_off,team_numbers)
team_table <- team_table[-56,]
team_table <- team_table[-62,]

# Rename columns
colnames(team_table) <- c("Team", "Team#")


##-----------------------------------------------------------------------##
##                       SCRAPE TEAM DATA                                ##
##-----------------------------------------------------------------------##
# https://www.collegehockeynews.com/stats/ retrieves current season stats
# https://www.collegehockeynews.com/stats/?season=20142015 retrieves 20142015 season stats
# season stat data available from 20202021 season to 20052006 season
# Be aware that column names represent what is specified on College Hockey News Website
#--------------------------------------------------------------------------

# IMPORTANT
# Specify year(s) of team data you would like to pull
td_years <- c("20192020","20182019","20172018","20162017","20152016","20142015")


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


# Create For Loop to create list of all URL's that will be fed into the export for-loop
list.of.urls <- list()
j <- 0
for (i in team_names) {
  if (j == 55){
    j <- j + 2 #Skip team number 56 as the data is not present after 2008
  } else {
    j <- j + 1 #increment team number by one every loop 
  }
  recordpage <- paste0("https://www.collegehockeynews.com/reports/teamHistory/",i,"/", j)
  pull.name <- paste0(i)
  list.of.urls[[pull.name]] <- recordpage
}

m <- 0 #set team number to zero 
team_record <- data.frame() # create data frame for scraped player data

for (k in list.of.urls) {
  if (m == 55){
    m <- m + 2 #Skip team number 56 as the data is not present after 2008
  } else {
    m <- m + 1 #increment team number by one every loop 
  }
  
  statpage <- paste0(k) # set URL from list 
  page <- read_html(statpage) # read HTML in from URL selected 
  
  #Scrape Data
  tbls <- html_nodes(page, "table")
  tbls_table <- html_table(tbls,fill=TRUE)
  record.table <- as.data.frame(tbls_table[1])
  record.table = record.table[,-c(11:100)] # delete N/A Columns
  record.table['Team#'] = m # m represents team position in alphabetical list
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

##-----------------------------------------------------------------------##
##                     SCRAPE PLAYER DATA                                ##
##-----------------------------------------------------------------------##


# Set Season list
season_yr <- c("20192020")#,"20182019","20172018","20162017","20152016"

#Enter file name you wish to save as
file_name <- "20182019season.csv" 


# Create For Loop to create list of all URL's that will be fed into the export for-loop
list.of.urls <- list()
j <- 0
for (i in team_names) {
  if (j == 55){
    j <- j + 2 #Skip team number 56 as the data is not present after 2008
  } else {
    j <- j + 1 #increment team number by one every loop 
  }
  statpage <- paste0("https://www.collegehockeynews.com/stats/team/",i,"/", j, "/overall,",season_yr)
  pull.name <- paste0(i)
  list.of.urls[[pull.name]] <- statpage
  }

# Scrape Skater and Goalie stats for all teams in a given Season
m <- 0 #set team number to zero 
player_data <- data.frame() # create data frame for scraped player data
goalie_data <- data.frame()
for (k in list.of.urls) {
  if (m == 55){
    m <- m + 2 #Skip team number 56 as the data is not present after 2008
  } else {
    m <- m + 1 #increment team number by one every loop 
  }
  
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
  goalie_table['Team#'] = m  # m represents team position in alphabetical list
  goalie_table['Season'] = season_yr #season data is being pulled for
  goalie_table$SA = goalie_table$GA + goalie_table$SV
  goalie_data <- rbind(goalie_table,goalie_data)
  
  #Skater Table cleanup - add column names & delete total row and column heading rows
  colnames(skater_table) = skater_table[1, ] # the first row will be the header
  skater_table = skater_table[-1, ] # delete row that was only column names
  skater_table <- skater_table[-nrow(skater_table),]
  
  # Add season and Team Columns
  skater_table['Team#'] = m # m represents team position in alphabetical list
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


# If you want to Export Data to CSV File un-comment the code on the line below
# write.csv(data_pull, file=file_name, row.names=FALSE)



  
  