install.packages("rvest")
install.packages("sjmisc")
library(rvest)
library(dplyr)
library(sjmisc)



master_std_stats <- NCAA_Team_Data_scrape("")

##-----------------------------------------------------------------------##
##                           URL SOURCE                                  ##
##-----------------------------------------------------------------------##
# https://www.collegehockeynews.com/stats/ retrieves current season stats
# https://www.collegehockeynews.com/stats/?season=20142015 retrieves 20142015 season stats
# season stat data available from 20202021 season to 20082009 season
# Be aware that column names represent what is specified on College Hockey News Website
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
NCAA_Team_Data_scrape <- function(year){
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
  
  team_names <- if(year == "20202021" ){
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
    recordpage <- paste0("https://www.collegehockeynews.com/reports/teamHistory/",team_names$Team[j],"/", team_table$`Team#`[j])
    pull.name <- paste0(i)
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
  
  
  return(master_std_ts)
}