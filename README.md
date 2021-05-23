# DI_College_Hockey_scraper
 
**Please note - if you use this scraper for any project I would love to be cited! All feedback is very helpful so feel free to let me know if you catch any inconsistencies.**

## NCAA DI Scraper Walkthrough

### NCAA_TD_scrape 
The NCAA_TD_scrape function is used to scrape a season of data from the College Hockey News aggregated website of existing college hockey data from 2005-present. A data frame of the requested data is returned from the function. Currently only a single season may be selected at a time. In the future, the goal is to be able to select multiple seasons or individual teams. 

### NCAA_PD_scrape
The NCAA_PD_scrape function is used to scrape player data for specific season of data from the College Hockey News website (years 2005-present). A data frame of the requested data is returned from the function. Currently only a single season may be selected at a time. In the future, the goal is to be able to select multiple seasons or individual players. 


## Input Definitions
### Season:
• A string representing a full season - example: "20182019""

### Example use:

NCAA_TD_scrape("20282019") 
