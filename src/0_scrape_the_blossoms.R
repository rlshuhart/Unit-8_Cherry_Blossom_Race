library(httr)
library(rvest)
library(stringr)

divisions <- c("Overall+Men", "Overall+Women")
gender <- c("M", "W")
event <- "10M"
year <- c(2016, 2015, 2014)

get_web_table <- function(division, gender, event, year, page){
  # p for page; Webpage starts at 1

  root_url <- "http://www.cballtimeresults.org/performances?"
  url <- paste(root_url, 
               "division=", division,
               "&page=", page,
               "&section=",event,
               "&sex=",gender,
               "&year=", year, 
               sep="")

  # Read page 1
  # Used selectorgadget to determine the html node
  web_table <-read_html(GET(url, add_headers('user-agent' = 'r'))) %>% 
    html_node(".rwd-table") %>% 
    html_table()
  
  return(web_table)
}

get_web_data <- function(division, gender, event, year){
  
  # Page 1 gather first to determine how many pages of data to retrieve
  web_table <- get_web_table(division, gender, event, year, 1)
  
  # Get total expected results and round up to next int for the max pages. 
  # ceiling function accomplishes the rounding up.
  pages <- ceiling(as.numeric(str_split(web_table$`PiS/TiS`[1], "/", 
                                        simplify = TRUE)[2])/20)
  
  # Get the remaining pages of data
  for (p in 2:pages){
    print(paste("Retrieving page:", p))
    table <- get_web_table(division, gender, event, year, p)
    web_table <- rbind(web_table, table)
  }
  
  return(web_table)
}

df <- get_web_data(divisions[1], gender[1], event, year[1])
