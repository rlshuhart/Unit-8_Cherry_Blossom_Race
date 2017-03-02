library(httr)
library(rvest)
library(stringr)

divisions <- c("Overall+Men", "Overall+Women")
gender <- c("M", "W")
event <- "10M"
year <- 2012:1999

get_web_table <- function(division, gender, event, year, page){
  
  root_url <- "http://www.cballtimeresults.org/performances?"
  url <- paste(root_url, 
               "division=", division,
               "&page=", page,
               "&section=",event,
               "&sex=",gender,
               "&year=", year, 
               sep="")

  # Used selectorgadget to determine the html node
  web_table <-read_html(GET(url, add_headers('user-agent' = 'r'))) %>% 
    html_node(".rwd-table") %>% 
    html_table()
  
  return(web_table)
}

get_web_data <- function(division, gender, event, year){
  
  # Page 1 gather first to determine how many pages of data to retrieve
  print(paste("Retrieving page: 1 | ", division,"/",year, sep=""))
  
  # Below block allows proxy adaption in order for Ryan to work on office network
  # Proxy setting only established if there is a failure without using it. 
  # No need to change for anyone else.
  tryCatch({web_table <- get_web_table(division, gender, event, year, 1)},
           error = function(e) {
             proxy <- str_split(Sys.getenv("https_proxy"),"://", simplify = TRUE)[2]
             ip <- str_split(proxy,":", simplify = TRUE)[1]
             port <- as.numeric(str_split(proxy,":", simplify = TRUE)[2])
             set_config(use_proxy(url=ip,port=port))
           },
           finally = {
             web_table <- get_web_table(division, gender, event, year, 1)
           }
  )
  
  # Get total expected results and round up to next int for the max pages. 
  # ceiling function accomplishes the rounding up.
  pages <- ceiling(as.numeric(str_split(web_table$`PiS/TiS`[1], "/", 
                                        simplify = TRUE)[2])/20)
  
  # Get the remaining pages of data
  for (p in 2:pages){
    print(paste("Retrieving page: ", p, " of ", pages," | ", division,"/",year, sep=""))
    table <- get_web_table(division, gender, event, year, p)
    web_table <- rbind(web_table, table)
  }
  
  # Additional Fields
  web_table$Sex <- gender
  web_table$Year <- year
  web_table$Event <- event
  
  return(web_table)
}

save_the_blossums <- function(run_data, file_name){
  saveRDS(run_data, paste("./data/external/", file_name,sep=""))
}


###### Scrape the Data
run_data <- data.frame()
for (y in 1:length(year)){
  for (d in 1:length(divisions)){
    new_runs <- get_web_data(divisions[d], gender[d], event, year[y])
    run_data <- rbind(run_data, new_runs)
  }
}

fname <- paste("run_data",min(year),"-",max(year),".rds", sep="")
save_the_blossums(run_data, fname)