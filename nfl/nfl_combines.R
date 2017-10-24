# author: @lrdegeest
# scrape NFL combine data

library(XML)

# base url defintions
base_url_prefix <- "http://nflcombineresults.com/nflcombinedata.php?year="
base_url_suffix <- "&pos=&college="

# scraper
get_combine <- function(x) {
  raw <- try(readHTMLTable(url, header = T, trim = T)) # function returns a list; first entry is scrapped data frame
  # pull at random intervals
  if(class(raw)=='try-error') next;
  Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  # just keep the dt
  df <- raw[[1]]
  # do a bunch of cleaning. coming back for this.
  return(df)
}

# looper
combines <- list()
for(i in 2016:2017) {
  url <- paste0(base_url_prefix, i, base_url_suffix)
  df <- get_combine(url)
  df.name <- paste0("combine.", i)
  combines[[df.name]] <- df
}




# view a combine
View(combines$combine.1987)
