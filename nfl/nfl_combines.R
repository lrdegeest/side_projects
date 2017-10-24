# author: @lrdegeest
# scrape NFL combine data

library(XML)

# base url defintions
base_url_prefix <- "http://nflcombineresults.com/nflcombinedata.php?year="
base_url_suffix <- "&pos=&college="

# scraper
get_combine <- function(x) {
  raw <- readHTMLTable(url, header = T, trim = T) # function returns a list; first entry is scrapped data frame
  df <- raw[[1]]
  # do a bunch of cleaning. coming back for this.
  return(df)
}

# looper
combines <- list()
for(i in 1987:2017) {
  url <- paste0(base_url_prefix, i, base_url_suffix)
  df <- try(get_combine(url))
  df.name <- paste0("combine.", i)
  combines[[df.name]] <- df
}

# view a combine
View(combines$combine.1987)
