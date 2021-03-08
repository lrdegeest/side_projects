# dependencies
library(tidyverse) 
library(rvest)
library(lubridate)
library(hrbrthemes)
library(scales)
library(progress)

# scrap cran --------------------------------------------------------------

# load the ACTIVE cran packages webpage (does not include archived CRAN packages)
## I just need this so I know how many packages there are (for the progress bar in the main scraper)
cran = read_html("https://cran.r-project.org/web/packages/available_packages_by_date.html") 

# convert to a data frame
cran_df = cran %>% # list of cran webpage
  html_nodes(., "table") %>% # extract the <table> elements
  .[[1]] %>%  # only need the first list element
  html_table() # convert it to a dataframe


# deep dive: scrape all package webpages ----------------------------------

# look at each package's webpage...
# ...and check if any of the imports or suggests or depends call a tidyverse package

# sanity checks
## coxrobust first listed package, has no Imports row
## DChaos example of package with multiple imports but none are tidy

is_it_tidy = function(pack_name){
  # scrape the package's webpage
  url = paste0("https://cran.r-project.org/web/packages/", pack_name, "/index.html")
  pack_page = read_html(url)
  df = pack_page %>% html_nodes(., "table") %>% # extract the <table></table> element
    .[[1]] %>% # first node is the table we want 
    html_table() %>% # parse to dataframe 
    rename(item = X1, details = X2) # nicer column names
  # check if any of the "Depends" or "Imports" or "Suggests" columns contain a tidyverse packages
  # vector of all tidyverse packages
  ## need to separate each with a "|" so I can use stringr::str_detect
  tidypacks = paste0(tidyverse_packages(include_self = TRUE), collapse = "|")
  # problem is that not all these columns appear in all packages 
  ## so need to handle if row doesn't exist
  check_tidy = function(.data, row_name){
    filter(.data, item == row_name) %>% 
      pull() %>% 
      str_detect(., tidypacks) %>% # true or false
      ifelse(length(.) == 0, FALSE, .) # handle possibility that this row doesn't exist
  }
  ## any Depends tidy?
  tidy_depends = df %>% check_tidy("Depends:")
  ## any Imports tidy?
  tidy_imports = df %>% check_tidy("Imports:")
  ## any Suggests tidy?
  tidy_suggests = df %>% check_tidy("Suggests:")
  # sum up the tidys
  total_tidy = sum(tidy_depends, tidy_imports, tidy_suggests)
  # boolean tidy
  tidy = ifelse(total_tidy > 1, 1, total_tidy)
  # count up all the imports
  ## it's a character string where each import is separated by a comma
  imports = df %>% filter(item == "Imports:") %>% pull(details)
  n_imports = ifelse(length(imports) > 0, str_count(imports, ",") + 1, 0)
  # date
  ## not always in the same row; need to find the row where the column "item" has the value "Published:"
  pub_date = df %>% filter(item == "Published:") %>% pull()
  # depends, imports, suggests
  ## not really using these now, could be interesting to map the package network
  depends = df %>% filter(item == "Depends:") %>% pull() %>% ifelse(length(.) == 0, NA, .)
  imports = df %>% filter(item == "Imports:") %>% pull() %>% ifelse(length(.) == 0, NA, .)
  suggests = df %>% filter(item == "Suggests:") %>% pull() %>% ifelse(length(.) == 0, NA, .)
  # put all the results in a tibble
  tb = tibble("package" = pack_name, "date" = pub_date, 
              "tidy_depends" = tidy_depends, 
              "tidy_imports" = tidy_imports,
              "tidy_suggests" = tidy_suggests,
              "total_tidy" = total_tidy, 
              "tidy" = tidy, 
              "n_imports" = n_imports, 
              "depends" = depends,
              "imports" = imports,
              "suggests" = suggests)
  # tick the progress bar
  pb$tick()
  # return the goods
  return(tb)
}

# create a progress bar
pb = progress_bar$new(total = nrow(cran_df), 
                      format = "[:bar] :current/:total (:percent) (:eta)")

# apply the scraper and return a data frame
## this takes some time to run (~2hrs as of 3/7/21)
tidy_packs_df = cran_df %>% 
  pull(Package) %>% # vector of all the package names
  map_df(is_it_tidy) # do it!


# plot the total verse ----------------------------------------------------

# cumulative sum and pct of tidyverse packs
tidy_packs_df = tidy_packs_df %>% 
  mutate(date = date(date)) %>% 
  arrange(date) %>% 
  mutate(totalverse = cumsum(tidy),
         totalverse_pct = totalverse / n())

# plot the totalverse over time
p_main = tidy_packs_df %>% 
  mutate(rn = row_number()) %>% 
  mutate(label_year = ifelse(rn == max(rn), totalverse_pct, NA)) %>%
  ggplot(aes(x = date, y = totalverse_pct)) + 
  geom_line(size = 1, color = "#9239F6") +
  geom_ribbon(aes(ymin=0,ymax=..y..), alpha = 0.1, fill = "#9239F6") +
  labs(title="Rise of the tidyverse", 
       subtitle = paste0("% of all CRAN packages (", format(nrow(tidy_packs_df), big.mark=","),") built with 1+ tidyverse dependency"), 
       y = "", x = "", 
       caption = paste0("CRAN scraped on ", today() %>% format('%b-%d-%y'), "\nDoes not include archived packages"),
       tag = "https://cran.r-project.org/web/packages/available_packages_by_date.html") +
  theme_ipsum_rc(grid = "Y", base_size = 18)+
  theme(plot.title = element_text(size=30), plot.subtitle = element_text(size=20)) + 
  theme(plot.tag = element_text(family = "Roboto Mono", size = 8, vjust = 2), 
        plot.tag.position = c(0.72, 0)) + 
  scale_y_percent(breaks = c(0, 0.1, 0.2)) + 
  geom_label(aes(label = scales::percent(label_year)), fill = "#9239F6", color = "white", size = 7, nudge_y = -0.012) +
  #ggrepel::geom_label_repel(aes(label = scales::percent(label_year)), fill = "#9239F6", color = "white", nudge_y = 50, size = 8) + 
  scale_x_date(breaks = c(date("2007-06-10"), date("2014-01-07"), date("2016-09-15"), date("2021-02-19")), date_labels = "%Y") + 
  annotate("text", x = date("2007-06-10"), y = 0.03, label = "ggplot2", color = "#617a89", family= "Roboto Mono", size = 5) +
  annotate("text", x = date("2014-01-07"), y = 0.05, label = "dplyr", color = "#617a89", family= "mono", size = 5) +
  annotate("text", x = date("2016-09-15"), y = 0.075, label = "tidyverse", color = "#617a89", family= "mono", size = 5) +
  geom_segment(x = date("2007-06-10"), xend = date("2007-06-10"), y = 0, yend = 0.018, color = "#617a89", arrow = arrow(length = unit(0.15, "cm"))) + 
  geom_segment(x = date("2014-01-07"), xend = date("2014-01-07"), y = 0, yend = 0.038, color = "#617a89", arrow = arrow(length = unit(0.15, "cm"))) + 
  geom_segment(x = date("2016-09-15"), xend = date("2016-09-15"), y = 0, yend = 0.062, color = "#617a89", arrow = arrow(length = unit(0.15, "cm")))

# inset: count tidy packages from 2018-2020
p_inset = tidy_packs_df %>% 
  mutate(year = year(date)) %>% 
  filter(year < 2021) %>% 
  group_by(year) %>% 
  summarise(pct_tidy = sum(tidy)/n()) %>% 
  filter(year >= 2017) %>% 
  mutate(label_year = ifelse(year == max(year), pct_tidy, NA)) %>% 
  ggplot(aes(x = as.factor(year), y = pct_tidy)) + 
  geom_segment(aes(xend=as.factor(year), yend=0), color = "#9239F6", size = 2, alpha = 0.5) + 
  coord_flip() + 
  geom_label(aes(label = scales::percent(label_year)), fill = "#9239F6", color = "white", nudge_y = -0.01) +
  #ggrepel::geom_label_repel(aes(label = scales::percent(label_year)), fill = "#9239F6", color = "white", nudge_y = 5, nudge_x = 10^6) + 
  theme_ipsum_rc(grid = "") + 
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 8)) + 
  theme(aspect.ratio = 1/5) + 
  labs(x = "", y = "How many packages each year are 'tidy'?\n2017 - 2020")

# combine
# 900 x 600
p_main + annotation_custom(ggplotGrob(p_inset), 
                                 xmin = date("2011-01-01"), xmax = date("2021-01-01"), 
                                 ymin = 0.025, ymax = 0.5)




 # E X P O N E N T I A L growth ------------------------------------------
# how many so far in 2021
tidy_packs_df %>% 
  mutate(year = year(date)) %>% 
  filter(year == 2021) %>% 
  summarise(sum(tidy))

# E X P O N E N T I A L growth
tidy_packs_df %>% 
  mutate(year = year(date)) %>% 
  filter(between(year, 2011, 2020)) %>% 
  group_by(year) %>% 
  summarise(n = sum(tidy)) %>% 
  lm(log(n) ~ year, data = .) %>% 
  ggeffects::ggpredict(model = ., terms = "year [2011:2030]") %>% 
  ggplot(aes(x, predicted)) + 
  geom_line(size = 1.5, color = "#9239F6") + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks() + 
  theme_ipsum_rc(grid = "XY", base_size = 18)+
  theme(plot.title = element_text(size=30), plot.subtitle = element_text(size=20)) + 
  labs(x = "", y = "", title = "The tidy swaRm", 
       subtitle = "log(tidyverse-dependent packages) ~ year", 
       caption = "[shrug emoji]") 


# complexity --------------------------------------------------------------
# complexity? 
tidy_packs_df %>% 
  mutate(year_aps = paste0("'", format(date, "%y"))) %>% 
  ggplot(aes(x= factor(year_aps), y = n_imports)) + 
  geom_boxplot(fill = "#9239F6", outlier.color = "#9239F6", outlier.size = 3, outlier.alpha = 0.25) + 
  theme_ipsum_rc(grid = "Y") + 
  theme_ipsum_rc(grid = "Y", base_size = 18)+
  theme(plot.title = element_text(size=30), plot.subtitle = element_text(size=20)) + 
  labs(x = "", y = "", title = "Complexity of new CRAN packages each year", 
       subtitle = "Total dependencies per new package",
       caption = paste0("CRAN scraped on ", today() %>% format('%b-%d-%y'), "\nDoes not include archived packages"),
     tag = "https://cran.r-project.org/web/packages/available_packages_by_date.html") + 
  theme(plot.tag = element_text(family = "Roboto Mono", size = 8, vjust = 2), 
        plot.tag.position = c(0.72, 0)) 

# npacks per year
tidy_packs_df %>% 
  mutate(year = year(date)) %>%
  filter(year < 2021) %>% 
  count(year) %>% 
  mutate(label_year = ifelse(year == max(year),  n, NA)) %>% 
  ggplot(aes(x=year, y=n)) + 
  geom_col(fill = "#9239F6") + 
  scale_x_continuous(breaks= c(2006, 2010, 2015, 2020)) +
  scale_y_comma(limits = c(0,6500)) + 
  theme_ipsum_rc(grid = "Y", base_size = 18)+
  theme(plot.title = element_text(size=30), plot.subtitle = element_text(size=20)) + 
  geom_label(aes(label = round(label_year,2)), color = "#9239F6", nudge_y = -0.25, size = 7) +
  labs(x = "", y = "", title = "New CRAN packages each year", subtitle = "",
       caption = paste0("CRAN scraped on ", today() %>% format('%b-%d-%y'), "\nDoes not include archived packages"),
       tag = "https://cran.r-project.org/web/packages/available_packages_by_date.html") + 
  theme(plot.tag = element_text(family = "Roboto Mono", size = 8, vjust = 2), 
        plot.tag.position = c(0.72, 0)) 
  
  
# SO ----------------------------------------------------------------------
# as of 2/28/21
r = 390039
dplyr = 23161
ggplot = 40038

(ggplot + dplyr)/r

# never used this
read_csv("so.csv") %>% 
  ggplot(aes(x = reorder(package, -tags), y = tags)) + 
  geom_col(fill = "#9239F6") + 
  labs(x = "", y = "", title = "Stack Overflow [tags]", subtitle = "tidyverse packages", caption = "Stack Overflow checked on 2/19/21") + 
  scale_y_continuous(labels = scales::comma) + 
  annotate(geom = "text", x = 5, y = 32000, label = "ggplot2 + dplyr = 16% of all R questions", color = "#909495", family = "Roboto Mono") + 
  theme(axis.text.x = element_text(family = "Roboto Mono", size = 10))


# count no. of functions in the tidyverse ---------------------------------

# get the attached packages
attached_packs = devtools::loaded_packages()[1]

# count the number of functions per tidyverse package
tidy_function_count = attached_packs %>% 
  mutate(tidy = package %in% tidyverse_packages(include_self = FALSE)) %>% 
  filter(tidy == TRUE) %>% 
  mutate(package_package = paste0('package:',package)) %>% 
  pull(package_package) %>%  
  map_df(function(x) tibble("pack" = x, 'count' = length(ls.str(x, mode='function')))) %>% 
  mutate(pack = str_remove(pack, "package:")) 

# plot
tidy_function_count %>% 
  ggplot(aes(x=reorder(pack, -count), y=count)) + 
  geom_col(fill = "#9239F6") + 
  theme_ipsum_rc(grid = "Y", base_size = 18)+
  theme(plot.title = element_text(size=30), plot.subtitle = element_text(size=20)) + 
  geom_label(aes(label = count), color = "#9239F6", nudge_y = -0.25, size = 7) + 
  labs(x = "", y = "", title = "Number of functions in the (core) tidyverse", subtitle = paste0("Total = ", scales::comma(sum(tidy_function_count$count)))) + 
  theme(axis.text.x = element_text(family = "Roboto Mono", size = 10), axis.text.y = element_blank()) 

# how many packages in base R?
sessionInfo()[6] %>% 
  unlist() %>% 
  map_dbl(function(x) length(ls.str(paste0('package:',x), mode='function'))) %>% 
  sum()

# save data ---------------------------------------------------------------
#write_csv(tidy_packs_df, "tidy_packs.csv")