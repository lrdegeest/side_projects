# author: @lrdegeest
# scrape NFL contracts

library(XML)
library(stringr)
library(ggplot2)
theme_set(theme_classic())
library(scales)
library(grid)
library(gridExtra)
library(scales)
library(dplyr)

# set up ------------------------------------------------------------------

load_url <- function(url) {
  "
  Function to load and clean url data from www.spotrac.com (also works for overthecap.com)
  Only goes so far; many column names for each league vary
  
  input: url
  output: data frame
  "
  df <- readHTMLTable(url, header = T, trim = T) # load data
  df <- as.data.frame(df) # convert to data frame
  colnames(df)<-gsub(pattern = 'NULL.', "", x = colnames(df), ignore.case = T) # remove prefix from column names
  df$Contract_years <- stringr::str_extract_all(df$Player, '(?<=\\()[0-9-]+(?=\\))') # create variable extracting contract years from player column
  df2 <- stringr::str_split_fixed(df$Contract_years, "-", 2) # create data frame that splits the contract years (xxxx-yyyy) into two separate columns (https://stackoverflow.com/questions/31292853/strsplit-by-parentheses)
  colnames(df2) <- c("Contract.Start", "Contract.End") # rename those columns
  df <- cbind(df, df2) # bind the data frames
  df$Contract_years <- NULL # don't need this anymore
  df$Player <- stringr::str_replace(df$Player, " \\(.*\\)", "") # remove the parentheses in player column
  df <- data.frame(apply(df, 2, function(x) gsub('\\$|,|-|%', '', x))) # remove special characters throughout data
  return(df)
}

# nfl figures: money guaranteed -----------------------------------------------
url <- "http://www.spotrac.com/nfl/contracts/sort-guaranteed-pct/" # set url
df.nfl <- load_url(url)
# converting columns from factor to numeric
colnames(df.nfl)[grep("..G.teed", colnames(df.nfl))] <- "Guaranteed"
cols.to.convert <- c("Dollars", "Average", "Guaranteed.at.Signing", "Practical.Guarantees", "Guaranteed")
df.nfl[cols.to.convert] <- sapply(df.nfl[cols.to.convert],function(x) as.numeric(levels(x))[x]) # key read: https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information

# Money guaranteed:
## by position
g1 <- ggplot(df.nfl, aes(reorder(Pos, -Guaranteed), Guaranteed)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", col="tomato2", size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  ylab("% Contract Guaranteed (Avg.)") + xlab("Player Position")
## by contract length
g2 <- ggplot(df.nfl, aes(reorder(Yrs, -Guaranteed), Guaranteed)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", col="tomato2", size=3) +
  scale_y_continuous(limits = c(0,100)) +
  ylab("% Contract Guaranteed (Avg.)") + xlab("Contract Length")
## by age
g3 <- ggplot(subset(df.nfl, Age != "0"), aes(reorder(Age, -Guaranteed), Guaranteed)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", col="tomato2", size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  ylab("% Contract Guaranteed (Avg.)") + xlab("Player Age")
## by year the contract started
g4 <- ggplot(df.nfl, aes(reorder(Contract.Start, -Guaranteed), Guaranteed)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", col="tomato2", size=3) +
  scale_y_continuous(limits = c(0,100)) +
  ylab("% Contract Guaranteed (Avg.)") + xlab("Contract Starting Year")
## player specifics:
### contract length 
g5 <- ggplot(df.nfl, aes(reorder(Pos, -as.numeric(Yrs)), as.numeric(Yrs))) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", col="tomato2", size=3) +
  scale_y_continuous(limits = c(0,100)) +
  ylab("Contract length (Avg.)") + xlab("Player position")

## view
title = textGrob("Guaranteed contracts in the NFL",gp=gpar(fontsize=20,font=2))
grid.arrange(g1, g2, g3, g4, ncol = 2, top=title)

# guaranteed salary by team
## toss out "signed with" instances for now
df.nfl$Team <- replace(df.nfl$Team, grep("Signed", df.nfl$Team), NA)
g.nfl <- ggplot(na.omit(df.nfl), aes(reorder(Team, -Guaranteed), Guaranteed)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", col="tomato2", size=3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(0,100)) + 
  ylab("% Contract Guaranteed (Avg.)") + xlab("") + ggtitle("NFL (Sportac.com)")


# nfl figures: raw contract value ---------------------------------------------
# labor supply
g6 <- ggplot(df.nfl, aes(reorder(Pos, -Pos, function(x)-length(x)))) + 
  geom_bar() + 
  ylab("Number of unique contracts") + xlab("Player position") + ggtitle("Labor supply in the NFL (active contracts)")

# by position
g7 <- ggplot(df.nfl, aes(reorder(Pos, -Dollars), Dollars)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", col="tomato2", size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels=comma) + 
  ylab("Contract Value (Avg. USD)") + xlab("Player Position")

high_rollers <- subset(df.nfl, Pos %in% c("LT", "QB"))
g8 <- ggplot(high_rollers, aes(Dollars, fill=Pos)) + 
  geom_density(alpha=0.75) + 
  xlab("Contract value (USD)") + ylab("Frequency") + 
  scale_fill_manual(values=c("orange","blue"),name="Position") + 
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + 
  scale_x_continuous(labels = comma)

title = textGrob("NFL contract values by position",gp=gpar(fontsize=20,font=2))
grid.arrange(g7,g8,ncol=2, top=title)

# overall contract value
ggplot(df.nfl, aes(Dollars)) + 
  geom_density(fill="tomato2", color="tomato2") + 
  xlab("Contract value (USD)") + ylab("Frequency") + ggtitle("NFL contract values") + 
  scale_x_continuous(labels = comma) 

# nba vs nfl --------------------------------------------------------------
# load nba contracts
url <- "http://www.spotrac.com/nba/contracts/" # set url
df.nba <- load_url(url)
# converting columns from factor to numeric
colnames(df.nba)[grep("..GTD", colnames(df.nba))] <- "Guaranteed.percent"
cols.to.convert <- c("Dollars", "Avg..Salary", "Guaranteed", "Guaranteed.percent")
df.nba[cols.to.convert] <- sapply(df.nba[cols.to.convert], function(x) as.numeric(levels(x))[x])
# toss out "signed with" instances for now
df.nba$Team <- replace(df.nba$Team, grep("Signed", df.nba$Team), NA)
g.nba <- ggplot(na.omit(df.nba), aes(reorder(Team, -Guaranteed.percent), Guaranteed.percent)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", col="tomato2", size=3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(0,100)) + 
  ylab("% Contract Guaranteed (Avg.)") + xlab("Team") + ggtitle("NBA")

# number of contracts per team: nba
g.nba.contracts <- na.omit(df.nba) %>% 
  group_by(Team) %>% 
  count() %>% 
  ggplot(., aes(reorder(Team,-n),n)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Number of active contracts") + xlab("Team") + ggtitle ("NBA")

# number of contracts per team: nba
g.nfl.contracts <- na.omit(df.nfl) %>% 
  group_by(Team) %>% 
  count() %>% 
  ggplot(., aes(reorder(Team,-n),n)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Number of active contracts") + xlab("Team") + ggtitle("NFL")

# nba vs nfl
title = textGrob("Guaranteed contracts: NFL vs NBA",gp=gpar(fontsize=20,font=2))
grid.arrange(g.nfl, g.nba, ncol=2, top=title)


# save data ---------------------------------------------------------------
# list of the data frames (should have done this in the beginning.....)
nfl_nba_data <- list(df.nfl, df.nba)
save(nfl_nba_data, file="nfl_nba.Rdata")


# follow up: other contract data ------------------------------------------
url <- "https://overthecap.com/contracts/"
df.nfl2 <- load_url(Rcurl::getURL(url))
cols.to.convert <- c("Total.Value", "Avg..Year", "Avg..Guarantee.Year", "..Guaranteed")
df.nfl2[cols.to.convert] <- sapply(df.nfl2[cols.to.convert],function(x) as.numeric(levels(x))[x]) 

g.nfl2 <- ggplot(na.omit(df.nfl2), aes(reorder(Team, -..Guaranteed), ..Guaranteed)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", col="cornflowerblue", size=3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(0,100)) + 
  ylab("% Contract Guaranteed (Avg.)") + xlab("") +  ggtitle("NFL (Overthecap.com)")

g.nfl.dist <- ggplot(df.nfl, aes(Guaranteed)) +
  geom_density(fill="tomato2",color="tomato2") + coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("") + ylab("Frequency") + ggtitle(" ") + theme_void()

g.nfl2.dist <- ggplot(df.nfl2, aes(..Guaranteed)) +
  geom_density(fill="cornflowerblue",color="cornflowerblue") + coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("") + ylab("Frequency") + ggtitle(" ") + theme_void()

cowplot::plot_grid(g.nfl, g.nfl.dist, g.nfl2, g.nfl2.dist, ncol=2, nrow=2, align = "h", rel_widths=c(4, 1))
