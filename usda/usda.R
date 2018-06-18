library(dplyr)
library(ggplot2)
library(googleVis)
library(maps)
library(sjPlot)

setwd("~/Desktop/notebook/side_projects/usda/")
df <- read.csv("SFHG_Section_502_Guaranteed_By Congressional Dist 6.9.2016.csv")
nonstates <- c("PR", "VI", "WP")
df <- subset(df, !(State %in% nonstates))
mr_clean <- function(x) {
  x <- gsub(",", "", x)
  x <- as.numeric(x)
}
State <- df$State
df <- cbind(State, data.frame(apply(df[,-1], 2, mr_clean)))
names(df) <- gsub("[.]", "_", names(df))
df.sum <- df %>%  
  select(State, Active_Loans) %>% 
  group_by(State) %>% 
  summarise(total = sum(Active_Loans))
df.sum <- data.frame(df.sum)
names(df.sum) <- c("State", "Active Loans")
rownames(df.sum) <- rownames(USArrests)
USDA_Loans <- df.sum
GeoStates <- gvisGeoChart(USDA_Loans, "State", "Active Loans",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=900, height=600))
plot(GeoStates)


# do any of these vars matter?
df.naomit <- na.omit(df)
y <- as.double(df.naomit$Active_Loans)
x <- as.matrix(df.naomit[,-(1:3)])
m1 <- glmnet::cv.glmnet(x, y, family='poisson', alpha=1, nfolds=10)
plot(m1)
plot(coef(m1))

alabama <- subset(df, State == "AL")

# static map --------------------------------------------------------------
choro2 <- left_join(
  map_data("state"), 
  df.sum %>% 
    add_rownames("region") %>% 
    mutate(region=tolower(region))
)

ggplot(choro2, aes(long, lat, colour="white")) +
  geom_polygon(aes(group = group, fill = total)) + 
  coord_quickmap() + theme_bw() +
  scale_y_continuous(breaks=c()) + 
  scale_x_continuous(breaks=c()) + 
  theme(panel.border =  element_blank()) + labs(x = "", y = "")