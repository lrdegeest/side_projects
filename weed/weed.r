setwd("/Users/LawrenceDeGeest/Desktop/notebook/coding/weed") #setwd
library(data.table) 
library(ggplot2)

# set up
data <- read.csv("weed.csv") #load data
dt <- data.table(data) #convert to data table
setkey(dt, State, date)
dt <- na.omit(dt)
dt <- subset(dt, select = c("State", "date", "HighQ", "MedQ", "LowQ"))
# new england
new_eng <- c("Massachusetts", "Connecticut", "Rhode Island", "New Hampshire", "Vermont", "Maine")

#density plot new england
dt.density <- subset(dt, select = c("State", "HighQ", "MedQ", "LowQ"))
dt.density_long <- melt(dt.density, id = "State")
dt.density_long_neweng <- subset(dt.density_long, dt.density_long$State %in% new_eng)
ggplot(dt.density_long_neweng, aes(value, fill = variable)) + 
  geom_density(alpha = 0.5) + facet_wrap(~State)

# time series new england 
dt.new_eng <- subset(dt, dt$State %in% new_eng); dt.new_eng$State <- droplevels(dt.new_eng$State)
dt.new_eng_long <- melt(dt.new_eng, id = c("date", "State"))
dt.new_eng_long_highq <- subset(dt.new_eng_long, variable == "HighQ")
ggplot(dt.new_eng_long, aes(date, value, group = variable, color = variable)) + 
  geom_line () + 
  facet_wrap(~State) + 
  ylab("Daily price ($/oz)") + 
  theme(axis.text.x=element_blank()) +
  xlab("Time (12/2013 to 08/2014)")

# set up for export. this is old.
dt.lowq = subset(dt, select = c("State", "date", "LowQ"))
x = reshape(dt.lowq, timevar = "State", idvar = c("date"), direction = "wide")  
colnames(x) = gsub("LowQ.", "", colnames(x))

