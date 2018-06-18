library(ggplot2)
library(dplyr)
theme_set(
  theme_minimal() + 
    theme(legend.title=element_blank()) + 
    theme(legend.justification=c(0,0), legend.position=c(0.05,0.75)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(text=element_text(family="mono",size=16,face="bold"))
  )

setwd("~/Desktop/notebook/teaching/wit/experiments/asset_market/")
df <- read.csv("asset_market_s18.csv", na.strings = c("*", "*.**"))
df$Section <- gsub(1, "Market 1", df$Section)
df$Section <- gsub(2, "Market 2", df$Section)

# present value of asset
pv = 7

# price diff: percent above pv --------------------------------------------
df$price_diff <- with(df, ((Price - pv)/pv)*100)
ggplot(df, aes(Round, price_diff, group=Section, color=factor(Section))) + 
  geom_hline(yintercept = 0, linetype="dotted") + geom_point(size=3) + geom_line(size=1) 
# animate
doPlot = function(data,i){
  #subset the data
  sub = data[data$Round <= i,]
  #plot the subset
  g = ggplot(sub, aes(Round, price_diff, group=Section, color=factor(Section))) + 
    geom_hline(yintercept = 0, linetype="dotted") + geom_point(size=3) + geom_line(size=1) + 
    scale_x_continuous(limits=c(0,20)) + scale_y_continuous(limits=c(min(na.omit(df$price_diff)), max(na.omit(df$price_diff)))) + 
    labs(x="Trading period", y="Percent deviation from PV ($7)")  
  #print the plot
  print(g)
}
animation::saveGIF(for(i in 1:20){doPlot(df,i)},interval = 0.2, ani.width = 500, ani.height = 400, movie.name = "bubbles.gif")


# payoff distribution  ----------------------------------------------------
x_min <- min(na.omit(df$Earnings))
x_max <- max(na.omit(df$Earnings))
y_min <- min(unlist(density(na.omit(df$Earnings))[2]))
y_max <- max(unlist(density(na.omit(df$Earnings))[2]))
doPlotPi = function(data,i){
  sub = data[data$Round <= i,]
  g = ggplot(sub, aes(Earnings, group=Section, color=Section, fill=Section)) + 
    geom_density(aes(y=..scaled..), alpha=0.75) + 
    scale_x_continuous(limits=c(x_min,x_max)) + 
    #scale_y_continuous(limits=c(y_min, y_max)) + 
    labs(x="Payoffs", y="Density (scaled)") + 
    annotate("label", x = -85, y = 0.65, label = paste0("Trading period ",  i), fill="gray")   
  print(g)
}
animation::saveGIF(for(i in 1:20){doPlotPi(df,i)},interval = 0.2, ani.width = 500, ani.height = 400, movie.name = "payoffs.gif")

# sanity check
ggplot(df, aes(Earnings, group=Section, color=Section, fill=Section)) + 
  geom_density(aes(y=..scaled..), alpha=0.75) + scale_x_continuous(limits=c(x_min,x_max)) + 
  annotate("label", x = -85, y = 0.65, label = paste0("Trading period ", "2"), fill="gray") 



# bids and offers ---------------------------------------------------------
# bid price
ggplot(df, aes(Round, Bid_P, group=Section, color=factor(Section), fill=factor(Section))) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", size=3)

# offer price
ggplot(df, aes(Round, Offer_P, group=Section, color=factor(Section), fill=factor(Section))) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", size=3)

# bid quantity
ggplot(df, aes(Round, Buy_Q, group=Section, color=factor(Section))) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
  stat_summary(fun.y="mean", geom="point", size=3) + 
  facet_wrap(~Section)

# offer quantity
doPlot = function(data,i){
  #subset the data
  sub = data[data$Round <= i,]
  #plot the subset
  g = ggplot(sub, aes(Round, Offer_Q, group=Section, color=factor(Section))) + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width=0) +
    stat_summary(fun.y="mean", geom="point", size=3) + 
    scale_x_continuous(limits=c(0,20)) + scale_y_continuous(limits=c(0,5)) +
    labs(x="Trading Period", y="Shares Offered") + 
    facet_wrap(~Section)
  #print the plot
  print(g)
}
animation::saveGIF(for(i in 1:20){doPlot(df,i)},interval = 0.2, ani.width = 500, ani.height = 400, movie.name = "sales.gif")


# cumulative earnings & top 3 earners in each market --------------------
df <- df %>% 
  group_by(Section, ID) %>% 
  mutate(cum_earnings = cumsum(Earnings)) %>% 
  mutate(total_cum_earnings = max(cum_earnings))
df <- df %>% 
  group_by(Section, Round) %>% 
  mutate(breadwinner = ifelse(total_cum_earnings %in% tail(sort(total_cum_earnings), 3), 1, 0))
xmin = min(df$Round)
xmax = max(df$Round)
ymin = min(na.omit(df$cum_earnings))
ymax = max(na.omit(df$total_cum_earnings))
doPlotCumPi = function(data,i){
  sub = data[data$Round <= i,]
  grouping_var <- if(i>1) sub$ID else 1
  g <- ggplot(sub) + 
    geom_line(aes(Round, cum_earnings, group=grouping_var, color=factor(breadwinner))) + 
    geom_hline(yintercept = 0, linetype="dotted") +
    scale_color_manual(values = c("gray", "red")) +
    scale_x_continuous(limits=c(xmin,xmax)) + 
    scale_y_continuous(limits=c(ymin, ymax)) + 
    facet_wrap(~Section) + 
    theme(legend.position="none") +
    labs(x="Trading Period", y="Cumulative earnings from assets") 
  print(g)
}
animation::saveGIF(for(i in 1:20){doPlotCumPi(df,i)},interval = 0.2, ani.width = 500, ani.height = 400, movie.name = "cum_payoffs.gif")
