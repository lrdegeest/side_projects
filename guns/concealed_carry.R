library(ggplot2)
df <- read.csv("concealed_carry.csv")
df$No <- df$May.issue + df$No.issue
df$Yes <- df$Unrestricted + df$Shall.issue
df.percent <- data.frame("Year" = df[,1], apply(df[,2:ncol(df)], 2, function(x) 100*(x/50)))
ggplot(df.percent, aes(Year, Yes)) + 
  geom_line() + scale_y_continuous(limits=c(0,100))
df.percent.long <- reshape2::melt(df.percent, id="Year")
ggplot(df.percent.long, aes(Year, value, color=variable)) + 
  geom_line() + scale_y_continuous(limits=c(0,100))