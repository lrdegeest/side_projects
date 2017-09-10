# set yr directory
setwd("[your directory here]")

# import `sortingHat()` and `littleHelpers()`
source("sortingHat.R")

# upload yr friends
df <- read.csv("amigos.csv")

# run the sorting hat and supress the output by assigning results to an object
df_picks <- sortingHat(df)

# put the little helpers to work
## cards will be saved in your directory
mapply(littleHelpers, df_picks$names, df_picks$pick)
