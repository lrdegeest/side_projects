#=== sorting hat
sortingHat <- function(df) {
  # argument: data frame with names and past
  # copy the data frame
  df.copy <- df
  # initial sucess condition
  success <- FALSE
  # start loop
  while(!success) {
    # shuffle the deck of names 
    pick <- with(df.copy, sample(names, replace = FALSE))
    # check if any of the picks violate the constraints
    # if any pick does, put the names back in the hat and reshuffle
    if(TRUE %in% grepl("past",names(df.copy))){
      success <- with(df.copy, !any(names == pick | past == pick))
    } else {
      success <- with(df.copy, !any(names == pick))
    }
  }
  # attach the satisfactory vector of picks to the original data frame
  df.copy[,"pick"] <- pick
  # return the updated data frame
  # note: comment out this line if you do not want to see the picks
  sanity_check <- function(df){
    sanity_check <- c()
    for(i in df[,1]){
      for(j in df[,2]){
        if(i != j){
          test_same <- identical(i,j)
          sanity_check <- append(sanity_check, test_same)
        }
      }
    }
    if(!(T %in% sanity_check)){
      return(df)
    }
  }
  sanity_check(df.copy)
}

#=== make assignment cards
# each person will have a .pdf with their name and their assignment. 
littleHelpers <- function(giver, receiver){
  pdf(paste0(giver,"_CARD",".pdf"), width=9, height=6, family = "mono")
  plot(0,type='n', axes = F, ann = F) 
  box(lty = '1373', col = 'red')
  text(1, 0.5, paste0("Dearest"," ", giver, ","), family = "mono", cex = 1)
  text(1,0, "The pagen gods of secret santa have foretold from \non high you shall buy a gift, in utmost secrecey, for...", cex = 1)
  text(1,-0.5, receiver, family = "mono", font = 2, cex = 2)
  dev.off()
}
