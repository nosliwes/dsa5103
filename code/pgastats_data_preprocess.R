
library(tidyverse)
library(stringr)
library(priceR) # For inflation adjustment

# set working directory to data location
# setwd("/data")

filenames = list.files() #Can't have any files in your working directory other than the 6
# .csv files


golf_data = data.frame("PLAYER.NAME" = NA) # create a dataframe with a PLAYER.NAME column

for (i in seq(from = 1, to = 6)) {

  mydata = read.csv(filenames[i])
  
  if (i == 1) { 
    #Driving accuracy
    mydata = mydata[,c(3,5,8)]
    colnames(mydata) = c("PLAYER.NAME", "PCT.FAIRWAYS.HIT", "YEAR")
  }
  
  else if (i == 2) { 
    #Driving distance
    mydata = mydata[,c(3,5,8)]
    colnames(mydata) = c("PLAYER.NAME", "AVG.DRIVE.DISTANCE", "YEAR")
  }
  else if (i == 3) { #Greens in Regulation
    # The percent of time a player was able to hit the green in regulation 
    # (greens hit in regulation/holes played). 
    # Note: A green is considered hit in regulation if any portion of the ball is 
    # touching the putting surface after the GIR stroke has been taken. 
    # (The GIR stroke is determined by subtracting 2 from par (1st stroke on a par 3, 
    # 2nd on a par 4, 3rd on a par 5)
    
    # Note to self, it is not clear to me what "RELATIVE/PAR" means for this stat.
    mydata = mydata[,c(3,5,8,9)]
    colnames(mydata) = c("PLAYER.NAME", "PCT.GREENS.IN.REGULATION", "GREENS.HIT.RELATIVE.TO.PAR", "YEAR")
  }
  else if (i == 4) { #Money
    mydata = mydata[,c(3,4,5,7)]
    colnames(mydata) = c("PLAYER.NAME", "EVENTS", "MONEY", "YEAR")
  }
  else if (i == 5) { # One Putts Percentage 
    # A one-putt occurs when a player putts the ball into the hole with 1 shot after landing on
    # the green (regardless of total score on the hole)
    mydata = mydata[,c(3,5,8)]    
    colnames(mydata) = c("PLAYER.NAME", "PCT.ONE.PUTTS", "YEAR")
  }
  else if (i == 6) { # Sand Saves
    # A sand save occurs any time a player hits the sand, then 
    # gets the ball in the hole after 1 or 2 more shots.
    # O/U stands for Over/Under, which is = a sand save.
    # What Total O/U Par means I don't know.
    mydata = mydata[,c(3,5,8,9)]
    colnames(mydata) = c("PLAYER.NAME", "PCT.SAND.SAVES", "TOTAL.OU.PAR","YEAR")
  }
  
  golf_data = merge(golf_data, mydata, all = T)
}


# Do a bit of data clean up.

golf_data = golf_data[golf_data$PLAYER.NAME != "",] #Removes the row with no data.

golf_data$MONEY = str_remove_all(golf_data$MONEY, "\\$") #Helps fix the money column to be numeric
golf_data$MONEY = str_remove_all(golf_data$MONEY, ",")

golf_data$TOTAL.OU.PAR = str_replace_all(golf_data$TOTAL.OU.PAR, "E", "0") #change E to 0 so it can be numeric


golf_data$YEAR = as.factor(golf_data$YEAR)
golf_data$MONEY = as.numeric(golf_data$MONEY)
golf_data$TOTAL.OU.PAR = as.numeric(golf_data$TOTAL.OU.PAR)


# Adjust money column for inflation based on the year:
golf_data$ADJ_MONEY_2019 = adjust_for_inflation(golf_data$MONEY, golf_data$YEAR, "US", to_date = 2019)

# Still not done: Adjusting the Player Names to be only roman characters, there are some
# messed up names in there still. But that's alright for now, since we won't use their names anyway.

# Write to a table:

write.table(golf_data, file = "all_PGA_data.tsv", sep = "\t", quote = F, row.names = F)



