# Reds WAR positions
# Need to figure out what the primary position was during inductees' Reds tenure so I can create position distributions


library(tidyverse)
library(openWARData)
library(Lahman)


redsWarJaws <- read_rds("data/05 - indRedsWARandJAWS.rds")
indWarR <- read_rds("data/03 - inducteeWARreds.rds")


# Making id, years lists for map functions
idList2 <- list(indWarR$playerId)
idList2 <- idList2[[1]]
yearList2 <- list(indWarR$yearId)
yearList2 <- yearList2[[1]]

# Using lists to filter Fielding data set for inductees and seasons as a Red
aFilter <- function(x,y) filter(Fielding, playerID == x & yearID == y)
# Fielding dataset has some weird designations for 1800's Reds teams: CN1 and CN2.
posDat <- map2_dfr(idList2, yearList2, aFilter) %>% 
      filter(teamID == "CIN" | teamID == "CN1" | teamID == "CN2")

# Getting position with most games as a Red
posDat2 <- posDat %>%
      select(playerID, POS, G) %>% 
      group_by(playerID, POS) %>% 
      summarise(sumG = sum(G)) %>% 
      filter(sumG == max(sumG)) %>% 
      ungroup() %>% 
      select(playerID, POS)

# Jim O'Toole's ID isn't showing up in the Fielding data set but he was a pitcher his whole career. Double-checked. Adding him to the df.
# Renaming playerID to join with another df
setdiff(redsWarJaws$playerId, posDat2$playerID)
posDat2 <- posDat2 %>% 
      add_row(playerID = "o'tooji01", POS = "P") %>% 
      rename(playerId = playerID)

# Removing the old POS column which I evidently didn't need to go to the trouble to add in the first place. Joining both dataframes to add new POS. Ugh could've just done an add_column but I'm tired. 
redsWarJaws <- redsWarJaws %>% select(-POS) %>% 
      inner_join(posDat2, by = "playerId")%>%
      select(playerId, fangraphs_id, name_whole, totalYrs, redsWAR, redsPeakWAR, redsWAR4, redsJAWS, POS, name_first, name_last) %>% 
      rename(tenure = totalYrs)

write_rds(redsWarJaws, "data/05 06 - indRedsWARandJAWS.rds")


# Nominees=======================================

nomWarR <- read_rds("data/03 - nomineeWARreds.rds")
nRedsWarJaws <- read_rds("data/05 - nomRedsWARandJAWS.rds")


# Making lists for map functions
nIdList <- list(nomWarR$playerId)
nIdList <- nIdList[[1]]
nYearList <- list(nomWarR$yearId)
nYearList <- nYearList[[1]]

# Using lists to filter Fielding data set
aFilter <- function(x,y) filter(Fielding, playerID == x & yearID == y)
nPosDat <- map2_dfr(nIdList, nYearList, aFilter)

# Getting the position where each player played the most games for each of the 4 yrs
nPosDat2 <- nPosDat %>%
      select(playerID, POS, G) %>% 
      group_by(playerID, POS) %>% 
      summarise(sumG = sum(G)) %>% 
      filter(sumG == max(sumG)) %>% 
      ungroup() %>% 
      select(playerID, POS) %>% 
      rename(playerId = playerID)

# Removing the old POS column Joining both dataframes to add new POS.
nRedsWarJaws <- nRedsWarJaws %>% select(-POS) %>% 
      inner_join(nPosDat2, by = "playerId") %>% 
      select(playerId, fangraphs_id, name_whole, tenure, redsWAR, redsPeakWAR, redsWAR4, redsJAWS, POS, name_first, name_last)


write_rds(nRedsWarJaws, "data/05 06 - nomRedsWARandJAWS.rds")

