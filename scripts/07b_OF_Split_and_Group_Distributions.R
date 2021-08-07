# More distributions
# sections: Inductees, Other Groups, Nominees

library(tidyverse)
library(Lahman)


iRedsWandJ <- read_rds("data/05 06 - indRedsWARandJAWS.rds")
iRedsYrs <- read_rds("data/03 - inducteeWARreds.rds")
# Seeing how many OF'ers I have.
table(iRedsWandJ$POS)

# Subsetting df with players with position = outfielder
of <- iRedsWandJ %>% 
      select(playerId, POS) %>% 
      filter(POS == "OF")
idList <- list(of$playerId)
idList <- idList[[1]]
yrFilter <- function(x, y) {
      filter(iRedsYrs, playerId == x)
}
ofYears <- map_dfr(idList, yrFilter)


# Making lists to feed to map
idList2 <- list(ofYears$playerId)
idList2 <- idList2[[1]]
yrsList <- list(ofYears$yearId)
yrsList <- yrsList[[1]]

# Getting number of games played at each OF position for each season

ofFilter <- function(x,y) {
      filter(Appearances, playerID == x & yearID == y)
}
ofSplit <- map2_dfr(idList2, yrsList, ofFilter)

ofSplit_gathered <- gather(ofSplit, 'G_lf', 'G_cf', 'G_rf', key = "of_pos", value = "G")

# For each player, summing number of games played at each position for entire Reds tenure and getting the most played position.
ofSplitSum <- ofSplit_gathered %>% 
      select(playerID, of_pos, G) %>% 
      group_by(playerID, of_pos) %>% 
      summarize(sumG = sum(G)) %>% 
      filter(sumG == max(sumG)) %>% 
      ungroup()

# renaming columns and values, so I can eventually join to another df
# Yes, I know I should've just changed the column names before I gathered earlier.
ofPos <- ofSplitSum %>% 
      select(playerID, of_pos) %>% 
      mutate(POS = plyr::mapvalues(of_pos, from = c("G_lf", "G_cf", "G_rf"), to = c("LF", "CF", "RF"))) %>% 
      rename(playerId = playerID) %>% 
      select(playerId, POS)
table(ofPos$POS)

# Replacing the POS column values that used to be "OF" with our new values.
ofWandJ <- iRedsWandJ %>% 
      filter(POS == "OF") %>% 
      select(-POS) %>% 
      inner_join(ofPos, by = "playerId")
iRedsWandJ <- iRedsWandJ %>% 
      filter(POS != "OF") %>%
      bind_rows(ofWandJ)

write_rds(iRedsWandJ, "data/05 06 07b - indRedsWARandJAWS.rds")



# Other groups =============================
# A lot of repetition here so it seems like there should be a way to create a couple functions to make this process more efficient. But with the varying number of inputs, a solution didn't readily come to mind --> copy&paste.

cornerIFdf <- iRedsWandJ %>% 
      filter(POS == "1B" | POS == "3B") %>%
      rename(aPOS = POS) %>% 
      mutate(POS = plyr::mapvalues(aPOS, from = c("1B", "3B"), to = c("CI", "CI"))) %>% 
      select(-aPOS)

middleIFdf <- iRedsWandJ %>% 
      filter(POS == "2B" | POS == "SS") %>%
      rename(aPOS = POS) %>% 
      mutate(POS = plyr::mapvalues(aPOS, from = c("2B", "SS"), to = c("MI", "MI"))) %>% 
      select(-aPOS)

outFielddf <- iRedsWandJ %>% 
      filter(POS == "LF" | POS == "CF" | POS == "RF") %>%
      rename(aPOS = POS) %>% 
      mutate(POS = plyr::mapvalues(aPOS, from = c("LF", "CF", "RF"), to = c("OF", "OF", "OF"))) %>% 
      select(-aPOS)

cornersdf <- iRedsWandJ %>% 
      filter(POS == "1B" | POS == "3B" | POS == "LF" | POS == "RF") %>%
      rename(aPOS = POS) %>% 
      mutate(POS = plyr::mapvalues(aPOS, from = c("1B", "LF", "RF", "3B"), to = c("CO", "CO", "CO", "CO"))) %>% 
      select(-aPOS)

middledf <- iRedsWandJ %>% 
      filter(POS == "2B" | POS == "SS" | POS == "C" | POS == "CF") %>%
      rename(aPOS = POS) %>% 
      mutate(POS = plyr::mapvalues(aPOS, from = c("2B", "SS", "C", "CF"), to = c("Md", "Md", "Md", "Md"))) %>% 
      select(-aPOS)

other_Groups <- cornerIFdf %>% 
      bind_rows(middleIFdf, outFielddf, cornersdf, middledf)

write_rds(other_Groups, "data/07b - otherGroupDistributions.rds")

# Summarized Values ============
# Will need these for a comparison table

iRedsWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds")

cornerIF <- iRedsWandJ %>% 
      filter(POS == "1B" | POS == "3B") %>% 
      summarize(number = n(), redsWAR = round(mean(redsWAR), 2), redsWAR4 = round(mean(redsWAR4), 2), redsJAWS = round(mean(redsJAWS), 2)) %>% 
      add_column(Group = "CI")

middleIF <- iRedsWandJ %>% 
      filter(POS == "2B" | POS == "SS") %>%
      summarize(number = n(), redsWAR = round(mean(redsWAR), 2), redsWAR4 = round(mean(redsWAR4), 2), redsJAWS = round(mean(redsJAWS), 2)) %>% 
      add_column(Group = "MI")

outField <- iRedsWandJ %>% 
      filter(POS == "LF" | POS == "CF" | POS == "RF") %>%
      summarize(number = n(), redsWAR = round(mean(redsWAR), 2), redsWAR4 = round(mean(redsWAR4), 2), redsJAWS = round(mean(redsJAWS), 2)) %>% 
      add_column(Group = "OF")

corners <- iRedsWandJ %>% 
      filter(POS == "1B" | POS == "3B" | POS == "LF" | POS == "RF") %>%
      summarize(number = n(), redsWAR = round(mean(redsWAR), 2), redsWAR4 = round(mean(redsWAR4), 2), redsJAWS = round(mean(redsJAWS), 2)) %>% 
      add_column(Group = "CO")

middle <- iRedsWandJ %>% 
      filter(POS == "2B" | POS == "SS" | POS == "C" | POS == "CF") %>%
      summarize(number = n(), redsWAR = round(mean(redsWAR), 2), redsWAR4 = round(mean(redsWAR4), 2), redsJAWS = round(mean(redsJAWS), 2)) %>% 
      add_column(Group = "Md")

group_Summary <- cornerIF %>% 
      bind_rows(middleIF, outField, corners, middle)

write_rds(group_Summary, "data/07b - otherGroupSummary.rds")



# nominees ===================================


# Copied errything from the inductee code section and either replaced the "i" for inductee with an "n" or added an "n".

nRedsWandJ <- read_rds("data/05 06 - nomRedsWARandJAWS.rds")
nRedsYrs <- read_rds("data/03 - nomineeWARreds.rds")

# Subsetting df with players with position = outfielder
nof <- nRedsWandJ %>% 
      select(playerId, POS) %>% 
      filter(POS == "OF")
nidList <- list(nof$playerId)
nidList <- nidList[[1]]
nyrFilter <- function(x, y) {
      filter(nRedsYrs, playerId == x)
}
nofYears <- map_dfr(nidList, nyrFilter)


# Making lists to feed to map
nidList2 <- list(nofYears$playerId)
nidList2 <- nidList2[[1]]
nyrsList <- list(nofYears$yearId)
nyrsList <- nyrsList[[1]]

# Getting number of games played at each OF position for each season

nofFilter <- function(x,y) {
      filter(Appearances, playerID == x & yearID == y)
}
nofSplit <- map2_dfr(nidList2, nyrsList, nofFilter)
nofSplit_gathered <- gather(nofSplit, 'G_lf', 'G_cf', 'G_rf', key = "of_pos", value = "G")

# For each player, summing number of games played at each position for entire Reds tenure and getting the most played position.
nofSplitSum <- nofSplit_gathered %>% 
      select(playerID, of_pos, G) %>% 
      group_by(playerID, of_pos) %>% 
      summarize(sumG = sum(G)) %>% 
      filter(sumG == max(sumG)) %>% 
      ungroup()

# renaming columns and values, so I can eventually join to another df
# Yes, I know I should've just changed the column names before I gathered earlier.
nofPos <- nofSplitSum %>% 
      select(playerID, of_pos) %>% 
      mutate(POS = plyr::mapvalues(of_pos, from = c("G_lf", "G_cf", "G_rf"), to = c("LF", "CF", "RF"))) %>% 
      rename(playerId = playerID) %>% 
      select(playerId, POS)
table(nofPos$POS)

# Replacing the POS column values that used to be "OF" with our new values.
nofWandJ <- nRedsWandJ %>% 
      filter(POS == "OF") %>% 
      select(-POS) %>% 
      inner_join(nofPos, by = "playerId")
nRedsWandJ <- nRedsWandJ %>% 
      filter(POS != "OF") %>%
      bind_rows(nofWandJ)

write_rds(nRedsWandJ, "data/05 06 07b - nomRedsWARandJAWS.rds")

