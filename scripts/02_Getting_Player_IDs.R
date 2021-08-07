# Getting BBRef IDs for members and nominees
# Need to clean up this script a bit


library(tidyverse)
library(openWARData)



# loading dataset with appropriate IDs. idTT is part of the openWARData pkg.
str(idTT)
head(idTT)

# Selecting ID and name columns; combining first, last names; renaming column
idTTa <- idTT %>% select(key_bbref, key_fangraphs, name_last, name_first) %>% 
  mutate(name_whole= paste(name_first, name_last)) %>% 
  rename(playerId = key_bbref, fangraphs_id = key_fangraphs)

# Taking column from other tibble and coercing into a list
members <- read_rds("data/01 - memberScrape.rds")
inductees <- list(members[,"Inductee"])
inductees <- inductees[[1]]



# Keeping the following section for my records =============

# Taking that list and filtering the tibble to get inductee names only
indFilter <- function(x) filter(idTTa, name_whole == x)
indID <- map_dfr(inductees, indFilter)
# Only 85 obs. so we're missing one.
str(indID)
head(indID)

# Seeing missing ID values but they're duplicate names
View(indID)

# 86 distinct values which is the original count so duplicates aren't coming from 
# the original list.
indTib <- tibble(inductees)
count(distinct(indTib))

# Seven missing values
sum(indID$playerId=="")

#============================================================


# Taking that list and filtering the tibble to get inductee names. Also filtering out missing values.
indFilter2 <- function(x) filter(idTTa, name_whole == x & playerId != "")
indID <- map_dfr(inductees, indFilter2)


# Need to run code I sectioned above for this to work ===========================

# Lets compare the two player lists and see what the filter loop missed
## Column names have to match.
indTib <- indTib %>% rename(name_whole = 'inductees')
indIDnam <- indID %>% select(name_whole)
# Some vowel marks, etc in the names are large part of the problem
# Don't see anything wrong w/Giles, Howsam, Herrmann though. Need to do some research on those birds.
missNames <- setdiff(indTib,indIDnam)

# ==============================================================================


# From members list, Giles was president/GM; Howsam was a GM; Herrmann was president, so they can 
# remain removed.
# Create list with corrected names. There's a Griffey Jr and Sr but both should pop up.
missNamList <- list("Dolf Luque", "Leo Cardenas", "Tony Perez", "Dave Concepcion", "Ken Griffey",
                    "Jose Rijo", "Cesar Geronimo", "Pedro Borbon")

# Pass new list to the function and assign to a tibble
missId <- map_dfr(missNamList, indFilter2)
str(missId)

# Got everything expected except an extra Borbon. One has Felix as
# a middle name. Googled him and he didn't play for the Reds so Borbope02 we can drop.
missId
missId <- filter(missId, playerId != "borbope02")

# Combine with previous tibble
indId <- bind_rows(indID, missId)
View(indId)

# Removing extra Pete Rose, Joe Morgan, Mike McCormick, George Wright. Figured which ones to drop by filtering rWAR data set and looking at the years played. Plus Sparky and Hutchinson are managers. Count = 81.
indIdComp <- filter(indId, playerId != "rosepe02" & playerId != "morgajo01"
                    & playerId != "mccormi03" & playerId != "andersp01" 
                    & playerId != "wrighge03" & playerId != "hutchfr01")
write_rds(indIdComp, "data/02 - inducteeIdsComplete.rds")


# 5 players are getting cut. See EDA.R script for the details
indIdFinal <- filter(indIdComp, playerId != "grangwa01" & playerId != "mckecbi01"
                     & playerId != "werbebi01" & playerId != "wrighge01"
                     & playerId != "wrighha01")
write_rds(indIdFinal, "data/02 - inducteeIdsFinal.rds")



# Now need the nominees
nomNamList <- list("Aaron Boone", "Adam Dunn", "John Franco", "Danny Graves", "Scott Rolen",
                   "Reggie Sanders")
nomId <- map_dfr(nomNamList, indFilter)

# Snagged an extra Sanders. Filtered rWAR again. Dropping the older one.
nomId <- filter(nomId, playerId != "sandere01")
write_rds(nomId, "data/02 - nomineeIds.rds")


