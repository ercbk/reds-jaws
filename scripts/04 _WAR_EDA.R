# EDA
## Trying to figure out how many top years should be used to calculate our RedsJAWS 
## WAR is biased towards early players so I'd like to cut those if need be. Only starting with 81 players though so don't need to get too scissor happy.

library(tidyverse)
library(openWARData)
library(Lahman)


indIdComp <- read_rds("data/02 - inducteeIdsComplete.rds")
nomId <- read_rds("data/02 - nomineeIds.rds")
members <- read_rds("data/01 - memberScrape.rds")

# Making a list of inductee IDs
indIdCompList <- list(indIdComp$playerId)
indIdCompList <- as.character(indIdCompList[[1]])

# Using the list to filter rWAR df
rWarFilter <- function(x) filter(rWAR, playerId == x)
indrWarDat <- map_dfr(indIdCompList, rWarFilter)
colnames(indrWarDat)

# Calculating years spent as a Red for each player
redsYrCount <- indrWarDat %>% 
      select(playerId, teamId, yearId) %>% 
      filter(teamId == "CIN") %>% 
      group_by(playerId) %>% 
      summarize(tenure = n())



# Checking out the distribution, basic statistics, outlier calculations----------------------------

hist <- ggplot(redsYrCount, aes(x = tenure)) +
geom_histogram(binwidth = 1)
hist
summary(redsYrCount$tenure)
sd(redsYrCount$tenure)
x <- mad(redsYrCount$tenure)
upperOutlier <- median(redsYrCount$tenure) + 2*x
lowerOutlier <- median(redsYrCount$tenure) - 2*x
# 14.9304 for upper threshold (Upper isn't as important to me but gonna see who these lads are too)
upperOutlier
# 3.0696 for lower threshold
lowerOutlier

# Traditional Tukey outlier thresholds
IQR <- IQR(redsYrCount$tenure)
upperThresh <- 1.5*IQR + 11
lowerThresh <- 7 - 1.5*IQR
# 17
upperThresh
# 1
lowerThresh


# Threshold Analysis ------------------------------------------------------------------------------

# If I stick with 7yrs, that nets me 67% of the inductees with WAR stats available and it'll stay consistent with MLB JAWS. With 6yrs, that's 78%; with 5yrs, 86%; with 4yrs, 92%.
table(redsYrCount$tenure)

# Lets see when the 7yrs and under guys played and who they are.
# tenures
belowSeven <- redsYrCount %>% 
      filter(tenure<7)

# Years played
belowSevenList <- list(belowSeven$playerId)
belowSevenList <- as.character(belowSevenList[[1]])
bSevFilter <- function(x) filter(rWAR, playerId == x)
bSevDat <- map_dfr(belowSevenList, bSevFilter)
bSevDatT <- bSevDat %>%
      select(playerId, teamId, yearId) %>% 
      filter(teamId == "CIN")

# full names
bSev2Filter <- function(x) filter(indIdComp, playerId == x)
bSev2Dat <- map_dfr(belowSevenList, bSev2Filter)

# Seaver has six seasons and Dave "the Cobra" Parker has four. I am loathe to cut the Cobra out of ANYTHING so starting RedsJAWS threshold will be four best seasons as a Red. I'm going to leave it to others to convince me that it should be 5, 6, or 7. Don't think having it at four will affect the workflow much. grangwa01, mckecbi01, and werbebi01 are out.
playersCut <- redsYrCount %>% 
      filter(tenure<5)



# Keeping with the mad-outlier calculations, lets see about these players having over 14 yrs of tenure with the Reds. Four Big Red Machine alumni plus Nuxhall, Larkin, and a 1800s player, Bid McPhee. With the upper threshold my worry would be a watering down of the distribution but I don't think that will be a concern with these players. May need to go deeper at a later date but at first glance, doesn't seem necessary.
# tenures
above14 <- redsYrCount %>% 
      filter(tenure > 14)

# years played for Cincytown
above14List <- list(above14$playerId)
above14List <- as.character(above14List[[1]])
above14Filter <- function(x) filter(rWAR, playerId == x)
above14Dat <- map_dfr(above14List, above14Filter)
above14DatT <- above14Dat %>%
      select(playerId, teamId, yearId) %>% 
      filter(teamId == "CIN")

# full names
above142Filter <- function(x) filter(indIdComp, playerId == x)
above142Dat <- map_dfr(above14List, above142Filter)



# Nominees should have at least 4 yrs tenure and doubt we see someone over 14 but better make sure.
# Making a list of nominee IDs
nomIdList <- list(nomId$playerId)
nomIdList <- as.character(nomIdList[[1]])

# Using the list to filter rWAR df
rWarFilter <- function(x) filter(rWAR, playerId == x)
nomrWarDat <- map_dfr(nomIdList, rWarFilter)

# Hmphh. Scottie "Jasper" Rolen only with 4 yrs. Seemed longer. Guess four was a good number after all. Doing this project for this year's nominees is the entire point so you have to include Rolen. That's two good reasons for four. All are under the upper threshold fyi.
nomYrCount <- nomrWarDat %>% 
      select(playerId, teamId, yearId) %>% 
      filter(teamId == "CIN") %>% 
      group_by(playerId) %>% 
      summarize(tenure = n())



#---------------------------------------------------------------------------------------------------

# Just dawned on me that there's only 79 players when there should be 81
n_distinct(redsYrCount$playerId)
# Wright bros are missing. May remember them not being in the rWAR dataset. No they're there. The problem is that they played with the Reds before 1871 so no stats available for them. Guess they get cut too. 5 total getting cut now.
missingObs <- setdiff(indIdComp$playerId, redsYrCount$playerId)
missingObs
filter(rWAR, playerId == "wrighge01" | playerId == "wrighha01")



