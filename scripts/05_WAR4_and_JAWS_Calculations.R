# Calculations





library(tidyverse)
library(openWARData)
library(Lahman)

nomId <- read_rds("data/02 - nomineeIds.rds")
indId <- read_rds("data/02 - inducteeIdsFinal.rds")
nomWarT <- read_rds("data/03 - nomineeWARtotal.rds")
nomWarR <- read_rds("data/03 - nomineeWARreds.rds")
indWarT <- read_rds("data/03 - inducteeWARtotal.rds")
indWarR <- read_rds("data/03 - inducteeWARreds.rds")


# Inductee War total without Craft who only had 6 yrs MLB service
indWarTnoC <- indWarT %>% filter(playerId != "craftha01")

# Career WAR and PeakWAR
carWardat <- indWarTnoC %>%
      group_by(playerId) %>%
      summarize(careerWAR = sum(rWAR), PeakWAR = max(rWAR)) %>%
      ungroup()

# WAR7 best seven WAR years
WAR7dat <- indWarTnoC %>%
      group_by(playerId) %>%
      top_n(7, rWAR) %>%
      tally(rWAR) %>%
      rename(WAR7 = n)
 
# Adding WAR7 column and calculating JAWS
warJaws <- carWardat %>% add_column(WAR7 = WAR7dat$WAR7)
warJaws <- mutate(warJaws, JAWS = round((careerWAR + WAR7)/2, 2))

# Joining warJAWS and inductee Id tables; Adding totalYrs col; rearranging columns
yearTot <- indWarT %>%
      select(playerId, yearId) %>%
      group_by(playerId) %>%
      summarize(sumYr = n_distinct(yearId))
yearTotc <- yearTot %>% filter(playerId != "craftha01")
warJaws <- warJaws %>% add_column(totalYrs = yearTotc$sumYr)
indIdnoC <- indId %>% filter(playerId != "craftha01")
warJaws <- inner_join(warJaws, indIdnoC, by = "playerId")
warJaws <- warJaws %>% select(playerId, fangraphs_id, name_whole, POS, totalYrs, careerWAR, PeakWAR, WAR7, JAWS, name_first, name_last)

write_rds(warJaws, "data/indCarWARandJAWS.rds")


# Reds Years =========================

# Reds career WAR and Peak War
carWarR <- indWarR %>% group_by(playerId) %>%
        summarize(redsWAR = sum(rWAR), redsPeakWAR = max(rWAR)) %>% 
        ungroup()

# Calculating top 4 WAR years
redsWAR4dat <- indWarR %>%
      group_by(playerId) %>%
      top_n(4, rWAR) %>%
      tally(rWAR) %>%
      rename(WAR4 = n)

# Adding WAR4 and calculating JAWS; reordering columns
redsWarJaws <- carWarR %>% add_column(redsWAR4 = redsWAR4dat$WAR4)
redsWarJaws <- mutate(redsWarJaws, redsJAWS = round((redsWAR + redsWAR4)/2, 2))
redsWarJaws <- redsWarJaws %>% select(playerId, redsWAR, redsPeakWAR, redsWAR4, redsJAWS)

# Joining dataframes to get names, pos, etc. with jaws and war. Adding total years as a Red.
redsWarJaws <- inner_join(redsWarJaws, indId, by = "playerId")
yearTotR <- indWarR %>%
      select(playerId, yearId) %>%
      group_by(playerId) %>%
      summarize(sumYr = n_distinct(yearId))
redsWarJaws <- redsWarJaws %>% add_column(totalYrs = yearTotR$sumYr)
redsWarJaws <- redsWarJaws %>% select(playerId, fangraphs_id, name_whole, POS, totalYrs, redsWAR, redsPeakWAR, redsWAR4, redsJAWS, name_first, name_last)

write_rds(redsWarJaws, "data/05 - indRedsWARandJAWS.rds")



# Nominees=========================================================


# Career WAR
nCarWardat <- nomWarT %>%
      group_by(playerId) %>%
      summarize(careerWAR = sum(rWAR), PeakWAR = max(rWAR)) %>%
      ungroup()

# sum of Top 7 WAR years
nWAR7dat <- nomWarT %>%
      group_by(playerId) %>%
      top_n(7, rWAR) %>%
      tally(rWAR) %>%
      rename(WAR7 = n)

# Career JAWS calculation
nWarJaws <- nCarWardat %>% add_column(WAR7 = nWAR7dat$WAR7)
nWarJaws <- mutate(nWarJaws, JAWS = round((careerWAR + WAR7)/2, 2))

# Joining warJAWS and inductee Id tables; Adding totalYrs col; rearranging columns
nYearTot <- nomWarT %>%
      select(playerId, yearId) %>%
      group_by(playerId) %>%
      summarize(sumYr = n_distinct(yearId))
nWarJaws <- nWarJaws %>% add_column(totalYrs = nYearTot$sumYr)
nWarJaws <- inner_join(nWarJaws, nomId, by = "playerId")
nWarJaws <- nWarJaws %>% select(playerId, fangraphs_id, name_whole, POS, totalYrs, careerWAR, PeakWAR, WAR7, JAWS, name_first, name_last)

write_rds(nWarJaws, "data/nomCarWARandJAWS.rds")


# Reds years ========================

# WAR during tenure as Red
nWarRdat <- nomWarR %>% group_by(playerId) %>%
      summarize(redsWAR = sum(rWAR), redsPeakWAR = max(rWAR)) %>% 
      ungroup()

# sum of Top 4 WAR years as Red
nRedsWAR4dat <- nomWarR %>%
      group_by(playerId) %>%
      top_n(4, rWAR) %>%
      tally(rWAR) %>%
      rename(WAR4 = n)

# Adding WAR4 and calculating JAWS; reordering columns
nRedsWarJaws <- nWarRdat %>% add_column(redsWAR4 = nRedsWAR4dat$WAR4)
nRedsWarJaws <- mutate(nRedsWarJaws, redsJAWS = round((redsWAR + redsWAR4)/2, 2))
nRedsWarJaws <- nRedsWarJaws %>% select(playerId, redsWAR, redsPeakWAR, redsWAR4, redsJAWS)

# Joining dataframes to get names, pos, etc. with jaws and war. Adding tenure as a Red.
nRedsWarJaws <- inner_join(nRedsWarJaws, nomId, by = "playerId")
nYearTotR <- nomWarR %>%
      select(playerId, yearId) %>%
      group_by(playerId) %>%
      summarize(sumYr = n_distinct(yearId))
nRedsWarJaws <- nRedsWarJaws %>% add_column(tenure = nYearTotR$sumYr)
nRedsWarJaws <- nRedsWarJaws %>% select(playerId, fangraphs_id, name_whole, POS, tenure, redsWAR, redsPeakWAR, redsWAR4, redsJAWS, name_first, name_last)

write_rds(nRedsWarJaws, "data/05 - nomRedsWARandJAWS.rds")
