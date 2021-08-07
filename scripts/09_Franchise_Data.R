# Franchise data
# sections: Batting, Pitching
# Note: CSVs created from Baseball Reference website. Rate stat thresholds (1500 PA, 500 IP) obtained from BBref franchise leader board page. Fangraphs uses 1000 PA (nothing for pitching) but decided to use BBRef threshold just because.


library(tidyverse)


# Batting =======================================

# BBRef

redsFranBat <- read_csv("data/09 - BBRef Franchise Batting.csv")



# Splitting Name column into name_whole and playerId. Removing "HOF" from name_whole players who are in MLB HOF.
nameId <- strsplit(redsFranBat$Name, "\\", fixed = TRUE)
firstElt <- function(x) {x[1]}
secondElt <- function(y) {y[2]}
redsFranBat$playerId <- sapply(nameId, secondElt)
redsFranBat$name_whole <- sapply(nameId, firstElt)
redsFranBat <- redsFranBat %>%
      select(-Name, -Rk) %>%
      select(playerId, name_whole, everything())
name_whole <- strsplit(redsFranBat$name_whole, " HOF", fixed = TRUE)
redsFranBat <- redsFranBat %>% 
      select(-name_whole) %>% 
      add_column(name_whole) %>% 
      select(playerId, name_whole, everything())


# Filtering nominee and inductee dfs for batters and their playerIds
iRedsWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds")
nRedsWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")
iRedsWandJb <- iRedsWandJ %>% 
      filter(POS != "P")
nRedsWandJb <- nRedsWandJ %>% 
      filter(POS != "P")
inRedsWandJb <- iRedsWandJb %>%
      bind_rows(nRedsWandJb)

# Using those playerIds to filter Franchise df to see if any batters fall below the 1500 PA threshold for rate stat qualification. Rolen, Mac, Smoky not enough.
inRedsFranBat <- map_dfr(inRedsWandJb$playerId, function(x) {redsFranBat %>% filter(playerId == x)})

# Position players that are below the 1500 PA threshold used to qualify for rate statistic ranking yet are in Reds HOF or are a nominee. Assuming top ranked players in counting stats have enough PAs.
fewAB <- redsFranBat %>% 
      filter(name_whole == "Scott Rolen" | name_whole == "Mike McCormick" | name_whole == "Smoky Burgess")

bbrefFranBat <- redsFranBat %>% 
      filter(PA > 1499) %>% 
      bind_rows(fewAB) %>% 
      rename(bbref_playerId = playerId)

# complete
missing_bbref <- setdiff(inRedsWandJb$playerId, bbrefFranBat$bbref_playerId)


# Fangraphs

# Clean as a whistle and I've already filtered for 1500 PA at the website
fgFranBat_dat <- read_csv("data/09 - FanGraphs Franchise Batting.csv")

rolen <- read_csv("data/09 - FanGraphs Rolen.csv") %>% 
      filter(Name == "Scott Rolen")
mcc <- read_csv("data/09 - FanGraphs McCormick.csv") %>% 
      filter(Name == "Mike McCormick")
smk <- read.csv("data/09 - FanGraphs Burgess.csv") %>% 
      rename(Name = ï..Name) %>% 
      mutate(Name = as.character(Name)) %>% 
      filter(Name == "Smoky Burgess")

left_out <- rolen %>% 
      bind_rows(mcc) %>% 
      bind_rows(smk)
fgFranBat <- fgFranBat_dat %>% 
      bind_rows(left_out) %>% 
      select(playerid, Name, 'BB%', 'K%', wOBA, 'wRC+', BsR) %>% 
      rename(fg_playerId = playerid)

# complete
missing_fg <- setdiff(inRedsWandJb$fangraphs_id, fgFranBat$fg_playerId)


# Combine Stats

# "Dick Hoblitzel" (2 end "l"s in bbref and others) "Ken Griffey Jr." (no Jr) "Elmer Smith" (fg has 2 Elmer Smiths but bbref and others call him, Mike.) "Eddie Taubensee" (bbref calls him Ed) "George Kelly" (High Pockets Kelly in bbref and yeah, that's the one we're using)
missing_players <- setdiff(fgFranBat$Name, bbrefFranBat$name_whole)
missing_players2 <- setdiff(bbrefFranBat$name_whole, fgFranBat$Name)

# For some reason, name_whole is a list
bbrefFranBat <- bbrefFranBat %>% mutate(name_whole = as.character(name_whole))

# Example of code used to investigate conflicts between dbs. No "Jr." in BBRef version
fgFranBat_dat %>% filter(grepl("Ken", Name)) %>% select(playerid, Name)

# Correcting conflicts between databases
fgFranBat <- fgFranBat %>% 
      mutate(Name = if_else(Name == "Ken Griffey Jr.", "Ken Griffey", Name), Name = if_else(Name == "Eddie Taubensee", "Ed Taubensee", Name), Name = if_else(Name == "George Kelly", "High Pockets Kelly", Name), Name = if_else(Name =="Dick Hoblitzel", "Dick Hoblitzell", Name), Name = if_else(Name == "Elmer Smith", "Mike Smith", Name))

# Too many rows
advFranBat <- bbrefFranBat %>% 
      select(bbref_playerId, name_whole, Yrs, From, To, G, PA, 'OPS+') %>% 
      inner_join(fgFranBat, by = c("name_whole" = "Name")) %>% 
      distinct() %>% 
      select(bbref_playerId, fg_playerId, name_whole, Yrs, From, To, G, PA, 'BB%', 'K%', wOBA, 'wRC+', 'OPS+', BsR)

# Nothing here, must be doubles
missing_players3 <- setdiff(advFranBat$name_whole, fgFranBat$Name)
missing_players4 <- setdiff(advFranBat$name_whole, bbrefFranBat$name_whole)
# Griffeys always get counted twice in these joins. Rows are combinations of each other which is what screws up distinct(). I'm joining by name so inner_join doesn't know which stats to attach to which Griffey so it creates every combination. Better than a damn error though.
table(advFranBat$name_whole)
advFranBat <- advFranBat[-c(54, 55),]

# complete
missing_adv <- setdiff(inRedsWandJb$playerId, advFranBat$bbref_playerId)

write_rds(advFranBat, "data/09 - franchiseAdvBatting.rds")


# Traditional

fg_iDandName <- fgFranBat %>% select(fg_playerId, Name)

tradFranBat <- bbrefFranBat %>% 
      select(-`OPS+`, -S, -C, -F, -ASG, -`Pos Summary`) %>% 
      inner_join(fg_iDandName, by = c("name_whole" = "Name")) %>% 
      slice(-c(54,55)) %>% 
      select(bbref_playerId, fg_playerId, everything())

missing_trad <- setdiff(inRedsWandJb$playerId, tradFranBat$bbref_playerId)

write_rds(tradFranBat, "data/09 - franchiseTradBatting.rds")



# Pitching =====================================

# BBRef

redsFranPit <- read_csv("data/csv/09 - bbref Franchise Pitching.csv")


# Same as with Batting
nameId <- strsplit(redsFranPit$Name, "\\", fixed = TRUE)
firstElt <- function(x) {x[1]}
secondElt <- function(y) {y[2]}
redsFranPit$playerId <- sapply(nameId, secondElt)
redsFranPit$name_whole <- sapply(nameId, firstElt)
redsFranPit <- redsFranPit %>%
      select(-Name, -Rk) %>%
      select(playerId, name_whole, everything())
name_whole <- strsplit(redsFranPit$name_whole, " HOF", fixed = TRUE)
redsFranPit <- redsFranPit %>% 
      select(-name_whole) %>% 
      add_column(name_whole) %>% 
      select(playerId, name_whole, everything())

# Filtering nominee and inductee dfs for pitchers and their playerIds
iRedsWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds")
nRedsWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")
iRedsWandJp <- iRedsWandJ %>% 
      filter(POS == "P")
nRedsWandJp <- nRedsWandJ %>% 
      filter(POS == "P")
inRedsWandJp <- iRedsWandJp %>%
      bind_rows(nRedsWandJp)

# Using those playerIds to filter Francise df to see if any pitchers fall below the 500 IP threshold for rate stat qualification. Everybody made it.
inRedsFranPit <- map_dfr(inRedsWandJp$playerId, function(x) {redsFranPit %>% filter(playerId == x)})

# Filtering Franchise df for pitchers with at least 500 innings pitched
bbrefFranPit <- redsFranPit %>% 
      filter(IP > 499) %>% 
      mutate(name_whole = as.character(name_whole))


# FanGraphs

fgFranPit_dat <- read_csv("data/csv/09 - FanGraphs Franchise Pitching.csv")

# Some naming conflicts but no one is left out
missing_pitchers <- setdiff(bbrefFranPit$name_whole, fgFranPit_dat$Name)
missing_pitchers2 <- setdiff(fgFranPit_dat$Name, bbrefFranPit$name_whole)

fgFranPit <- fgFranPit_dat %>% 
      mutate(Name = if_else(Name == "Elmer Smith", "Mike Smith", Name), Name = if_else(Name == "Junior Thompson", "Gene Thompson", Name), Name = if_else(Name == "Elton Chamberlain", "Ice Box Chamberlain", Name))

# Good to go
missing_pitchers3 <- setdiff(bbrefFranPit$name_whole, fgFranPit$Name)
missing_pitchers4 <- setdiff(fgFranPit$Name, bbrefFranPit$name_whole)


# add-in fWAR; forgot to filter IP > 499 at the sit.
fWar <- read_csv("data/csv/09 - FanGraphs Pitching fWAR.csv") %>% 
      filter(IP > 499) %>% 
      select(playerid, Name, WAR) %>% 
      rename(fWAR = WAR) %>% 
      mutate(Name = if_else(Name == "Elmer Smith", "Mike Smith", Name), Name = if_else(Name == "Junior Thompson", "Gene Thompson", Name), Name = if_else(Name == "Elton Chamberlain", "Ice Box Chamberlain", Name))

# All good
missing_pitchers <- setdiff(bbrefFranPit$name_whole, fWar$Name)
missing_pitchers2 <- setdiff(fWar$Name, bbrefFranPit$name_whole)

fgFranPit <- fgFranPit %>% 
      inner_join(fWar, by = c("playerid", "Name"))
# Combine Stats

# Swapping, copying, and discarding columns

era_plus <- bbrefFranPit %>% 
      select(name_whole, playerId, Yrs, To, From, `ERA+`)

fgID <- fgFranPit %>%
      select(playerid, Name)

tradFranPit <- bbrefFranPit %>% 
      select(playerId, name_whole, Yrs, From, To, W, L, `W-L%`, ERA, G, GS, CG, SHO, SV, IP, SO) %>% 
      inner_join(fgID, by = c("name_whole" = "Name")) %>% 
      rename(bbref_id = playerId, fg_id = playerid) %>% 
      mutate(`W-L%` = round(`W-L%` * 100, 0)) %>% 
      select(bbref_id, fg_id, everything())

advFranPit <- fgFranPit %>% 
      select(-`E-F`, -xFIP, -ERA, -`LOB%`, -BABIP, -FIP) %>% 
      inner_join(era_plus, by = c("Name" = "name_whole")) %>% 
      rename(bbref_id = playerId, fg_id = playerid) %>%
      select(bbref_id, fg_id, Name, Yrs, From, To, `ERA-`, `FIP-`, `xFIP-`, `ERA+`, WHIP, SIERA, everything())


write_rds(tradFranPit, "data/09 - tradFranchisePitching.rds")
write_rds(advFranPit, "data/09 - advFranchisePitching.rds")



