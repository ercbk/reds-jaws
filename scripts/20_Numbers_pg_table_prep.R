# Numbers pg table prep



library(tidyverse)



# Tenure ==============================================

# Batting has no fg_id, etc. Plus need to wittle down some of these stats and maybe add others. Basically a re-do of script 13 which is pretty much a re-do of script 10. UGH!


# Reds HOF inductee and nominee war and jaws (career) stats
iWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds")
nWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")
inWandJp <- iWandJ %>% bind_rows(nWandJ) %>% filter(POS == "P")
inWandJb <- iWandJ %>% bind_rows(nWandJ) %>% filter(POS != "P")

# franchise advanced stats for entire Reds career
advFranBat <- read_rds("data/09 - franchiseAdvBatting.rds")

# franchise traditional stats for entire Reds career
tradFranBat <- read_rds("data/09 - franchiseTradBatting.rds")



# Batting =============

fgFranBat <- read_csv("data/csv/09 - FanGraphs Franchise Batting.csv")
rolen <- read_csv("data/csv/09 - FanGraphs Rolen.csv") %>% 
      filter(Name == "Scott Rolen")
mcc <- read_csv("data/csv/09 - FanGraphs McCormick.csv") %>% 
      filter(Name == "Mike McCormick")
smk <- read.csv("data/csv/09 - FanGraphs Burgess.csv") %>% 
      rename(Name = ï..Name) %>% 
      mutate(Name = as.character(Name)) %>% 
      filter(Name == "Smoky Burgess")

left_out <- rolen %>% 
      bind_rows(mcc) %>% 
      bind_rows(smk) %>%
      select(playerid, Name, WAR)
fgFranBat <- fgFranBat %>% 
      select(playerid, Name, WAR) %>% 
      bind_rows(left_out) %>% 
      mutate(Name = if_else(Name == "Ken Griffey Jr.", "Ken Griffey", Name), Name = if_else(Name == "Eddie Taubensee", "Ed Taubensee", Name), Name = if_else(Name == "George Kelly", "High Pockets Kelly", Name), Name = if_else(Name =="Dick Hoblitzel", "Dick Hoblitzell", Name), Name = if_else(Name == "Elmer Smith", "Mike Smith", Name))
missing_fg <- setdiff(inWandJb$fangraphs_id, fgFranBat$playerid)
# Not applicable. Its franchise so it's supposed to have player not in HOF. Just leaving this here. Had players missing in this process and everything got confused.
#missing_wj <- setdiff(fgFranBat$playerid, inWandJb$fangraphs_id)

# Adding WAR col
advFranBat <- advFranBat %>%
      inner_join(fgFranBat, by = c("name_whole" = "Name", "fg_playerId" = "playerid")) %>%
      rename(Name = name_whole)
missing_fg <- setdiff(inWandJb$fangraphs_id, advFranBat$fg_playerId)

# Combine traditional and advanced tables; 
franchise_batting <- tradFranBat %>% 
      inner_join(advFranBat[, -c(2,3,4,5,6,7,8)], by = "bbref_playerId")
missing_fran <- setdiff(inWandJb$playerId, franchise_batting$bbref_playerId)

# add Jr to Griff; remove some extraneous columns; rename name_whole, WAR
jr <- franchise_batting %>% 
      filter(fg_playerId == "327") %>% 
      mutate(name_whole = if_else(name_whole == "Ken Griffey", "Ken Griffey Jr", name_whole))


fran_num <- franchise_batting[-54,] %>% 
      bind_rows(jr) %>%
      rename(Name = name_whole, fWAR = WAR) %>% 
      arrange(bbref_playerId) %>% 
      select(-CS)

# Values have % symbol included. Getting rid of it
K_num <- strsplit(fran_num$`K%`, " ", fixed = TRUE)
BB_num <- strsplit(fran_num$`BB%`, " ", fixed = TRUE)

fran_num <- fran_num[-c(19,20)]

firstElt <- function(x) {x[1]}
fran_num$K_num <- sapply(K_num, firstElt)
fran_num$BB_num <- sapply(BB_num, firstElt)

fran_num <- fran_num %>% 
      mutate('K%' = as.numeric(K_num), 'BB%' = as.numeric(BB_num)) %>% 
      select(-K_num, -BB_num)

fran_num <- fran_num %>% 
      select(bbref_playerId:BB, `BB%`, SO, `K%`, everything())


hof_num <- map_dfr(inWandJb$playerId, function(x) {filter(fran_num, bbref_playerId == x)}) %>% 
      rename(`BBRef Id` = bbref_playerId, `FG Id` = fg_playerId) %>% 
      arrange(`BBRef Id`)

# Add bWAR and JAWS4 column
wandj_b <- inWandJb %>% 
      select(playerId, redsWAR, redsJAWS) %>% 
      rename(`BBRef Id` = playerId, WAR = redsWAR, JAWS4 = redsJAWS)

hof_num <- hof_num %>% 
      inner_join(wandj_b, by = "BBRef Id")

write_rds(hof_num, "data/20 - Numbers pg HOF Batting.rds")




# Pitching ====


num_hof_pitching <- read_rds("data/13 - HOF Pitching wIds.rds") %>% 
      rename(`BBRef Id` = bbref_id, `FG Id` = fg_id)

# add bWAR and JAWS4 columns
wandj_p <- inWandJp %>% 
      select(playerId, redsWAR, redsJAWS) %>% 
      rename(`BBRef Id` = playerId, WAR = redsWAR, JAWS4 = redsJAWS)

num_hof_pitching <- num_hof_pitching %>% 
      inner_join(wandj_p, by = "BBRef Id") %>% 
      select(`BBRef Id`:`K/BB`, `K%`, `BB%`, `K-BB%`, `HR/9`, everything())

write_rds(num_hof_pitching, "data/20 - Numbers pg HOF Pitching.rds")


# Season ======================================================


# Traditional

# round_any allows me to truncate at the tenths decimal place
sTradBat <- read_rds("data/10 - seasTraditionalBatStats.rds") %>% 
      rename(`BBRef Id` = bbref_id)
sTradPit <- read_rds("data/10 - seasTraditionalPitStats.rds") %>% 
      mutate(IP = plyr::round_any(IP, accuracy = 0.1, f = floor)) %>% 
      rename(`BBRef Id` = bbref_id)

# Advanced
sAdvBat <- read_rds("data/10 - seasAdvancedBatStats.rds") %>% 
      rename(`BBRef Id` = playerId, Year = yearId) %>% 
      select(-wRAA)
sAdvPit <- read_rds("data/10 - seasAdvancedPitStats.rds") %>% 
      rename(`BBRef Id` = playerId, Year = yearId)

# 53 Batters and 29 pitchers. Good
sAdvBat %>% summarize(n = n_distinct(`BBRef Id`))
sTradBat %>% summarize(n = n_distinct(`BBRef Id`))
sTradPit %>% summarize(n = n_distinct(`BBRef Id`))
sAdvPit %>% summarize(n = n_distinct(`BBRef Id`))

# Get Names and FG Ids
num_hof_batting <- read_rds("data/20 - Numbers pg HOF Batting.rds") 
num_hof_pitching <- read_rds("data/20 - Numbers pg HOF Pitching.rds")

seasBat <- sTradBat %>%
      mutate(`FG Id` = plyr::mapvalues(`BBRef Id`, from = num_hof_batting$`BBRef Id`, to = num_hof_batting$`FG Id`), Name = plyr::mapvalues(`BBRef Id`, from = num_hof_batting$`BBRef Id`, to = num_hof_batting$Name)) %>% 
      select(`BBRef Id`, `FG Id`, Name, everything()) %>% 
      inner_join(sAdvBat, by = c("BBRef Id", "Year"))

seasPit <- sTradPit %>%
      mutate(`FG Id` = plyr::mapvalues(`BBRef Id`, from = num_hof_pitching$`BBRef Id`, to = num_hof_pitching$`FG Id`), Name = plyr::mapvalues(`BBRef Id`, from = num_hof_pitching$`BBRef Id`, to = num_hof_pitching$Name)) %>% 
      select(`BBRef Id`, `FG Id`, Name, everything()) %>% 
      inner_join(sAdvPit, by = c("BBRef Id", "Year"))

seasBat_noId <- seasBat %>% 
      select(-`BBRef Id`, -`FG Id`)
seasPit_noId <- seasPit %>% 
      select(-`BBRef Id`, -`FG Id`)

write_rds(seasBat_noId, "data/20 - Numbers pg HOF seas Batting.rds")
write_rds(seasBat, "data/20 - Numbers pg wIds HOF seas Batting.rds")
write_rds(seasPit_noId, "data/20 - Numbers pg HOF seas Pitching.rds")
write_rds(seasPit, "data/20 - Numbers pg wIds HOF seas Pitching.rds")


# Fielding, tenure =======================================

# Need bbref ids and I'll move innings further to the front
field_tenure <- read_rds("data/10 - careerAdvancedFieldingStats.rds")
n_distinct(field_tenure$playerid)

# Griff jr not included in fielding stats; and Peter Rose Jr is included
setdiff(num_hof_batting$`FG Id`, field_tenure$playerid)
setdiff(field_tenure$playerid, num_hof_batting$`FG Id`)

# slice can kiss my ass. Not working for me
rose_jr <- field_tenure %>% 
      filter(playerid == "1011218")
field_tenure <- field_tenure %>% 
      anti_join(rose_jr, by = c("playerid", "Pos"))

adv_field <- read_csv("data/csv/10 - FanGraphs DRS, TZL, UZR, DEF.csv")
tz_field <- read_csv("data/csv/10 - FanGraphs TZ.csv")

griff_adv <- adv_field %>% 
      filter(playerid == "327") %>% 
      select(playerid, Pos, DRS, TZL, UZR, Def)

griff <- tz_field %>%
      filter(playerid == "327") %>% 
      select(playerid, Name, Pos, G, GS, Inn, TZ) %>% 
      inner_join(griff_adv, by = c("playerid", "Pos")) %>% 
      mutate(Name = if_else(Name == "Ken Griffey Jr.", "Ken Griffey Jr", Name))

field_tenure <- field_tenure %>% 
      bind_rows(griff)

field_tenure <- field_tenure %>% 
      mutate(`BBRef Id` = plyr::mapvalues(playerid, from = num_hof_batting$`FG Id`, to = num_hof_batting$`BBRef Id`)) %>% 
      rename(`FG Id` = playerid) %>% 
      select(`BBRef Id`, `FG Id`, Pos:GS, Inn, everything())

# We good.
setdiff(num_hof_batting$`FG Id`, field_tenure$`FG Id`)
setdiff(field_tenure$`FG Id`, num_hof_batting$`FG Id`)
n_distinct(field_tenure$`FG Id`)

field_ten_noIds <- field_tenure %>% 
      select(-`BBRef Id`, -`FG Id`)

write_rds(field_ten_noIds, "data/20 - Numbers pg HOF Fielding.rds")
write_rds(field_tenure, "data/20 - Numbers pg wIds HOF Fielding.rds")



# Postseason, tenure ===========================================

hof_bat <- read_rds("data/20 - Numbers pg HOF Batting.rds")
hof_Pit <- read_rds("data/20 - Numbers pg HOF Pitching.rds")

# Some pruning; Rename Slugging; add FG Id, Name
ps_bat <- read_rds("data/11 - careerRedsPostseasonBat.rds") %>% 
      select(-CS, -IBB, -HBP, -SH, -SF, -GIDP, -PA.1, -TB.1) %>% 
      rename(SLG = SlugPct) %>%
      mutate(`FG Id` = plyr::mapvalues(playerId, from = hof_bat$`BBRef Id`, to = hof_bat$`FG Id`), Name = plyr::mapvalues(playerId, from = hof_bat$`BBRef Id`, to = hof_bat$Name)) %>% 
      rename(`BBRef Id` = playerId, `2B` = X2B, `3B` = X3B) %>% 
      select(`BBRef Id`, `FG Id`, Name, everything())
      
      
ps_pit <- read_rds("data/11 - careerRedsPostseasonPit.rds") %>% 
      select(-HR, -IBB, -WP, -HBP, -BK, -BFP, -GF, -SH, -SF, -GIDP, -IPouts) %>% 
      mutate(`FG Id` = plyr::mapvalues(playerId, from = hof_Pit$`BBRef Id`, to = hof_Pit$`FG Id`), Name = plyr::mapvalues(playerId, from = hof_Pit$`BBRef Id`, to = hof_Pit$Name)) %>% 
      rename(`BBRef Id` = playerId) %>% 
      select(`BBRef Id`, `FG Id`, Name, W:CG, IP, SHO:H, R, everything())

ps_bat_noId <- ps_bat %>% 
      select(-`BBRef Id`, -`FG Id`)
ps_pit_noId <- ps_pit %>% 
      select(-`BBRef Id`, -`FG Id`)

write_rds(ps_bat_noId, "data/20 - Numbers pg HOF Postseason Batting.rds")
write_rds(ps_bat, "data/20 - Numbers pg wIds HOF Postseason Batting.rds")
write_rds(ps_pit_noId, "data/20 - Numbers pg HOF Postseason Pitching.rds")
write_rds(ps_pit, "data/20 - Numbers pg wIds HOF Postseason Pitching.rds")



# Awards ===========================================

hof_bat <- read_rds("data/20 - Numbers pg HOF Batting.rds")
hof_Pit <- read_rds("data/20 - Numbers pg HOF Pitching.rds")

awards_bat <- read_rds("data/11 - summaryBattingAwards.rds") %>%
      ungroup() %>% 
      mutate(`FG Id` = plyr::mapvalues(playerID, from = hof_bat$`BBRef Id`, to = hof_bat$`FG Id`), Name = plyr::mapvalues(playerID, from = hof_bat$`BBRef Id`, to = hof_bat$Name)) %>%
      rename(`BBRef Id` = playerID, Award = awardID) %>% 
      select(`BBRef Id`,`FG Id`, Name, everything())
      
awards_pit <- read_rds("data/11 - summaryPitchingAWards.rds") %>% 
      ungroup() %>% 
      mutate(`FG Id` = plyr::mapvalues(playerID, from = hof_Pit$`BBRef Id`, to = hof_Pit$`FG Id`), Name = plyr::mapvalues(playerID, from = hof_Pit$`BBRef Id`, to = hof_Pit$Name)) %>% 
      rename(`BBRef Id` = playerID, Award = awardID) %>% 
      select(`BBRef Id`, `FG Id`, Name, everything())

awards <- awards_bat %>% 
      bind_rows(awards_pit)

write_rds(awards, "data/20 - Numbers pg HOF Awards.rds")



# Award Shares ============================================
hof_bat <- read_rds("data/20 - Numbers pg HOF Batting.rds")
hof_Pit <- read_rds("data/20 - Numbers pg HOF Pitching.rds")

shares_bat <- read_rds("data/11 - battingAwardsVoteShares.rds") %>% 
      select(-lgID) %>% 
      mutate(`FG Id` = plyr::mapvalues(playerID, from = hof_bat$`BBRef Id`, to = hof_bat$`FG Id`), Name = plyr::mapvalues(playerID, from = hof_bat$`BBRef Id`, to = hof_bat$Name)) %>%
      rename(`BBRef Id` = playerID, Award = awardID, Year = yearID, `Vote %` = vote_percentage) %>% 
      select(`BBRef Id`,`FG Id`, Name, Year, Award, everything())


shares_pit <- read_rds("data/11 - pitchingAwardsVoteShares.rds") %>% 
      select(-lgID) %>%
      mutate(`FG Id` = plyr::mapvalues(playerID, from = hof_Pit$`BBRef Id`, to = hof_Pit$`FG Id`), Name = plyr::mapvalues(playerID, from = hof_Pit$`BBRef Id`, to = hof_Pit$Name)) %>%
      rename(`BBRef Id` = playerID, Award = awardID, Year = yearID, `Vote %` = vote_percentage) %>% 
      select(`BBRef Id`,`FG Id`, Name, Year, Award, everything())

shares <- shares_bat %>% 
      bind_rows(shares_pit)

write_rds(shares, "data/20 - Numbers pg HOF Award Shares.rds")
      
