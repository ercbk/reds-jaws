# Adv Stat Calculation
# Sections: Create Constants tibbles, Pitching(gather stats, calculation), Batting(gather stats, calculation), Fielding
# Note: Links to averages, factors, and constants (aka 'guts') can be found on the fangraphs glossary pages for each of the adv stats calc'd





library(tidyverse)
library(Lahman)
library(rlang)

# Various constants
fConstants <- read_csv("./10 - FanGraphsConstants.csv")
# Park Factors
fFactors <- read_csv("./10 - FanGraphsParkFactors.csv")
# Handedness Park Factors (don't end up needing them)
fHand <- read_csv("./10 - FanGraphsHandedness.csv")
# NL league Avgs for Pitching and Batting stats
fPitAvgs <- read_csv("./10 - FanGraphsNLPitAvgs.csv")
fnBatAvgs <- read_csv("./10 - FanGraphsNLBatAvgs.csv")
# MLB League Averages for Batting stats
fBatAvgs <- read_csv("./10 - FanGraphsMLBBatAvgs.csv")


iRedsWar <- read_rds("data/03 - inducteeWARreds.rds") %>% 
      select(playerId, yearId)
nRedsWar <- read_rds("data/03 - nomineeWARreds.rds") %>%
      select(playerId, yearId)




# Synching playerIds, yearIds to constants, etc =====================


# Creating inductee, nominee tibbles by filtering by yearId and binding playerId and yearId cols

iBatAvgs <- map_dfr(iRedsWar$yearId, function(x) {
      filter(fBatAvgs, Season == x)
}) %>% 
      bind_cols(iRedsWar) %>% 
      select(playerId, yearId, everything())

nBatAvgs <- map_dfr(nRedsWar$yearId, function(x) {
      filter(fBatAvgs, Season == x)
}) %>% 
      bind_cols(nRedsWar) %>% 
      select(playerId, yearId, everything())
inBatAvgs <- iBatAvgs %>% 
      bind_rows(nBatAvgs)

iNLBatAvgs <- map_dfr(iRedsWar$yearId, function(x) {
      filter(fnBatAvgs, Season == x)
}) %>% 
      bind_cols(iRedsWar) %>% 
      select(playerId, yearId, everything())

nNLBatAvgs <- map_dfr(nRedsWar$yearId, function(x) {
      filter(fnBatAvgs, Season == x)
}) %>% 
      bind_cols(nRedsWar) %>% 
      select(playerId, yearId, everything())
inNLBatAvgs <- iNLBatAvgs %>% 
      bind_rows(nNLBatAvgs)


iPitAvgs <- map_dfr(iRedsWar$yearId, function(x) {
      filter(fPitAvgs, Season == x)
}) %>% 
      bind_cols(iRedsWar) %>% 
      select(playerId, yearId, everything())

nPitAvgs <- map_dfr(nRedsWar$yearId, function(x) {
      filter(fPitAvgs, Season == x)
}) %>% 
      bind_cols(nRedsWar) %>% 
      select(playerId, yearId, everything())
inPitAvgs <- iPitAvgs %>% 
      bind_rows(nPitAvgs)


iConstants <- map_dfr(iRedsWar$yearId, function(x) {
      filter(fConstants, Season == x)
}) %>% 
      bind_cols(iRedsWar) %>% 
      select(playerId, yearId, everything())

nConstants <- map_dfr(nRedsWar$yearId, function(x) {
      filter(fConstants, Season == x)
}) %>% 
      bind_cols(nRedsWar) %>% 
      select(playerId, yearId, everything())
inConstants <- iConstants %>% 
      bind_rows(nConstants) %>% 
      rename(R_per_PA = "R/PA", R_per_W = "R/W")

# 1878, 1879, 1880 not included. Opened csv in Excel and added season, team values so I could bind playerId and yearId cols (nrows needs to be equal).
# Starting at 2001, cols 10:13 are all missing values
# Starting at 1973, cols 4:14 are all missing values (leaving basic, team, and year)
# Starting at 1881, cols 3:14 are all missing values (leaving team and year)
iFactors <- map_dfr(iRedsWar$yearId, function(x) {
      filter(fFactors, Season == x)
}) %>% 
      bind_cols(iRedsWar) %>%
      select(playerId, yearId, everything())

nFactors <- map_dfr(nRedsWar$yearId, function(x) {
      filter(fFactors, Season == x)
}) %>%  
      bind_cols(nRedsWar) %>%
      select(playerId, yearId, everything())
inFactors <- iFactors %>% 
      bind_rows(nFactors)

# Handedness Park Factors only available from 2002 to 2015
iRedsWarx <- iRedsWar %>% 
      filter(between(yearId, 2002, 2015))
nRedsWarx <- nRedsWar %>%
      filter(between(yearId, 2002, 2015))

iHand <- map_dfr(iRedsWarx$yearId, function(x) {
      filter(fHand, Season == x)
}) %>% 
      bind_cols(iRedsWarx) %>% 
      select(playerId, yearId, everything())

nHand <- map_dfr(nRedsWarx$yearId, function(x) {
      filter(fHand, Season == x)
}) %>% 
      bind_cols(nRedsWarx) %>% 
      select(playerId, yearId, everything())
inHand <- iHand %>% 
      bind_rows(nHand)


# Combine necessary constants ===============

# Batting

tempB_inMLBBatAvgs <- inBatAvgs %>% 
      select(playerId, yearId, wOBA)
tempB_inConstants <- inConstants %>% 
      select(playerId, yearId, wBB, wHBP, w1B, w2B, w3B, wHR, wOBAScale, R_per_PA)
tempB_inFactors <- inFactors %>% 
      select(playerId, yearId, Basic)
tempB_inNLBatAvgs <- inNLBatAvgs %>% 
      select(playerId, yearId, wRC, PA)
batting_constants <- full_join(tempB_inMLBBatAvgs, tempB_inNLBatAvgs, by = c("playerId", "yearId"))
batting_constants <- full_join(batting_constants, tempB_inConstants, by = c("playerId", "yearId"))
batting_constants <- full_join(batting_constants, tempB_inFactors, by = c("playerId", "yearId"))

write_rds(batting_constants, "data/10 - BattingConstants.rds")

# Pitching

tempP_inConstants <- inConstants %>% 
      select(playerId, yearId, cFIP)
tempP_inFactors <- inFactors %>% 
      rename(pf_FIP = FIP) %>% 
      select(playerId, yearId, pf_FIP, Basic)
tempP_inNLPitAvgs <- inPitAvgs %>% 
      select(playerId, yearId, FIP)
pitching_constants <- full_join(tempP_inNLPitAvgs, tempP_inFactors, by = c("playerId", "yearId"))
pitching_constants <- full_join(pitching_constants, tempP_inConstants, by = c("playerId", "yearId"))

write_rds(pitching_constants, "data/10 - PitchingConstants.rds")



# Pitching  ===========================================================


# Gathering needed stats ========================

# Inductees
iRedsWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds") %>% 
      filter(POS == "P")
redsYrsPit <- map_dfr(iRedsWandJ$playerId, function(x) {
      filter(iRedsWar, playerId == x)
})

redsPitStats <- map2_dfr(redsYrsPit$playerId, redsYrsPit$yearId, function(x, y) {
      filter(Pitching, playerID == x & yearID == y) %>% 
            mutate(IP = IPouts/3) %>% 
            select(playerID, teamID, yearID, W, L, G, GS, CG, SV, IP, H, ER, HR, BB, HBP, SHO, SO, BAOpp)
})

# Something is up with Jim O'Toole's ID. Pitching db in Lahman has otoolji01 while openWARdat doesn't have the "l" but has "'". openWARdat has the correct id for the bbref site. So that needs changed at my earliest convenience.
otoole <- Pitching %>%
      filter(playerID == "otoolji01" & teamID == "CIN") %>%
      mutate(IP = IPouts/3) %>%
      select(-playerID) %>% 
      add_column(playerID = rep("o'tooji01", 9)) %>% 
      select(playerID, teamID, yearID, W, L, G, GS, CG, SV, IP, H, ER, HR, BB, HBP, SHO, SO, BAOpp)
redsPitStats <- redsPitStats %>%
      bind_rows(otoole)


# Nominees
nRedsWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds") %>% 
      filter(POS == "P")
nRedsYrsPit <- map_dfr(nRedsWandJ$playerId, function(x) {
      filter(nRedsWar, playerId == x)
})

nRedsPitStats <- map2_dfr(nRedsYrsPit$playerId, nRedsYrsPit$yearId, function(x, y) {
      filter(Pitching, playerID == x & yearID == y) %>% 
            mutate(IP = IPouts/3) %>% 
            select(playerID, teamID, yearID, W, L, G, GS, CG, SV, IP, H, ER, HR, BB, HBP, SHO, SO, BAOpp)
})

# Need this for Numbers Pg
inTradPitStats <- redsPitStats %>% 
      bind_rows(nRedsPitStats) %>% 
      filter(teamID == "CIN" | teamID == "CN1" | teamID == "CN2" | teamID == "CN3") %>% 
      rename(bbref_id = playerID, Year = yearID) %>% 
      select(-teamID, -HR, -HBP)
n_distinct(inTradPitStats$bbref_id)
setdiff(redsPitStats$playerID, inTradPitStats$bbref_id)
write_rds(inTradPitStats, "data/10 - seasTraditionalPitStats.rds")

# Stats for adv stats calc
inRedsPitStats <- redsPitStats %>% 
      bind_rows(nRedsPitStats) %>% 
      filter(teamID == "CIN" | teamID == "CN1" | teamID == "CN2") %>% 
      rename(playerId = playerID, yearId = yearID) %>% 
      select(playerId, yearId, H, ER, HR, BB, HBP, SO, IP)
n_distinct(inRedsPitStats$playerId)
write_rds(inRedsPitStats, "data/10 - inRedsPitStats.rds")


# Calculation ===================================

inRedsPitStats <- read_rds("data/10 - inRedsPitStats.rds")
pitching_constants <- read_rds("data/10 - PitchingConstants.rds")

# FIP
fip <- function(player, year) {
      
      fipCon <- pitching_constants %>% 
            filter(playerId == !!player & yearId == !!year)
      cFIP <- fipCon$cFIP[1]
      
      
      stat <- inRedsPitStats %>% 
            filter(playerId == !!player & yearId == !!year) 
      HR <- stat$HR[1]
      BB <- stat$BB[1]
      HBP <- stat$HBP[1]
      SO <- stat$SO[1]
      IP <- stat$IP[1]
      
      fip <- ((13*HR)+(3*(BB+HBP))-(2*SO))/IP + cFIP
}

# FIP-
fip_minus <- function(FIP, player, year) {
      
      fipCon <- pitching_constants %>% 
            filter(playerId == !!player & yearId == !!year)
      MLB_FIP <- fipCon$FIP[1]
      pf_FIP <- fipCon$pf_FIP[1]
      bsc <- fipCon$Basic[1]
      
      if(is.na(pf_FIP)) {
            fipminus <- (FIP+(FIP-(FIP*(bsc/100))))/(MLB_FIP)*100 
      } else {
            fipminus <- (FIP+(FIP-(FIP*(pf_FIP/100))))/(MLB_FIP)*100
      }
}

# Empty tibble
tempTibPit <- tibble(
      playerId = character(),
      yearId = numeric(),
      K_per_BB = numeric(),
      K_per_nine = numeric(),
      WHIP = numeric(),
      FIP_minus = numeric(),
      ERA = numeric()
)

# input: playerId and yearId; function calcs adv stats for each player

fillAdv <- function(player, year) {
      
      FIP <- fip(quo(player), quo(year))
      
      fill_stat <- inRedsPitStats %>% 
            filter(playerId == player & yearId == year)
      fill_BB <- fill_stat$BB[1]
      fill_SO <- fill_stat$SO[1]
      fill_IP <- fill_stat$IP[1]
      fill_H <- fill_stat$H[1]
      fill_ER <- fill_stat$ER[1]
      
      tempTibPit <- tempTibPit %>%
            add_row(
                  playerId = player,
                  yearId = year,
                  K_per_BB = round(fill_SO/fill_BB, 2),
                  K_per_nine = round(fill_SO/(fill_IP/9), 2),
                  WHIP = round((fill_H + fill_BB)/fill_BB, 2),
                  FIP_minus = round(fip_minus(FIP, quo(player), quo(year)), 0),
                  ERA = round((9*fill_ER)/fill_IP, 2)
            )
}

playerPitList <- list(inRedsPitStats$playerId)
playerPitList <- playerPitList[[1]]
yearPitList <- list(inRedsPitStats$yearId)
yearPitList <- yearPitList[[1]]

# Got everything except Will White's FIP- from 1878 to 1883 because there weren't HBP stats for him those years
seasRedsPitAdv <- map2_dfr(playerPitList, yearPitList, fillAdv)

missPitAdv <- naniar::miss_var_summary(seasRedsPitAdv)

seasRedsPitAdv <- seasRedsPitAdv %>% 
      rename(`K/BB` = K_per_BB, `K/9` = K_per_nine, `FIP-` = FIP_minus)

write_rds(seasRedsPitAdv, "data/10 - seasAdvancedPitStats.rds")



# Batting ====================================================


# Gathering needed stats ========================

# inductees
iRedsWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds") %>% 
      filter(POS != "P")
iRedsYrsBat <- map_dfr(iRedsWandJ$playerId, function(x) {
      filter(iRedsWar, playerId == x)
})

# nominees
nRedsWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds") %>% 
      filter(POS != "P")
nRedsYrsBat <- map_dfr(nRedsWandJ$playerId, function(x) {
      filter(nRedsWar, playerId == x)
})

# Both
inRedsYrsBat <- iRedsYrsBat %>% 
      bind_rows(nRedsYrsBat)

# Batting db augmented with other stats
# Provided in Lahman pkg
augBatStats <- battingStats()
inRedsBatStats <- map2_dfr(inRedsYrsBat$playerId, inRedsYrsBat$yearId, function(x, y) {
      filter(augBatStats, playerID == x & yearID == y)
})

# Seeing if Batting db has same extra Reds teamIds and it does
table(inRedsBatStats$teamID)
inRedsBatStats <- inRedsBatStats %>% 
      rename(playerId = playerID, yearId = yearID) %>%
      mutate(one_B = H - (X2B + X3B + HR), uBB = BB - IBB) %>% 
      filter(teamID == "CIN" | teamID == "CN1" | teamID == "CN2" | teamID == "CN3") %>% 
      mutate(teamID = as.character(teamID))

# always have to make sure everyone is here
n_distinct(inRedsBatStats$playerId)

write_rds(inRedsBatStats, "data/10 - inRedsBatStats.rds")

# Need this for Numbers page
sTradBat <- inRedsBatStats %>% 
      select(-stint, -teamID, -lgID, -CS, -IBB, -SH, -SF, -GIDP, -TB, -uBB, -HBP) %>% 
      rename(`1B` = one_B, SLG = SlugPct, bbref_id = playerId, Year = yearId) %>% 
      select(bbref_id:AB, PA, H, `1B`, everything())

write_rds(sTradBat, "data/10 - seasTraditionalBatStats.rds")

# Calculation ===================================

inRedsBatStats <- read_rds("data/10 - inRedsBatStats.rds")
batting_constants <- read_rds("data/10 - BattingConstants.rds")

# wOBA
woba <- function(player, year) {
      
      obaStat <- inRedsBatStats %>%
            filter(playerId == !!player & yearId == !!year)
      BB <- obaStat$BB[1]
      HBP <- obaStat$HBP[1]
      one_B <- obaStat$one_B[1]
      X2B <- obaStat$X2B[1]
      X3B <- obaStat$X3B[1]
      HR <- obaStat$HR[1]
      AB <- obaStat$AB[1]
      IBB <- obaStat$IBB[1]
      SF <- obaStat$SF[1]
      uBB <- obaStat$uBB[1]

      obaCon <- batting_constants %>%
            filter(playerId == !!player & yearId == !!year)
      wBB <- obaCon$wBB[1]
      wHBP <- obaCon$wHBP[1]
      w1B <- obaCon$w1B[1]
      w2B <- obaCon$w2B[1]
      w3B <- obaCon$w3B[1]
      wHR <- obaCon$wHR[1]

      woba <- ((wBB*uBB)+(wHBP*HBP)+(w1B*one_B)+(w2B*X2B)+(w3B*X3B)+(wHR*HR))/(AB+BB-IBB+SF+HBP)

}

# wRAA
wraa <- function(wOBA_val, player, year) {

      raaStat <- inRedsBatStats %>%
            filter(playerId == !!player & yearId == !!year)
      PA <- raaStat$PA[1]

      raaCon <- batting_constants %>%
            filter(playerId == !!player & yearId == !!year)
      wOBAScale <- raaCon$wOBAScale[1]
      lgwOBA <- raaCon$wOBA[1]
      
      wraa <- ((wOBA_val-lgwOBA)/wOBAScale)*PA

}

# wRC+
wrcplus <- function(wRAA_val, player, year) {
      
      rcStat <- inRedsBatStats %>% 
            filter(playerId == !!player & yearId == !!year)
      PA <- rcStat$PA[1]
      
      rcCon <- batting_constants %>%
            filter(playerId == !!player & yearId == !!year)
      r_per_PA <- rcCon$R_per_PA[1]
      NLwRC <- rcCon$wRC[1]
      NLPA <- rcCon$PA[1]
      pF <- rcCon$Basic[1]
      
      wrcplus <- ((((wRAA_val / PA) + r_per_PA) + (r_per_PA - ((pF/100) * r_per_PA))) / (NLwRC/NLPA)) * 100
}

tempTibBat <- tibble(
      playerId = character(),
      yearId = numeric(),
      wOBA = numeric(),
      wRAA = numeric(),
      wRC_plus = numeric()
)

fillBatAdv <- function(player, year) {
      
      wOBA_val <- woba(quo(player), quo(year))
      wRAA_val <- wraa(wOBA_val, quo(player), quo(year))
      wRC_plus_val <- wrcplus(wRAA_val, quo(player), quo(year))
      
      tempTibBat <- tempTibBat %>%
            add_row(
                  playerId = player,
                  yearId = year,
                  wOBA = round(wOBA_val, 3),
                  wRAA = round(wRAA_val, 1),
                  wRC_plus = round(wRC_plus_val, 0)
            )
      
}

playerBatList <- list(inRedsBatStats$playerId)
playerBatList <- playerBatList[[1]]
yearBatList <- list(inRedsBatStats$yearId)
yearBatList <- yearBatList[[1]]

seasRedsBatAdv <- map2_dfr(playerBatList, yearBatList, fillBatAdv)

seasRedsBatAdv <- seasRedsBatAdv %>% 
      rename(`wRC+` = wRC_plus)

missBatAdv <- naniar::miss_var_summary(seasRedsBatAdv)


write_rds(seasRedsBatAdv, "data/10 - seasAdvancedBatStats.rds")



# Fielding ==============================================



# Filtering nominee and inductee dfs for batters and their playerIds
iRedsWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds")
nRedsWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")
iRedsWandJb <- iRedsWandJ %>% 
      filter(POS != "P")
nRedsWandJb <- nRedsWandJ %>% 
      filter(POS != "P")
inRedsWandJb <- iRedsWandJb %>%
      bind_rows(nRedsWandJb)

# No idea why but couldn't get four players to be exported with the data unless I specified only outfielders. Weird.
fg_ofStats <- read_csv("data/10 - FanGraphs OF Std Fielding.csv")
# Adv. fielding stats only available from 2002 onward
fg_defStats <- read_csv("data/10 - FanGraphs DRS, TZL, UZR, DEF.csv")
fg_tz <- read_csv("data/10 - FanGraphs TZ.csv")

fg_ofStats_trunc <- fg_ofStats %>% 
      filter(Name == "Dummy Hoy" | Name == "Mike McCormick" | Name == "Ival Goodman" | Name == "Sam Crawford")

fg_defStats_trunc <- map_dfr(inRedsWandJb$name_whole, function(x) {filter(fg_defStats, Name == x)}) %>% 
      select(playerid, Pos, Inn, DRS, TZL, UZR, Def)

fg_tz_trunc <- map_dfr(inRedsWandJb$name_whole, function(x) {filter(fg_tz, Name == x)}) %>% 
      bind_rows(fg_ofStats_trunc) %>% 
      select(playerid, Name, Pos, G, GS, TZ)

# Got'em all
missing_fielders <- setdiff(fg_tz_trunc$Name, inRedsWandJb$name_whole)
missing_fielders2 <- setdiff(inRedsWandJb$name_whole, fg_tz_trunc$Name)

fg_fielding <- fg_tz_trunc %>% 
      full_join(fg_defStats_trunc, by = c("playerid", "Pos"))

# Still good
missing_fielders3 <- setdiff(inRedsWandJb$name_whole, fg_fielding$Name)


write_rds(fg_fielding, "data/10 - careerAdvancedFieldingStats.rds")
