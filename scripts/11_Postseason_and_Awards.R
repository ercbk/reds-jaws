# Postseason and Awards
# sections: Postseason (Per Season, Reds Tenure), Awards, Award Shares



library(tidyverse)
library(Lahman)


inRedsBatStats <- read_rds("data/10 - inRedsBatStats.rds")
inRedsPitStats <- read_rds("data/10 - inRedsPitStats.rds")

# Combined inductee and nominee lists

playerBatList <- list(inRedsBatStats$playerId)
playerBatList <- playerBatList[[1]]
yearBatList <- list(inRedsBatStats$yearId)
yearBatList <- yearBatList[[1]]

playerPitList <- list(inRedsPitStats$playerId)
playerPitList <- playerPitList[[1]]
yearPitList <- list(inRedsPitStats$yearId)
yearPitList <- yearPitList[[1]]


# Postseason ===================================================

# Per Season ======================


# Batting

postBat_data <- battingStats(data = Lahman::BattingPost)
filter_BatPost <- function(x, y) {
      filter(postBat_data, playerID == x & yearID == y)
}
inRedsBatPost <- map2_dfr(playerBatList, yearBatList, filter_BatPost) %>% filter(teamID == "CIN")


# Pitching

filter_PitPost <- function(x, y) {
      filter(PitchingPost, playerID == x & yearID == y)
}
inRedsPitPost <- map2_dfr(playerPitList, yearPitList, filter_PitPost) %>% 
      filter(teamID == "CIN")



# Reds Tenure =====================


# Batting

# Counting Stats

stat_Batlabels <- names(inRedsBatPost)
stat_Batlabels <- stat_Batlabels[-c(1,2,3,4,5,23,26,27,28,29)]

sum_Batfill <- function(x) {
      inRedsBatPost %>% 
            filter(playerID == x) %>% 
            select(stat_Batlabels) %>% 
            colSums() %>% 
            t() %>% 
            as.tibble()
}

postBatList <- unique(playerBatList)

sum_postBatStats <- map_dfr(postBatList, sum_Batfill) %>% 
      add_column(playerId = postBatList) %>% 
      select(playerId, everything())

# Adding in some of the traditional rate stats      
sum_postBatStats <- battingStats(data = sum_postBatStats)

write_rds(sum_postBatStats, "data/11 - careerRedsPostseasonBat.rds")


# Pitching

stat_Pitlabels <- names(inRedsPitPost)
stat_Pitlabels <- stat_Pitlabels[-c(1,2,3,4,5,19,20)]

sum_Pitfill <- function(x) {
      inRedsPitPost %>% 
            filter(playerID == x) %>% 
            select(stat_Pitlabels) %>% 
            colSums() %>% 
            t() %>% 
            as.tibble()
}

postPitList <- unique(playerPitList)

sum_postPitStats <- map_dfr(postPitList, sum_Pitfill) %>% 
      add_column(playerId = postPitList) %>% 
      select(playerId, everything()) %>% 
      mutate(IP = round(IPouts/3, 2),
             ERA = round((9*ER)/IP, 2),
             WHIP = round((H+BB)/IP, 2),
             BAopp = round(H/(BFP - BB - HBP - SH - SF), 3)
      )
             

write_rds(sum_postPitStats, "data/11 - careerRedsPostseasonPit.rds")



# Awards =====================================================

# Batting

filter_BatAwa <- function(x, y) {
      filter(AwardsPlayers, playerID == x & yearID == y)
}
inRedsBatAwa <- map2_dfr(playerBatList, yearBatList, filter_BatAwa) %>% 
      select(playerID, yearID, awardID)

# Number of times each player has won each award      
sum_BatAwa <- inRedsBatAwa %>% 
      select(playerID, awardID) %>% 
      group_by(playerID, awardID) %>% 
      summarize(number = n())

write_rds(sum_BatAwa, "data/11 - summaryBattingAwards.rds")


# Pitching

filter_PitAwa <- function(x, y) {
      filter(AwardsPlayers, playerID == x & yearID == y)
}
inRedsPitAwa <- map2_dfr(playerPitList, yearPitList, filter_PitAwa) %>% 
      select(playerID, yearID, awardID)

sum_PitAwa <- inRedsPitAwa %>% 
      select(playerID, awardID) %>% 
      group_by(playerID, awardID) %>% 
      summarize(number = n())

write_rds(sum_PitAwa, "data/11 - summaryPitchingAwards.rds")



# Award Shares =================================================

# Interested in the near misses since even placing 2nd or 3rd indicates a really good season in relation to peers

# Batting

# Contains ROY and MVP vote shares
filter_BatSha <- function(x, y) {
      filter(AwardsSharePlayers, playerID == x & yearID == y)
}
inRedsBatSha <- map2_dfr(playerBatList, yearBatList, filter_BatSha) %>% 
      mutate(vote_percentage = round((pointsWon/pointsMax)*100, 0))

mvp_roy_bat <- inRedsBatAwa %>% 
      filter(awardID == "Most Valuable Player" | awardID == "Rookie of the Year") %>% 
      select(playerID, yearID)

# This gets me the votes shares of awards that players didn't win
inRedsBatSha2 <- anti_join(inRedsBatSha, mvp_roy_bat, by = c("playerID", "yearID"))

# need to add rows back: 1968 Bench MVP, 1956 Robinson MVP
rows_to_add <- setdiff(inRedsBatSha, inRedsBatSha2) %>% 
      filter(yearID == "1968" & awardID == "MVP" | yearID == "1956" & awardID == "MVP")

# This contains the vote percentages/shares of awards players didn't win
inRedsBatSha2 <- inRedsBatSha2 %>% 
      bind_rows(rows_to_add)

write_rds(inRedsBatSha2, "data/11 - battingAwardsVoteShares.rds")




# Pitching

# Shares for CY, ROY, and MVP
filter_PitSha <- function(x, y) {
      filter(AwardsSharePlayers, playerID == x & yearID == y)
}
inRedsPitSha <- map2_dfr(playerPitList, yearPitList, filter_PitSha) %>% 
      mutate(vote_percentage = round((pointsWon/pointsMax)*100, 0))

mvp_roy_cy_pit <- inRedsPitAwa %>% 
      filter(awardID == "Most Valuable Player" | awardID == "Rookie of the Year" |awardID == "Cy Young Award") %>% 
      select(playerID, yearID)

# This gets me the vote percentages/shares of awards that players didn't win
inRedsPitSha2 <- anti_join(inRedsPitSha, mvp_roy_cy_pit, by = c("playerID", "yearID"))

write_rds(inRedsPitSha2, "data/11 - pitchingAwardsVoteShares.rds")
      

