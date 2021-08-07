# Table preparation for the Stat Rank pg
# Sections: Batting (Franchise Batting table, HOF Batting table, Rank Franchise/HOF Batting). Do it again for pitching



library(tidyverse)
# Don't think I used broom here
library(broom)
library(rlang)



# Reds HOF inductee and nominee war and jaws (career) stats
iWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds")
nWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")
inWandJp <- iWandJ %>% bind_rows(nWandJ) %>% filter(POS == "P")
inWandJb <- iWandJ %>% bind_rows(nWandJ) %>% filter(POS != "P")

# franchise advanced stats for entire Reds career
advFranBat <- read_rds("data/09 - franchiseAdvBatting.rds")
advFranPit <- read_rds("data/09 - advFranchisePitching.rds")

# franchise traditional stats for entire Reds career
tradFranBat <- read_rds("data/09 - franchiseTradBatting.rds")
tradFranPit <- read_rds("data/09 - tradFranchisePitching.rds")



# Display table ====================================

# All look good for display
fran_bat <- read_rds("data/13 - Franchise Batting.rds")
hof_bat <- read_rds("data/13 - HOF Batting.rds")
fran_pit <- read_rds("data/13 - Franchise Pitching.rds")
hof_pit <- read_rds("data/13 - HOF Pitching.rds")



# Batting ==================================================================

# Probably want 2 sets of tables with each set having two tables. 1 set with ranks and percentages for HOF and Franchise and the other with stats for HOF and Franchise. Then do it again for pitching.


# Create Franchise, HOF tables ===================

# Surprisingly enough bbref doesn't include WAR in their franchise players table but fangraphs does and I want it. Most of this is a replay of script 9 so I'm not going to re-comment the lines.


# Create Franchise Table =========================

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


franchise_batting <- franchise_batting[-54,] %>% 
      bind_rows(jr) %>%
      rename(Name = name_whole, fWAR = WAR) %>% 
      arrange(bbref_playerId) %>% 
      select(-fg_playerId, -Yrs, -From, -To, -CS)


# K%, BB% are char vars with % in the values
K_perc <- strsplit(franchise_batting$`K%`, " ", fixed = TRUE)
BB_perc <- strsplit(franchise_batting$`BB%`, " ", fixed = TRUE)

franchise_batting <- franchise_batting[-c(19,20)]

firstElt <- function(x) {x[1]}
franchise_batting$K_perc <- sapply(K_perc, firstElt)
franchise_batting$BB_perc <- sapply(BB_perc, firstElt)

franchise_batting <- franchise_batting %>% 
      mutate('K%' = as.numeric(K_perc), 'BB%' = as.numeric(BB_perc)) %>% 
      select(-K_perc, -BB_perc)

fb_withIds <- franchise_batting %>% 
      select(bbref_playerId:BB, `BB%`, SO, `K%`, everything())

franchise_batting <- franchise_batting %>% 
      select(bbref_playerId:BB, `BB%`, SO, `K%`, everything()) %>% 
      select(-bbref_playerId)

write_rds(franchise_batting, "data/13 - Franchise Batting.rds")
write_rds(fb_withIds, "data/13 - Franchise Batting wIds.rds")

# Create HOF table =========================


hof_batting <- map_dfr(inWandJb$playerId, function(x) {filter(fb_withIds, bbref_playerId == x)}) %>% 
      select(-bbref_playerId)
hb_withIds <- map_dfr(inWandJb$playerId, function(x) {filter(fb_withIds, bbref_playerId == x)})

# complete
missing_hof <- setdiff(hof_batting$bbref_playerId, inWandJb$playerId)
missing_wj <- setdiff(inWandJb$playerId, hof_batting$bbref_playerId)

write_rds(hof_batting, "data/13 - HOF Batting.rds")
write_rds(hb_withIds, "data/13 - HOF Batting wIds.rds")



# Rank Franchise, HOF Batting ======================

# Need to make ranking one function where you just enter the df but I don't feel like messing with it. Pitching has to be different because lower is better in some pitching stats.

# Franchise


fran_rank_perc_FUN <- function(x) {
      
      rank_var <- paste0("rank_", x)
      perc_var <- paste0("perc_", x)
      median_var <- paste0("median_", x)
      
      if(x %in% c("SO", "K%")) {
            fb_withIds %>%
                  mutate(!!rank_var := min_rank(!! rlang::sym(x)), !!perc_var := round(percent_rank(desc(!! rlang::sym(x))) * 100 + 1, 0), !!median_var := median(!! rlang::sym(x), na.rm = TRUE)) %>% 
                  select(rank_var, perc_var, median_var)
      } else {
            fb_withIds %>%
                  mutate(!!rank_var := min_rank(desc(!! rlang::sym(x))), !!perc_var := round(percent_rank(!! rlang::sym(x)) * 100, 0), !!median_var := median(!! rlang::sym(x), na.rm = TRUE)) %>% 
                  select(rank_var, perc_var, median_var)
      }
}

fran_id_cols <- fb_withIds %>% 
      select(bbref_playerId, Name)
fran_col_names <- names(fb_withIds)
fran_col_names <- fran_col_names[-c(1,2)]

fran_rank_perc <- map_dfc(fran_col_names, fran_rank_perc_FUN) %>% 
      bind_cols(fran_id_cols) %>% 
      select(bbref_playerId, Name, everything())

write_rds(fran_rank_perc, "data/13 - franchise batting ranks.rds")

fran_stats_rank <- fb_withIds %>% 
      inner_join(fran_rank_perc[-2], by = "bbref_playerId")
write_rds(fran_stats_rank, "data/13 - Franchise Batting Stats and Ranks.rds")


# HOF

hof_rank_perc_FUN <- function(x) {
      
      rank_var <- paste0("rank_", x)
      perc_var <- paste0("perc_", x)
      median_var <- paste0("median_", x)
      
      if(x == "SO") {
            hof_batting %>%
                  mutate(!!rank_var := min_rank(!! rlang::sym(x)), !!perc_var := round(percent_rank(desc(!! rlang::sym(x))) * 100, 0), !!median_var := median(!! rlang::sym(x), na.rm = TRUE)) %>% 
                  select(rank_var, perc_var, median_var)
      } else {
            hof_batting %>%
                  mutate(!!rank_var := min_rank(desc(!! rlang::sym(x))), !!perc_var := round(percent_rank(!! rlang::sym(x)) * 100, 0), !!median_var := median(!! rlang::sym(x), na.rm = TRUE)) %>% 
                  select(rank_var, perc_var, median_var)
      }
}

hof_id_cols <- hof_batting %>% 
      select(bbref_playerId, Name)

hof_col_names <- names(hof_batting)
hof_col_names <- hof_col_names[-c(1,2)]
hof_rank_perc <- map_dfc(hof_col_names, hof_rank_perc_FUN) %>% 
      bind_cols(hof_id_cols) %>% 
      select(bbref_playerId, Name, everything())
write_rds(hof_rank_perc, "data/13 - hof batting ranks.rds")

hof_stats_rank <- hof_batting %>% 
      inner_join(hof_rank_perc[-2], by = "bbref_playerId")
write_rds(hof_stats_rank, "data/13 - HOF Batting Stats and Ranks.rds")


# Pitching =================================================================

# Create Tables =============================

# Franchise Table

# Make sure everything starting out is kosher
missing_adv <- setdiff(inWandJp$playerId, advFranPit$bbref_id)
missing_trad <- setdiff(inWandJp$playerId, tradFranPit$bbref_id)

franchise_pitching <- tradFranPit %>% 
      inner_join(advFranPit[, -c(2,3,4,5,6)], by = "bbref_id") %>% 
      rename(Name = name_whole)
missing_fran <- setdiff(inWandJp$playerId, franchise_pitching$bbref_id)

# K%, BB%, and K-BB% are char vars with % in the values
K_perc <- strsplit(franchise_pitching$`K%`, " ", fixed = TRUE)
BB_perc <- strsplit(franchise_pitching$`BB%`, " ", fixed = TRUE)
K_BB_perc <- strsplit(franchise_pitching$`K-BB%`, " ", fixed = TRUE)
franchise_pitching <- franchise_pitching[-c(28,29,30)]
firstElt <- function(x) {x[1]}
franchise_pitching$K_perc <- sapply(K_perc, firstElt)
franchise_pitching$BB_perc <- sapply(BB_perc, firstElt)
franchise_pitching$K_BB_perc <- sapply(K_BB_perc, firstElt)

fp_withIds <- franchise_pitching %>% 
      mutate('K%' = as.numeric(K_perc), 'BB%' = as.numeric(BB_perc), 'K-BB%' = as.numeric(K_BB_perc)) %>% 
      select(-K_perc, -BB_perc, -K_BB_perc)

franchise_pitching <- franchise_pitching %>% 
      mutate('K%' = as.numeric(K_perc), 'BB%' = as.numeric(BB_perc), 'K-BB%' = as.numeric(K_BB_perc)) %>% 
      select(-K_perc, -BB_perc, -K_BB_perc, -bbref_id, -fg_id, -From, -To)



# Needed for HOF table which is needed for Numbers pg
write_rds(fp_withIds, "data/13 - Franchise Pitching wIds.rds")

write_rds(franchise_pitching, "data/13 - Franchise Pitching.rds")

# HOF Table

hp_withIds <- map_dfr(inWandJp$playerId, function(x) {filter(fp_withIds, bbref_id == x)})
hof_pitching <- hp_withIds %>% 
      select(-bbref_id, -fg_id, -From, -To)

missing_hof <- setdiff(hof_pitching$bbref_id, inWandJp$playerId)
missing_wj <- setdiff(inWandJp$playerId, hof_pitching$bbref_id)

# Needed for Numbers pg
write_rds(hp_withIds, "data/13 - HOF Pitching wIds.rds")

write_rds(hof_pitching, "data/13 - HOF Pitching.rds")



# Rank Franchise, HOF Pitching ===========================

# Franchise

franp_rank_perc_FUN <- function(x) {
      
      rank_var <- paste0("rank_", x)
      perc_var <- paste0("perc_", x)
      median_var <- paste0("median_", x)
      
      if(x %in% c("L", "ERA", "ERA-", "FIP-", "xFIP-", "WHIP", "SIERA", "BB/9", "HR/9", "BB%", "AVG")) {
            fp_withIds %>%
                  mutate(!!rank_var := min_rank(!! rlang::sym(x)), !!perc_var := round(percent_rank(desc(!! rlang::sym(x))) * 100, 0), !!median_var := median(!! rlang::sym(x), na.rm = TRUE)) %>% 
                  select(rank_var, perc_var)
      } else {
            fp_withIds %>%
                  mutate(!!rank_var := min_rank(desc(!! rlang::sym(x))), !!perc_var := round(percent_rank(!! rlang::sym(x)) * 100, 0), !!median_var := median(!! rlang::sym(x), na.rm = TRUE)) %>% 
                  select(rank_var, perc_var, median_var)
      }
}

franp_id_cols <- fp_withIds %>% 
      select(bbref_id, Name)

franp_col_names <- names(fp_withIds)
franp_col_names <- franp_col_names[-c(1,2,3,5,6)]
franp_rank_perc <- map_dfc(franp_col_names, franp_rank_perc_FUN) %>% 
      bind_cols(franp_id_cols) %>% 
      select(bbref_id, Name, everything())
write_rds(franp_rank_perc, "data/13 - franchise pitching ranks.rds")

franp_stats_rank <- fp_withIds %>% 
      inner_join(franp_rank_perc[-2], by = "bbref_id")
write_rds(franp_stats_rank, "data/13 - Franchise Pitching Stats and Ranks.rds")

# HOF

hofp_rank_perc_FUN <- function(x) {
      
      rank_var <- paste0("rank_", x)
      perc_var <- paste0("perc_", x)
      median_var <- paste0("median_", x)
      
      if(x %in% c("L", "ERA", "ERA-", "FIP-", "xFIP-", "WHIP", "SIERA", "BB/9", "HR/9", "BB%", "AVG")) {
            hp_withIds %>%
                  mutate(!!rank_var := min_rank(!! rlang::sym(x)), !!perc_var := round(percent_rank(desc(!! rlang::sym(x))) * 100, 0), !!median_var := median(!! rlang::sym(x), na.rm = TRUE)) %>% 
                  select(rank_var, perc_var)
      } else {
            hp_withIds %>%
                  mutate(!!rank_var := min_rank(desc(!! rlang::sym(x))), !!perc_var := round(percent_rank(!! rlang::sym(x)) * 100, 0), !!median_var := median(!! rlang::sym(x), na.rm = TRUE)) %>% 
                  select(rank_var, perc_var, median_var)
      }
}

hofp_id_cols <- hp_withIds %>% 
      select(bbref_id, Name)

hofp_col_names <- names(hp_withIds)
hofp_col_names <- hofp_col_names[-c(1,2,3,5,6)]
hofp_rank_perc <- map_dfc(hofp_col_names, hofp_rank_perc_FUN) %>% 
      bind_cols(hofp_id_cols) %>% 
      select(bbref_id, Name, everything())

write_rds(hofp_rank_perc, "data/13 - hof pitching ranks.rds")


hofp_stats_rank <- hp_withIds %>% 
      inner_join(hofp_rank_perc[-2], by = "bbref_id")
write_rds(hofp_stats_rank, "data/13 - HOF Pitching Stats and Ranks.rds")



