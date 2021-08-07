# Profile table prep
# Display table section at the bottom

library(tidyverse)
library(caret)

franchise_batting <- read_rds("data/13 - Franchise Batting.rds")
franchise_pitching <- read_rds("data/13 - Franchise Pitching.rds")

hof_batting <- read_rds("data/13 - HOF Batting.rds")
hof_pitching <- read_rds("data/13 - HOF Pitching.rds")



# Get only the numeric vars
fr_bat_num <- franchise_batting[-1]
fr_pit_num <- franchise_pitching[-1]
hof_bat_num <- hof_batting[-1]
hof_pit_num <- hof_pitching[-1]


# Reg standardization

mean_sd <- preProcess(fr_bat_num, method = c("center", "scale"))
mean_sd2 <- preProcess(fr_pit_num, method = c("center", "scale"))
mean_sd3 <- preProcess(hof_bat_num, method = c("center", "scale"))
mean_sd4 <- preProcess(hof_pit_num, method = c("center", "scale"))

rstd_fr_bat <- predict(mean_sd, fr_bat_num)
rstd_fr_pit <- predict(mean_sd2, fr_pit_num)
rstd_hof_bat <- predict(mean_sd3, hof_bat_num)
rstd_hof_pit <- predict(mean_sd4, hof_pit_num)



# Positional Normalization

# Other functions in various pkgs I found wouldn't deal with columns that had NAs. This will though.

mad_median_FUN <- function(df){
      df_nested <- df %>% 
            gather() %>%
            group_by(key) %>% 
            nest()
      
      pos_norm_FUN <- function(col) {
            col_n <- col[[1]]
            med <- median(col_n, na.rm = TRUE)
            mad <- mad(col_n, na.rm = TRUE)
            value <- map_dbl(col_n, function(x) {(x - med)/mad}) %>% 
                  t() %>% 
                  as.tibble()
      }
      
      # outputs matrix (couldn't get spread() to work)
      norm_matrix <- df_nested %>% 
            mutate(stats = map(data, pos_norm_FUN)) %>% 
            select(key, stats) %>%
            unnest() %>%
            t()
      
      # First row has column names but I need to get rid of it for now
      norm_pre <- norm_matrix[-1,]
      # Now add the column names and convert to tibble.
      colnames(norm_pre) <- names(df)
      norm_df <- as.tibble(norm_pre)
      norm_df_num <- norm_df %>% 
            mutate_if(is.character, as.numeric)
}

posn_fr_bat <- mad_median_FUN(fr_bat_num)
posn_fr_pit <- mad_median_FUN(fr_pit_num)
posn_hof_bat <- mad_median_FUN(hof_bat_num)
posn_hof_pit <- mad_median_FUN(hof_pit_num)


# add Name columns back
fin_rstd_fr_bat <- rstd_fr_bat %>%
      bind_cols(franchise_batting[1]) %>% 
      select(Name, everything())
fin_posn_fr_bat <- posn_fr_bat %>%
      bind_cols(franchise_batting[1]) %>% 
      select(Name, everything()) 
fin_rstd_hof_bat <- rstd_hof_bat %>%
      bind_cols(hof_batting[1]) %>% 
      select(Name, everything())
fin_posn_hof_bat <- posn_hof_bat %>%
      bind_cols(hof_batting[1:2]) %>% 
      select(Name, everything())

fin_rstd_fr_pit <-  rstd_fr_pit %>%
      bind_cols(franchise_pitching[1]) %>% 
      select(Name, everything()) 
fin_posn_fr_pit <-  posn_fr_pit %>%
      bind_cols(franchise_pitching[1]) %>% 
      select(Name, everything()) 
fin_rstd_hof_pit <- rstd_hof_pit %>%
      bind_cols(hof_pitching[1]) %>% 
      select(Name, everything()) 
fin_posn_hof_pit <- posn_hof_pit %>%
      bind_cols(hof_pitching[1]) %>% 
      select(Name, everything()) 


bat_sign_FUN <- function(df) {
      col_df <- as.character(names(df[-c(1,2)]))
      df_g <- df %>% 
            gather(col_df, key = "stat", value = "score") %>% 
            mutate(score = if_else(stat == "K%", score * -1, score), score = if_else(stat == "SO", score * -1, score)) 
      df_g$sign <- factor(ifelse(df_g$score < 0, "negative", "positive"), levels = c("negative", "positive"))
      return(df_g)
}

final_rfb <- bat_sign_FUN(fin_rstd_fr_bat)
final_rhb <- bat_sign_FUN(fin_rstd_hof_bat)
final_pfb <- bat_sign_FUN(fin_posn_fr_bat)
final_phb <- bat_sign_FUN(fin_posn_hof_bat)


pit_sign_FUN <- function(df) {
      col_df <- as.character(names(df[-1]))
      df_g <- df %>% 
            gather(col_df, key = "stat", value = "score") %>% 
            mutate(score = if_else(stat %in% c("L", "ERA", "ERA-", "FIP-", "xFIP-", "SIERA", "WHIP", "BB/9", "HR/9", "AVG", "BB%"), score * -1, score)) 
      df_g$sign <- factor(ifelse(df_g$score < 0, "negative", "positive"), levels = c("negative", "positive"))
      return(df_g)
}

final_rfp <- pit_sign_FUN(fin_rstd_fr_pit)
final_rhp <- pit_sign_FUN(fin_rstd_hof_pit)
final_pfp <- pit_sign_FUN(fin_posn_fr_pit)
final_php <- pit_sign_FUN(fin_posn_hof_pit)

write_rds(final_rfb, "data/14 - reg standardization fran batting.rds")
write_rds(final_rfp, "data/14 - reg standardization fran pitching.rds")
write_rds(final_rhb, "data/14 - reg standardization hof batting.rds")
write_rds(final_rhp, "data/14 - reg standardization hof pitching.rds")

write_rds(final_pfb, "data/14 - pos normalization fran batting.rds")
write_rds(final_pfp, "data/14 - pos normalization fran pitching.rds")
write_rds(final_phb, "data/14 - pos normalization hof batting.rds")
write_rds(final_php, "data/14 - pos normalization hof pitching.rds")



# Display Tables ============================================

# Depending on how it looks, I might include season, shares
seas_bat <- read_rds("data/20 - Numbers pg HOF seas Batting.rds") %>% 
      select(-`BBRef Id`, -`FG Id`)
seas_pit <- read_rds("data/20 - Numbers pg HOF seas Pitching.rds") %>% 
      select(-`BBRef Id`, -`FG Id`)
# Maybe double column this with awards
shares <- read_rds("data/20 - Numbers pg HOF Award Shares.rds") %>% 
      select(-`BBRef Id`, -`FG Id`)

# Batting tab
batting <- read_rds("data/13 - HOF Batting.rds")

fielding <- read_rds("data/20 - Numbers pg HOF Fielding.rds") %>% 
      select(-`BBRef Id`, -`FG Id`) %>% 
      select(Name, everything())
write_rds(fielding, "data/14 - Fielding display table.rds")

postseas_b <- read_rds("data/20 - Numbers pg HOF Postseason Batting.rds") %>% 
      select(-`BBRef Id`, -`FG Id`)
write_rds(postseas_b, "data/14 - Postseason batting display table.rds")

awards <- read_rds("data/20 - Numbers pg HOF Awards.rds") %>%
      select(-`BBRef Id`, -`FG Id`)
write_rds(awards, "data/14 - Awards display table.rds")

# Pitching tab
pitching <- read_rds("data/13 - HOF Pitching.rds")
postseas_p <- read_rds("data/20 - Numbers pg HOF Postseason Pitching.rds") %>% 
      select(-`BBRef Id`, -`FG Id`)
write_rds(postseas_p, "data/14 - Postseason pitching display table.rds")
