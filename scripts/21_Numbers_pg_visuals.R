# Numbers Page Visuals


library(tidyverse)
library(formattable)

# Only going to use pitching and batting in shiny but thought add link to the rest just because.

award_shares <- read_rds("data/20 - Numbers pg HOF Award Shares.rds")
awards <- read_rds("data/20 - Numbers pg HOF Awards.rds")
batting <- read_rds("data/20 - Numbers pg HOF Batting.rds")
fielding <- read_rds("data/20 - Numbers pg wIds HOF Fielding.rds")
pitching <- read_rds("data/20 - Numbers pg HOF Pitching.rds")
ps_bat <- read_rds("data/20 - Numbers pg wIds HOF Postseason Batting.rds")
ps_pit <- read_rds("data/20 - Numbers pg wIds HOF Postseason Pitching.rds")
seas_bat <- read_rds("data/20 - Numbers pg wIds HOF seas Batting.rds")
seas_pit <- read_rds("data/20 - Numbers pg wIds HOF seas Pitching.rds")


# Splitting into pitcher/position player
awards_shares_b_noId <- map_dfr(batting$Name, function(x) {
      filter(award_shares, Name == x)
}) %>% 
      select(-`BBRef Id`, -`FG Id`)

awards_shares_p_noId <- map_dfr(pitching$Name, function(x) {
      filter(award_shares, Name == x)
}) %>% 
      select(-`BBRef Id`, -`FG Id`)

awards_b_noId <- map_dfr(batting$Name, function(x) {
      filter(awards, Name ==x)
}) %>% 
      select(-`BBRef Id`, -`FG Id`)
awards_p_noId <- map_dfr(pitching$Name, function(x) {
      filter(awards, Name ==x) 
}) %>% 
      select(-`BBRef Id`, -`FG Id`)


write_rds(awards_shares_b_noId, "data/21 - Numbers pg HOF Batting Awards Shares.rds")
write_rds(awards_shares_p_noId, "data/21 - Numbers pg HOF Pitching Awards Shares.rds")
write_rds(awards_b_noId, "data/21 - Numbers pg HOF Batting Awards.rds")
write_rds(awards_p_noId, "data/21 - Numbers pg HOF Pitching Awards.rds")

# Examples of addresses used by bbref and fg

# Eric Davis, bbref
# https://www.baseball-reference.com/players/d/daviser01.shtml
# Jose Rijo, bbref
# https://www.baseball-reference.com/players/r/rijojo01.shtml
# Johnny Bench, bbref
# https://www.baseball-reference.com/players/b/benchjo01.shtml


# Thank gawd there some kind of autocomplete for web addresses so I can disregard the bit after the playerids and don't have to match my player primary positions to that of FG's in order to get the web pages to load.
# Eric Davis, fg
# https://www.fangraphs.com/statss.aspx?playerid=1003048&position=OF
# Jose Rijo, fg
# https://www.fangraphs.com/statss.aspx?playerid=349&position=P
# Johnny Bench, fg
# https://www.fangraphs.com/statss.aspx?playerid=1000826&position=C






link_FUN <- function(df) {
      
      # bbref has a prefix in their address: the first letter of playerid
      string1 <- str_extract_all(df$`BBRef Id`, "^[a-z]")
      string2 <- paste0(string1, "/")
      thing <- df %>% 
            add_column(b_prefix = string2) %>% 
            select(b_prefix, everything())
      
      # create url columns, formattable does its thing (whatever that is) and creates obj for as.datatable.
      # the part in shQuote is the actual web address. The rest is html magic I know nothing about.
      obj <- thing %>% 
            mutate(url_bbref = paste0("<a href = ", shQuote(paste0("https://www.baseball-reference.com/players/", b_prefix, `BBRef Id`, ".shtml")),
                                      "target='_blank'>",
                                      `BBRef Id`,
                                      "</a>"),
                   url_fg = paste0("<a href = ", shQuote(paste0("https://www.fangraphs.com/statss.aspx?playerid=", `FG Id`)),
                                   "target='_blank'>",
                                   `FG Id`,
                                   "</a>")) %>%
            select(-`BBRef Id`, -`FG Id`, -b_prefix) %>%
            rename(`BBRef Id` = url_bbref, `FG Id` = url_fg) %>% 
            select(Name, `BBRef Id`, `FG Id`, everything()) %>%
            # as.datatable() included in orginal code as the last line. It's part of the formattable pkg that converts the df that formattable() creates into a DT table. I used it in the shiny server.R file.
            formattable()
      
}

batting_num <- link_FUN(batting)
awards_shares_b_num <- link_FUN(awards_shares_b)
awards_shares_p_num <- link_FUN(awards_shares_p)
awards_b_num <- link_FUN(awards_b)
awards_p_num <- link_FUN(awards_b)
fielding_num <- link_FUN(fielding)
pitching_num <- link_FUN(pitching)
ps_bat_num <- link_FUN(ps_bat)
ps_pit_num <- link_FUN(ps_pit)
seas_bat_num <- link_FUN(seas_bat)
seas_pit_num <- link_FUN(seas_pit)





write_rds(batting_num, "data/21 - Numbers pg HOF Batting.rds")
write_rds(awards_shares_b_num, "data/21 - Numbers pg wLinks HOF Batting Awards Shares.rds")
write_rds(awards_shares_p_num, "data/21 - Numbers pg wLinks HOF Pitching Awards Shares.rds")
write_rds(awards_b_num, "data/21 - Numbers pg wLinks HOF Batting Awards.rds")
write_rds(awards_p_num, "data/21 - Numbers pg wLinks HOF Pitching Awards.rds")
write_rds(fielding_num, "data/21 - Numbers pg HOF Fielding.rds")
write_rds(pitching_num, "data/21 - Numbers pg HOF Pitching.rds")
write_rds(ps_bat_num, "data/21 - Numbers pg HOF Postseason Batting.rds")
write_rds(ps_pit_num, "data/21 - Numbers pg HOF Postseason Pitching.rds")
write_rds(seas_bat_num, "data/21 - Numbers pg HOF Season Batting.rds")
write_rds(seas_pit_num, "data/21 - Numbers pg HOF Season Pitching.rds")
