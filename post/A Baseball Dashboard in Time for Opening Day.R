library(tidyverse)
library(rvest)
library(openWARData)
library(Lahman)

# Objects to be saved part one: warDat, indWar, nomWar, posDat


# ============= Scrape =====================

url <- "https://en.wikipedia.org/wiki/Cincinnati_Reds_Hall_of_Fame_and_Museum#Cincinnati_Reds_Hall_of_Fame_members"

members <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
      html_table()
members <- members[[1]]

members <- members %>% 
      filter(Inductee != "Adam Dunn" & Inductee != "Fred Norman" & Inductee != "Dave Bristol")


# ============= Get IDs =====================

idTTa <- idTT %>%
      select(key_bbref, name_last, name_first) %>%
      mutate(name_whole = paste(name_first, name_last))

indID <- map_dfr(members[,"Inductee"], function(x) {
      filter(idTTa, name_whole == x & key_bbref != "")})

missNamList <- list("Dolf Luque", "Leo Cardenas", "Tony Perez", "Dave Concepcion", "Ken Griffey",
                    "Jose Rijo", "Cesar Geronimo", "Pedro Borbon")
indID <- map_dfr(missNamList, function(x) {
      filter(idTTa, name_whole == x & key_bbref != "")}) %>% 
      bind_rows(indID) %>% 
      mutate(name_whole = if_else(key_bbref == "griffke02", "Ken Griffey Jr", name_whole))

indID <- filter(indID, key_bbref != "rosepe02" & key_bbref != "morgajo01"
                & key_bbref != "mccormi03" & key_bbref != "andersp01" 
                & key_bbref != "wrighge03" & key_bbref != "hutchfr01"
                & key_bbref != "borbope02"    
)

nomNamList <- list("Aaron Boone", "Adam Dunn", "John Franco", "Danny Graves", "Scott Rolen",
                   "Reggie Sanders")
nomID <- map_dfr(nomNamList, function(x) {
      filter(idTTa, name_whole == x & key_bbref != "")})

nomID <- filter(nomID, key_bbref != "sandere01")


# ============= Get WAR ====================

indWar <- map_dfr(as.character(indID$key_bbref), function(x) {
      filter(rWAR, playerId == x)}) %>%
      select(playerId, yearId, teamId, rWAR) %>%
      mutate_if(is.factor, as.character) %>% 
      filter(teamId == "CIN")

indWar <- indID %>% 
      select(name_whole, key_bbref) %>% 
      rename(Name = name_whole, playerId = key_bbref) %>% 
      inner_join(indWar, by = 'playerId')

nomWar <- map_dfr(as.character(nomID$key_bbref), function(x) {
      filter(rWAR, playerId == x)}) %>%
      select(playerId, yearId, teamId, rWAR) %>%
      mutate_if(is.factor, as.character) %>% 
      filter(teamId == "CIN")

nomWar <- nomID %>% 
      select(name_whole, key_bbref) %>% 
      rename(Name = name_whole, playerId = key_bbref) %>% 
      inner_join(nomWar, by = 'playerId')

indWar <- filter(indWar, playerId != "grangwa01" & playerId != "mckecbi01"
                 & playerId != "werbebi01" & playerId != "wrighge01"
                 & playerId != "wrighha01")

warDat <- indWar %>% 
      bind_rows(nomWar)


# ============= Positions ====================

posDat <- map2_dfr(warDat$playerId, warDat$yearId, function(x,y) {
      filter(Fielding, playerID == x & yearID == y)}) %>% 
      filter(teamID == "CIN" | teamID == "CN1" | teamID == "CN2")

posDat <- posDat %>%
      select(playerID, POS, G) %>% 
      group_by(playerID, POS) %>% 
      summarize(sumG = sum(G)) %>% 
      filter(sumG == max(sumG)) %>% 
      ungroup() %>% 
      select(playerID, POS)

posDat <- posDat %>% 
      add_row(playerID = "o'tooji01", POS = "P") %>% 
      rename(playerId = playerID)


# ============= OF Split ====================

ofDat <- posDat %>% 
      filter(POS == "OF")

ofYears <- map_dfr(ofDat$playerId, function(x) {
      filter(warDat, playerId == x)
})

ofSplit <- map2_dfr(ofYears$playerId, ofYears$yearId, function(x,y) {
      filter(Appearances, playerID == x & yearID == y)}) %>% 
      rename(LF = G_lf, CF = G_cf, RF = G_rf) %>% 
      gather('LF', 'CF', 'RF', key = "POS", value = "G")

splitSum <- ofSplit %>% 
      select(playerID, POS, G) %>%
      rename(playerId = playerID) %>%
      group_by(playerId, POS) %>% 
      summarize(sumG = sum(G)) %>% 
      filter(sumG == max(sumG)) %>% 
      ungroup() %>% 
      select(playerId, POS)

ofPos <- posDat %>% 
      filter(POS == "OF") %>% 
      select(-POS) %>% 
      inner_join(splitSum, by = "playerId")

posDat <- posDat %>% 
      filter(POS != "OF") %>%
      bind_rows(ofPos)

warDat <- warDat %>% 
      inner_join(posDat, by = 'playerId')


# ============= JAWS Calculation =================

# total WAR during Reds tenure
warSum <- warDat %>%
      group_by(playerId) %>%
      summarize(WARtenure = sum(rWAR)) %>% 
      ungroup()

# Sum of top 4 WAR years
war4Dat <- warDat %>%
      group_by(playerId) %>%
      top_n(4, rWAR) %>%
      tally(rWAR) %>%
      rename(WAR4 = n)

# Calculating JAWS
warJaws <- warSum %>% 
      inner_join(war4Dat, by = 'playerId') %>% 
      mutate(JAWS4 = round((WARtenure + WAR4)/2, 2)) %>% 
      select(playerId, WARtenure, WAR4, JAWS4)

# Add Names and Positions
names <- warDat %>% 
      select(playerId, Name) %>% 
      distinct()

warJaws <- warJaws %>%
      inner_join(posDat, by = 'playerId') %>% 
      inner_join(names, by = 'playerId') %>% 
      select(playerId, Name, POS, everything())


# ============= Weighted Distribution ==================

# inductees
indJaws <- warJaws %>% 
      anti_join(nomWar, by = 'playerId')

batJaws <- indJaws %>%
      select(-playerId) %>% 
      filter(POS != "P")

# 1B and CF are highest with 10 members a piece so they won't need filler players
table(batJaws$POS)

# Number of filler players needed at each position
neededPOS <- batJaws %>%
      group_by(POS) %>%
      summarize(n = n()) %>% 
      mutate(remPOS = max(n) - n) %>%
      filter(POS != "1B", POS != "CF") %>%
      select(-n)

# List of lists with filler position values
posLL <- map2(neededPOS$POS, neededPOS$remPOS, function(POS, n) {
      POS <- rep(POS, n)
})

# Create tibble with all the filler players for each position

# Empty tibble
posFillTib <- tibble(
      Name = character(),
      POS = character(),
      WARtenure = numeric(),
      WAR4 = numeric(),
      JAWS4 = numeric()
      
)

# input: Position; function creates one filler player with avgHOF stats
fillPOS <- function(POS) {
      posFillTib <- posFillTib %>%
            add_row(Name = "avgHOFplayer",
                    POS = POS,
                    WARtenure = median(batJaws$WARtenure),
                    WAR4 = median(batJaws$WAR4),
                    JAWS4 = median(batJaws$JAWS4)
                    
            )
}
# List of lists fed to function; outputs tibble of filler players
fillerPlayers <- map_dfr(posLL, fillPOS)

# Creating weighted distribution of position players
wtBatDistr <- batJaws %>%
      bind_rows(fillerPlayers)

# Calculate weighted averages at each position
wbd_nested <- wtBatDistr %>% 
      group_by(POS) %>% 
      nest()

wt_avg_FUN <- function(df) {
      mutate(df, `Wt Avg WAR` = round(mean(WARtenure), 1), `Wt Avg WAR4` = round(mean(WAR4), 1), `Wt Avg JAWS4` = round(mean(JAWS4), 1))
}

wbd_avgs <- wbd_nested %>% 
      mutate(stats = map(data, wt_avg_FUN)) %>% 
      select(POS, stats) %>% 
      unnest() %>% 
      select(Name, POS, everything()) %>% 
      filter(Name != "avgHOFplayer")

# Add nominees

nomBatJaws <- warJaws %>% 
      anti_join(indWar, by = 'playerId') %>% 
      filter(POS != "P") %>% 
      select(-playerId)

# Sync averages to nominee positions and combine with inductee averages df
wtBatJaws <- nomBatJaws %>% 
      mutate(`Wt Avg WAR` = plyr::mapvalues(POS, from = wbd_avgs$POS, to = wbd_avgs$`Wt Avg WAR`) %>% as.numeric(), `Wt Avg WAR4` = plyr::mapvalues(POS, from = wbd_avgs$POS, to = wbd_avgs$`Wt Avg WAR4`) %>% as.numeric(), `Wt Avg JAWS4` = plyr::mapvalues(POS, from = wbd_avgs$POS, to = wbd_avgs$`Wt Avg JAWS4`) %>% as.numeric()) %>% 
      bind_rows(wbd_avgs)


# Gotta get the pitchers
pitJaws <- warJaws %>% 
      anti_join(nomWar, by = 'playerId') %>% 
      select(-playerId) %>% 
      filter(POS == "P") %>%
      mutate(`Wt Avg WAR` = round(mean(WARtenure), 1), `Wt Avg WAR4` = round(mean(WAR4), 1), `Wt Avg JAWS4` = round(mean(JAWS4), 1))

# Add Nominees
nomPitJaws <- warJaws %>% 
      anti_join(indWar, by = 'playerId') %>% 
      filter(POS == "P") %>% 
      select(-playerId)

# Sync (Not actually weighted)
wtPitJaws <- nomPitJaws %>% 
      mutate(`Wt Avg WAR` = plyr::mapvalues(POS, from = pitJaws$POS, to = pitJaws$`Wt Avg WAR`) %>% as.numeric(), `Wt Avg WAR4` = plyr::mapvalues(POS, from = pitJaws$POS, to = pitJaws$`Wt Avg WAR4`) %>% as.numeric(), `Wt Avg JAWS4` = plyr::mapvalues(POS, from = pitJaws$POS, to = pitJaws$`Wt Avg JAWS4`) %>% as.numeric()) %>% 
      bind_rows(pitJaws)


display_table <- wtBatJaws %>% 
      bind_rows(wtPitJaws) %>% 
      arrange(Name)

write_rds(display_table, "post/shiny/data/display_table.rds")


# ============= Other Groups (Cleveland Dot Charts) ===================

# Build df with group positions
cornerIF <- warJaws %>% 
      filter(POS == "1B" | POS == "3B") %>%
      mutate(POS = plyr::mapvalues(POS, from = c("1B", "3B"), to = c("CI", "CI")))

middleIF <- warJaws %>% 
      filter(POS == "2B" | POS == "SS") %>%
      mutate(POS = plyr::mapvalues(POS, from = c("2B", "SS"), to = c("MI", "MI")))

outField <- warJaws %>% 
      filter(POS == "LF" | POS == "CF" | POS == "RF") %>%
      mutate(POS = plyr::mapvalues(POS, from = c("LF", "CF", "RF"), to = c("OF", "OF", "OF")))

corners <- warJaws %>% 
      filter(POS == "1B" | POS == "3B" | POS == "LF" | POS == "RF") %>% 
      mutate(POS = plyr::mapvalues(POS, from = c("1B", "LF", "RF", "3B"), to = c("CO", "CO", "CO", "CO")))

middle <- warJaws %>% 
      filter(POS == "2B" | POS == "SS" | POS == "C" | POS == "CF") %>% 
      mutate(POS = plyr::mapvalues(POS, from = c("2B", "SS", "C", "CF"), to = c("Md", "Md", "Md", "Md")))

other_groups <- cornerIF %>% 
      bind_rows(middleIF, outField, corners, middle)


# Calculate averages of each group

other_groups_i <- other_groups %>% 
      anti_join(nomWar, by = 'playerId')

og_nested <- other_groups_i %>% 
      group_by(POS) %>% 
      nest()

avg_FUN <- function(df) {
      mutate(df, WAR_avg = round(mean(WARtenure), 1), WAR4_avg = round(mean(WAR4), 1), JAWS_avg = round(mean(JAWS4), 1))
}

group_avgs_i <- og_nested %>% 
      mutate(stats = map(data, avg_FUN)) %>% 
      select(POS, stats) %>% 
      unnest() %>% 
      select(playerId, Name, POS, everything())

# Add Nominees

other_groups_n <- other_groups %>% 
      anti_join(indWar, by = 'playerId')

group_avgs <- other_groups_n %>% 
      mutate(WAR_avg = plyr::mapvalues(POS, from = group_avgs_i$POS, to = group_avgs_i$WAR_avg) %>% as.numeric(), WAR4_avg = plyr::mapvalues(POS, from = group_avgs_i$POS, to = group_avgs_i$WAR4_avg) %>% as.numeric(), JAWS_avg = plyr::mapvalues(POS, from = group_avgs_i$POS, to = group_avgs_i$JAWS_avg) %>% as.numeric()) %>% 
      bind_rows(group_avgs_i)

# Prepare dataframe for JAWS dot chart
dot_table <- display_table %>% 
      rename(JAWS_avg = `Wt Avg JAWS4`, WAR_avg = `Wt Avg WAR`) %>% 
      bind_rows(group_avgs)

jaws_group <- dot_table %>% 
      select(Name, POS, JAWS4, JAWS_avg) %>% 
      rename(Group = POS, `Avg HOF` = JAWS_avg) %>% 
      gather(key = "Stat", value = "Value", -c(Name, Group))

# Prepare dataframe for WAR dot chart
war_group <- dot_table %>% 
      select(Name, POS, WARtenure, WAR_avg) %>% 
      rename(Group = POS, `Avg HOF` = WAR_avg, WAR = WARtenure) %>% 
      gather(key = "Stat", value = "Value", -c(Name, Group))

write_rds(jaws_group, "post/shiny/data/jaws_dot.rds")
write_rds(war_group, "post/shiny/data/war_dot.rds")


# ============= Line Chart Data ====================

# WAR4 + years; add type column
war4Dat <- warDat %>%
      group_by(playerId) %>%
      top_n(4, rWAR) %>% 
      ungroup() %>% 
      select(-teamId) %>% 
      add_column(type = rep("WAR4", 328))

# Not WAR4 + years; add type column
notWar4 <- warDat %>% 
      anti_join(war4Dat, by = c("playerId", "yearId")) %>% 
      select(-teamId) %>% 
      add_column(type = rep("WAR", 427))

war_combined <- notWar4 %>% 
      bind_rows(war4Dat)


#  Positional seasonal average WAR values

pitMedWar <- war_combined %>% 
      filter(POS == "P") %>% 
      summarize(`Median Pitcher WAR` = median(rWAR))

posMedWAR <- war_combined %>% 
      filter(POS != "P") %>% 
      summarize(`Median Position WAR` = median(rWAR))

war_combo_avg <- war_combined %>% 
      mutate(`Median WAR` = if_else(POS == "P", pitMedWar$`Median Pitcher WAR`[[1]], posMedWAR$`Median Position WAR`[[1]])) %>% 
      rename(bbref_id = playerId, WAR = rWAR) %>% 
      select(bbref_id, Name, everything())

write_rds(war_combo_avg, "post/shiny/data/line_chart.rds")


