# 16 - JAWS tables prep

# The two tables calc'd below and the groupSummary table should be enough to recreate the bbref JAWS table and create the cleveland dot plots and line plots for JAWS page


library(tidyverse)

iWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds")
nWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")
inWandJ <- iWandJ %>% 
      bind_rows(nWandJ)

groupWandJ <- read_rds("data/08 - unweightedandGroupJoyplot.rds")
wtWandJ <- read_rds("data/08 - weightedPositionJoyPlot.rds")
groupSummary <- read_rds("data/07b - otherGroupSummary.rds")

# Positional non-weighted averages
# Will alsoe be used for Pitcher averages on JAWS page since they're unweighted
avgPOS_FUN <- function(x) {
      
      n_vector <- iWandJ %>% 
            filter(POS == x) %>% 
            summarize(n = n())
      
      n <- n_vector[[1]]
      
      df <- iWandJ %>% 
            filter(POS == x) %>% 
            summarize(WAR_avg = round(mean(redsWAR), 1), WAR4_avg = round(mean(redsWAR4), 1), JAWS_avg = round(mean(redsJAWS), 1), n = n()) %>% 
            add_column(POS = x) %>% 
            add_column(number = n)
      
      return(df)      
}

avgHofPos <- map_dfr(unique(iWandJ$POS), avgPOS_FUN) %>%
      select(POS, number, everything(), -n)


# Calculating weighted averages for positional players
wtAvgPOS_FUN <- function(x) {
      
      n_vector <- wtWandJ %>% 
            filter(POS == x) %>% 
            summarize(n = n())
      n <- n_vector[[1]]
      
      df <- wtWandJ %>% 
            filter(POS == x) %>% 
            summarize(wtWAR_avg = round(mean(redsWAR), 1), wtWAR4_avg = round(mean(redsWAR4), 1), wtJAWS_avg = round(mean(redsJAWS), 1), n = n()) %>% 
            add_column(POS = x) %>% 
            add_column(number = n)
      
      return(df)      
}

wtAvgHofPos <- map_dfr(unique(wtWandJ$POS), wtAvgPOS_FUN) %>%
      select(POS, number, everything(), -n) %>% 
      filter(POS != "P")


write_rds(avgHofPos, "data/16 - Average HOF WAR and JAWS.rds")
write_rds(wtAvgHofPos, "data/16 - Weighted Average HOF WAR and JAWS.rds")
