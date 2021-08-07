# JAWS splash visual


# Ended up using DT tables
library(tidyverse)
library(knitr)
library(kableExtra)


# Ridge Plot Table ==============================================

bothDistrb <- read_rds("data/unweightedandGroupJoyplot.rds")


# Kable Table Creation ==========================================

# Group averages
other_summary <- read_rds("data/07b - otherGroupSummary.rds") %>% 
      rename(WARtenure = redsWAR, WAR4 = redsWAR4, JAWS4 = redsJAWS, Position = Group, Number = number) %>%
      mutate(WARtenure = round(WARtenure, 1), WAR4 = round(WAR4, 1), JAWS4 = round(JAWS4, 1)) %>%
      mutate(Position = case_when(Position == "CI" ~ "CI (1B,3B)", Position == "MI" ~ "MI (2B,SS)", Position == "OF" ~ "OF (LF,CF,RF)", Position == "CO" ~ "CO (1B,3B,LF,RF)", Position == "Md" ~ "Md (C,2B,SS,CF)")) %>%
      select(Position, everything())


# Weighted position + pitcher averages
wtHofWJ <- read_rds("data/16 - Weighted Average HOF WAR and JAWS.rds") %>% 
      rename(Position = POS, wtWARtenure = wtWAR_avg, wtWAR4 = wtWAR4_avg, wtJAWS4 = wtJAWS_avg) %>%
      select(-number)


row_order <- c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF")

# Combining tables; reordering positions because I'm going to do some row grouping with kable
jaws_ref_table <- read_rds("data/16 - Average HOF WAR and JAWS.rds") %>% 
      rename(Position = POS, Number = number, WARtenure = WAR_avg, WAR4 = WAR4_avg, JAWS4 = JAWS_avg) %>% 
      full_join(wtHofWJ, by = "Position") %>% 
      slice(match(row_order, Position)) %>% 
      bind_rows(other_summary)
      
write_rds(jaws_ref_table, "data/17 - JAWS Reference Table.rds")



# kable table =================================================

jaws_ref_table <- read_rds("data/17 - JAWS Reference Table.rds")

kable_jaws_tbl <- kable(jaws_ref_table, "html") %>% 
      kable_styling() %>% 
      add_header_above(c(" " = 1, "Unweighted with Typical HOFers" = 4, "Weighted with Typical HOFers" = 3)) %>% 
      group_rows("Pos Players", 2, 9, label_row_css = "background-color: #C6011F; color: #fff;") %>% 
      group_rows("Other Groups", 10, 14, label_row_css = "background-color: #C6011F; color: #fff;")

kable_jaws_tbl
