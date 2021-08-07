# Positional Joy Plots
# sections: weighted position + pitcher, unweighted position + pitcher, group, unweighted + group (with/without jittered points)

library(tidyverse)
library(ggridges)
library(viridis)
library(cowplot)
library(ggbeeswarm)


# Combining Pitcher and weighted position player distributions
batPosDistrbWt <- read_rds("data/07a - weightedPositionDistributions.rds")
pitPosDistrb <- read_rds("data/07a - pitcherPositionDistribution.rds") %>% 
      select(name_whole, redsWAR, redsWAR4, redsJAWS, POS)

wtCompPosDistrb <- bind_rows(batPosDistrbWt,pitPosDistrb)
wtCompPosDistrb$POSf <- as.factor(compPosDistrb$POS)

write_rds(wtCompPosDistrb, "data/08 - weightedPositionJoyPlot.rds")



# Weighted Position + Pitcher Distributions ===============


# Some variation.
wtJoyPOS <- ggplot(data = wtCompPosDistrb,
                 aes(y = forcats::fct_relevel(POSf, "RF", "CF", "LF", "SS", "3B", "2B", "1B", "C", "P"),
                     x = redsJAWS,
                     color = redsJAWS,
                     fill = ..x..)) +
      geom_density_ridges_gradient(rel_min_height = 0.01,
                                   alpha = 0.75)  +
      scale_fill_viridis(option = "C",
                         name = "Reds JAWS") +
      scale_color_viridis(option = "C") +
      guides(color = F) +
      geom_quasirandom(color = "white",
                       alpha = 0.5,
                       shape = 21,
                       size = 1.1,
                       groupOnX = F) +
      theme_ridges() +
      theme(legend.position = "top",
            plot.caption = element_text(hjust=0),
            legend.key.width = unit(1.25, "cm")) +
      labs(x = "Reds JAWS", y = "Position",
           title = "Weighted Reds HOF JAWS Distributions")

wtJoyPOS


# Unweighted =====================================


# Removing filler players
compPosDistrb <- wtCompPosDistrb %>% 
      filter(name_whole != "avgHOFplayer")

write_rds(compPosDistrb, "data/08 - positionJoyPlot.rds")


# 3B only has 2 players so no density plot
# Also removed jittered points. Usually in favor but I think it possibly subtracts from the visual here.
joyPOS <- ggplot(data = compPosDistrb,
                   aes(y = forcats::fct_relevel(POSf, "RF", "CF", "LF", "SS", "3B", "2B", "1B", "C", "P"),
                       x = redsJAWS,
                       color = redsJAWS,
                       fill = ..x..)) +
      geom_density_ridges_gradient(rel_min_height = 0.01,
                                   alpha = 0.75)  +
      scale_fill_viridis(option = "C",
                         name = "Reds JAWS") +
      scale_color_viridis(option = "C") +
      guides(color = F) +
      theme_ridges() +
      theme(legend.position = "top",
            plot.caption = element_text(hjust=0),
            legend.key.width = unit(1.25, "cm")) +
      labs(x = "Reds JAWS", y = "Position",
           title = "Reds HOF JAWS Distributions")

joyPOS



# Groups ==============================================

otherGroups <- read_rds("data/07b - otherGroupDistributions.rds")

# Adding HOF distribution
compPosDistrbA <- compPosDistrb %>% 
      select(-POS, -POSf) %>% 
      add_column(POS = rep("Reds HOF", nrow(compPosDistrb)))

otherGroupsA <- otherGroups %>% 
      select(name_whole, redsWAR, redsWAR4, redsJAWS, POS)

groupDistrb <- bind_rows(compPosDistrbA, otherGroupsA) %>%
      mutate(POSf = as.factor(POS))

write_rds(groupDistrb, "data/groupJoyPlot.rds")


groupJoy <- ggplot(data = groupDistrb,
                   aes(y = forcats::fct_relevel(POSf, "Reds HOF", "Md", "CO", "OF", "MI", "CI"),
                       x = redsJAWS,
                       color = redsJAWS,
                       fill = ..x..)) +
      geom_density_ridges_gradient(rel_min_height = 0.01,
                                   alpha = 0.75)  +
      scale_fill_viridis(option = "C",
                         name = "Reds JAWS") +
      scale_color_viridis(option = "C") +
      guides(color = F) +
      geom_quasirandom(color = "white",
                       alpha = 0.5,
                       shape = 21,
                       size = 1.1,
                       groupOnX = F) +
      theme_ridges() +
      theme(legend.position = "top",
            plot.caption = element_text(hjust=0),
            legend.key.width = unit(1.25, "cm")) +
      labs(x = "Reds JAWS", y = "Group",
           title = "Reds HOF JAWS Group Distributions")

groupJoy



# Unweighted + Groups ==================================


compPosDistrbB <- compPosDistrb %>% 
      select(-POSf)

groupDistrbB <- groupDistrb %>%
      select(-POSf)

bothDistrb <- bind_rows(compPosDistrbB, groupDistrbB) %>% 
      mutate(POSf = as.factor(POS))

# Changed this file in the next section below
write_rds(bothDistrb, "data/08 - unweightedandGroupJoyplot.rds")


bothJoy <- ggplot(data = bothDistrb,
                   aes(y = forcats::fct_relevel(POSf, "Reds HOF", "Md", "CO", "OF", "MI", "CI", "RF", "CF", "LF", "SS", "3B", "2B", "1B", "C", "P"),
                       x = redsJAWS,
                       color = redsJAWS,
                       fill = ..x..)) +
      geom_density_ridges_gradient(rel_min_height = 0.01,
                                   alpha = 0.75)  +
      scale_fill_viridis(option = "C",
                         name = "Reds JAWS") +
      scale_color_viridis(option = "C") +
      guides(color = F) +
      geom_quasirandom(color = "white",
                       alpha = 0.5,
                       shape = 21,
                       size = 1.1,
                       groupOnX = F) +
      theme_ridges() +
      theme(legend.position = "top",
            plot.caption = element_text(hjust=0),
            legend.key.width = unit(1.25, "cm")) +
      labs(x = "Reds JAWS", y = "Group",
           title = "Reds HOF JAWS Distributions")

bothJoy


# Without Jitter points ===========

# No legend (guide(fill = F)), no axis labels(axis.title.x/y)
# Changing Reds HOF -> HOF to reduce left margin
bothDistrb <- bothDistrb %>% 
      select(-POSf) %>% 
      mutate(POS = if_else(POS == "Reds HOF", "HOF", POS))
bothDistrb$POSf <- as.factor(bothDistrb$POS)

write_rds(bothDistrb, "data/08 - unweightedandGroupJoyplot.rds")

ggplot(data = bothDistrb,
       aes(y = forcats::fct_relevel(POSf, "HOF", "Md", "CO", "OF", "MI", "CI", "RF", "CF", "LF", "SS", "3B", "2B", "1B", "C", "P"),
           x = redsJAWS,
           color = redsJAWS,
           fill = ..x..)) +
      geom_density_ridges_gradient(rel_min_height = 0.01,
                                   alpha = 0.75)  +
      scale_fill_viridis(option = "C") +
      scale_color_viridis(option = "C") +
      guides(fill = F) +
      theme_ridges() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(title = "Reds HOF JAWS-4 Distributions")

