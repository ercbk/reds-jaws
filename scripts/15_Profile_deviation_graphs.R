# Profile deviation graphs

# Going with HOF dataset and Positional Normalization




library(tidyverse)
library(ggpubr)



rfb <- read_rds("data/14 - reg standardization fran batting.rds")
rfp <- read_rds("data/14 - reg standardization fran pitching.rds")
rhb <- read_rds("data/14 - reg standardization hof batting.rds")
rhp <- read_rds("data/14 - reg standardization hof pitching.rds")

pfb <- read_rds("data/14 - pos normalization fran batting.rds")
pfp <- read_rds("data/14 - pos normalization fran pitching.rds")
phb <- read_rds("data/14 - pos normalization hof batting.rds")
php <- read_rds("data/14 - pos normalization hof pitching.rds")


phb_griff <- phb %>%
      filter(Name == "Ken Griffey Jr")

phb_dunn <- phb %>% 
      filter(Name == "Adam Dunn")

# Color by groups
# Custom color palette
# Sort value in descending order
# Add segments from y = 0 to dots
# Change segment color and size
# Order by groups
# Large dot size
# Add mpg values as dot labels
# Adjust label parameters
# ggplot2 theme
# Make it vertical

# Experimental
p <- ggdotchart(phb_griff, x = "stat", y = "score",
           color = "sign",                                
                 palette = c("#000000", "#C6011F", "#FC4E07"), 
           sorting = "descending",                       
           add = "segments",                             
           add.params = list(color = "lightgray", size = 2), 
           group = "sign",
           label = round(phb_griff$score, 1),
           xlab = FALSE,
           dot.size = 7, 
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               
           ggtheme = theme_pubr(),                       
           rotate = TRUE
)+
      geom_vline(xintercept = 0, linetype = 2, color = "lightgray") + rremove("x.axis") + rremove("legend")

ggpar(p, title = "HOF Score", caption = "Score relates player to typical HOFer", ticks = FALSE)






# Control
ggdotchart(phb_dunn, x = "stat", y = "score",
           title = "Reds HOF",
           color = "sign",                                
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
           sorting = "descending",                       
           add = "segments",                             
           add.params = list(color = "lightgray", size = 2), 
           group = "sign",
           label = round(phb_dunn$score, 1),
           dot.size = 6,                                 
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               
           ggtheme = theme_pubr(),                       
           rotate = TRUE
)+
      geom_vline(xintercept = 0, linetype = 2, color = "lightgray")

