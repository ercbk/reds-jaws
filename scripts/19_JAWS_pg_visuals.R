# JAWS tab visuals




library(tidyverse)
library(ggiraph)


# Display Table ==============================================

wj_display <- read_rds("data/18 - JAWS pg display table.rds")


# Cleveland Dot ==============================================

jaws_group <- read_rds("data/18 - JAWS pg JAWS dot chart table.rds")


bench_jaws <- jaws_group %>% 
      filter(Name == "Johnny Bench")

boone_jaws <- jaws_group %>% 
      filter(Name == "Aaron Boone")


# Bench
# 2 group plot, no x-axis

jaws_right_label <- bench_jaws %>% 
      group_by(Group) %>%
      arrange(desc(Value)) %>% 
      top_n(1)

jaws_left_label <- bench_jaws %>% 
      group_by(Group) %>% 
      arrange(desc(Value)) %>% 
      slice(2)

ggplot(bench_jaws, aes(x = Value, y = Group)) +
      geom_line(aes(group = Group)) +
      geom_point(aes(color = Stat), size = 3) +
      geom_text(data = jaws_right_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = -0.5) +
      geom_text(data = jaws_left_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = 1.5) +
      scale_color_manual(labels = c("Typical HOFer (weighted)", "Player"), values = c("#000000", "#C6011F")) +
      labs(title = "JAWS-4") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.justification = c(0,1),
            legend.position = c(.1, 1),
            legend.background = element_blank(),
            legend.direction = "horizontal",
            # text = element_text(family = "TT Arial"),
            plot.title = element_text(size = 20, margin = margin(b = 10))
      )


# Boone
# 3 group plot

jaws_right_label <- boone_jaws %>% 
      group_by(Group) %>% 
      arrange(desc(Value)) %>% 
      top_n(1)

jaws_left_label <- boone_jaws %>% 
      group_by(Group) %>% 
      arrange(desc(Value)) %>% 
      slice(2)

p <- ggplot(boone_jaws, aes(x = Value, y = Group)) +
      geom_line(aes(group = Group)) +
      geom_point(aes(color = Stat), size = 3) +
      geom_text(data = jaws_right_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = -0.5) +
      geom_text(data = jaws_left_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = 1.5) +
      scale_x_continuous(limits = c(min(boone_jaws$Value)-10, max(boone_jaws$Value)+10)) + 
      scale_color_manual(labels = c("Typical HOFer (weighted)", "Player"), values = c("#000000", "#C6011F")) +
      labs(title = "JAWS-4") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.justification = c(0,1),
            legend.position = c(.1, 1),
            legend.background = element_blank(),
            legend.direction = "horizontal",
            # text = element_text(family = "Georgia"),
            plot.title = element_text(size = 20, margin = margin(b = 10))
      )


# WAR dot plot


# Mr. Perfect

war_group <- read_rds("data/18 - JAWS pg WAR dot chart table.rds")

# "browning_war" would match line chart df
browning_war_dot <- war_group %>% 
      filter(Name == "Tom Browning")



war_right_label <- browning_war_dot %>% 
      group_by(Group) %>% 
      arrange(desc(Value)) %>% 
      top_n(1)

war_left_label <- browning_war_dot %>% 
      group_by(Group) %>% 
      arrange(desc(Value)) %>% 
      slice(2)

ggplot(browning_war_dot, aes(x = Value, y = Group)) +
      geom_line(aes(group = Group)) +
      geom_point(aes(color = Stat), size = 3) +
      geom_text(data = war_right_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = -0.5) +
      geom_text(data = war_left_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = 1.5) +
      scale_x_continuous(limits = c(min(browning_war_dot$Value)-10, max(browning_war_dot$Value)+10)) + 
      scale_color_manual(labels = c("Typical HOFer (weighted)", "Player"), values = c("#000000", "#C6011F")) +
      labs(title = "WARtenure") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.justification = c(0,1),
            legend.position = c(.1, 1),
            legend.background = element_blank(),
            legend.direction = "horizontal",
            # text = element_text(family = "TT Arial"),
            plot.title = element_text(size = 20, margin = margin(b = 10))
      )


# Big Donkey

war_dot <- war_group %>% 
      filter(Name == "Adam Dunn")


war_right_label <- war_dot %>% 
      group_by(Group) %>% 
      arrange(desc(Value)) %>% 
      top_n(1)

war_left_label <- war_dot %>% 
      group_by(Group) %>% 
      arrange(desc(Value)) %>% 
      slice(2)

ggplot(war_dot, aes(x = Value, y = Group)) +
      geom_line(aes(group = Group)) +
      geom_point(aes(color = Stat), size = 3) +
      geom_text(data = war_right_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = -0.5) +
      geom_text(data = war_left_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = 1.5) +
      scale_x_continuous(limits = c(min(war_dot$Value)-10, max(war_dot$Value)+10)) + 
      scale_color_manual(labels = c("Typical HOFer (weighted)", "Player"), values = c("#000000", "#C6011F")) +
      labs(title = "WARtenure") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.justification = c(0,1),
            legend.position = c(.1, 1),
            legend.background = element_blank(),
            legend.direction = "horizontal",
            # text = element_text(family = "TT Arial"),
            plot.title = element_text(size = 20, margin = margin(b = 10))
      )



# Line Chart ====================================================

war_combo_avg <- read_rds("data/18 - JAWS pg line chart table.rds")


# Bench

war_line <- war_combo_avg %>% 
      filter(Name == "Johnny Bench")
line_filtered <- war_line %>% 
      filter(type == "WAR4")

p <- ggplot(data = war_line) + 
      geom_point_interactive(aes(x = yearId, y = WAR, group = type, tooltip = WAR), color = alpha("#000000", 0.5)) +
      geom_point_interactive(data = line_filtered, aes(x = yearId, y = WAR, color = type, tooltip = WAR), size = 2.5, shape = 17) +
      geom_line(aes(x = yearId, y = WAR)) +
      # all the Median WAR is the same, taking mean is just me hacking to get a value instead of a vector for the y-intercept
      geom_hline(aes(yintercept = mean(`Median WAR`), linetype = "Typical HOFer"), color = alpha("#C6011F", 0.5), size = 1.25) +
      scale_linetype_manual(values = 2, guide = guide_legend(override.aes = list(color = "#C6011F"))) +
      scale_y_continuous(limits = c(min(war_line$WAR)-5, max(war_line$WAR)+5)) +
      labs(title = "WAR") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.justification = c(0,1),
            legend.position = c(.1, 1),
            legend.box = "horizontal",
            legend.background = element_blank(),
            legend.direction = "horizontal",
            # text = element_text(family = "Georgia"),
            plot.title = element_text(size = 20, margin = margin(b = 10))
      )

ggiraph(ggobj = p)

# Reggie Sanders (non-interactive)

war_line <- war_combo_avg %>% 
      filter(Name == "Reggie Sanders")
line_filtered <- war_line %>% 
      filter(type == "WAR4")


p <- ggplot(data = war_line) + 
      geom_point_interactive(aes(x = yearId, y = WAR, group = type, tooltip = WAR), color = alpha("#000000", 0.5)) +
      geom_point_interactive(data = line_filtered, aes(x = yearId, y = WAR, color = type, tooltip = WAR), size = 2.5, shape = 17) +
      geom_line(aes(x = yearId, y = WAR)) +
      geom_hline(aes(yintercept = mean(`Median WAR`), linetype = "Typical HOFer"), color = alpha("#C6011F", 0.5), size = 1.25) +
      scale_linetype_manual(values = 2, guide = guide_legend(override.aes = list(color = "#C6011F"))) +
      scale_y_continuous(limits = c(min(war_line$WAR)-5, max(war_line$WAR)+5)) +
      labs(title = "WAR") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.justification = c(0,1),
            legend.position = c(.1, 1),
            legend.box = "horizontal",
            legend.background = element_blank(),
            legend.direction = "horizontal",
            # text = element_text(family = "Georgia"),
            plot.title = element_text(size = 20, margin = margin(b = 10))
      )
ggiraph(ggobj = p)

# Adam Dunn

war_line <- war_combo_avg %>% 
      filter(Name == "Adam Dunn")
line_filtered <- war_line %>% 
      filter(type == "WAR4")


p <- ggplot(data = war_line) + 
      geom_point_interactive(aes(x = yearId, y = WAR, group = type, tooltip = WAR), color = alpha("#000000", 0.5)) +
      geom_point_interactive(data = line_filtered, aes(x = yearId, y = WAR, color = type, tooltip = WAR), size = 2.5, shape = 17) +
      geom_line(aes(x = yearId, y = WAR)) +
      geom_hline(aes(yintercept = mean(`Median WAR`), linetype = "Typical HOFer"), color = alpha("#C6011F", 0.5), size = 1.25) +
      scale_linetype_manual(values = 2, guide = guide_legend(override.aes = list(color = "#C6011F"))) +
      scale_y_continuous(limits = c(min(war_line$WAR)-5, max(war_line$WAR)+5)) +
      labs(title = "WAR") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.justification = c(0,1),
            legend.position = c(.1, 1),
            legend.box = "horizontal",
            legend.background = element_blank(),
            legend.direction = "horizontal",
            # text = element_text(family = "Georgia"),
            plot.title = element_text(size = 20, margin = margin(b = 10))
      )
ggiraph(ggobj = p)
