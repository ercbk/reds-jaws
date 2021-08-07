# Server

# rsconnect::deployApp(appName="jaws4-post")

library(shiny)
library(tidyverse)
library(DT)
library(ggiraph)
library(ggpubr)

# Data

display_table <- read_rds("data/display_table.rds")
war_combo_avg <- read_rds("data/line_chart.rds")
war_group <- read_rds("data/war_dot.rds")
jaws_group <- read_rds("data/jaws_dot.rds")



shinyServer(function(input, output, session){
      
      output$jTable <- renderDT({
            datatable(
                  data = display_table,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 search = list(regex = TRUE),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      
      output$warCleve <- renderPlot({
            
            war_dot <- war_group %>% 
                  filter(Name == input$jplayer)
            
            
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
                  scale_x_continuous(limits = c(min(war_dot$Value)-30, max(war_dot$Value)+28)) +
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
                        legend.direction = "vertical",
                        # text = element_text(family = "TT Arial"),
                        plot.title = element_text(size = 20, margin = margin(b = 10))
                  )
      })
      
      
      output$jawsCleve <- renderPlot({
            
            jaws_dot <- jaws_group %>% 
                  filter(Name == input$jplayer)
            
            jaws_right_label <- jaws_dot %>% 
                  group_by(Group) %>%
                  arrange(desc(Value)) %>% 
                  top_n(1)
            
            jaws_left_label <- jaws_dot %>% 
                  group_by(Group) %>% 
                  arrange(desc(Value)) %>% 
                  slice(2)
            
            ggplot(jaws_dot, aes(x = Value, y = Group)) +
                  geom_line(aes(group = Group)) +
                  geom_point(aes(color = Stat), size = 3) +
                  geom_text(data = jaws_right_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = -0.5) +
                  geom_text(data = jaws_left_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = 1.5) +
                  scale_x_continuous(limits = c(min(jaws_dot$Value)-30, max(jaws_dot$Value)+28)) +
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
                        legend.direction = "vertical",
                        # text = element_text(family = "TT Arial"),
                        plot.title = element_text(size = 20, margin = margin(b = 10))
                  )
      })
      
      
      
      output$lineChart <- renderggiraph({
            
            war_line <- war_combo_avg %>% 
                  filter(Name == input$jplayer)
            line_filtered <- war_line %>% 
                  filter(type == "WAR4")
            
            p <- ggplot(data = war_line) + 
                  geom_point_interactive(aes(x = yearId, y = WAR, group = type, tooltip = WAR), color = alpha("#000000", 0.5)) +
                  geom_point_interactive(data = line_filtered, aes(x = yearId, y = WAR, color = type, tooltip = WAR), size = 2.5, shape = 17) +
                  geom_line(aes(x = yearId, y = WAR)) +
                  # all the Median WAR is the same; taking mean is just me hacking to get a value instead of a vector for the y-intercept
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
            ggiraph(code = print(p))
            
      })
})

