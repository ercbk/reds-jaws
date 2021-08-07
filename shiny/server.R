


library(shiny)
library(tidyverse)
library(DT)


# Ridge Plots
library(ggridges)
library(viridis)

# Interactive Line Chart
library(ggiraph)

# Cleveland Dot Plots and Deviation Chart
library(ggpubr)

# Needed for Density Plots page
library(broom)
library(rlang)

# Numbers page
library(formattable)

# Data =========================================================

# Home Page ====================

jaws_ref_table <- read_rds("data/17 - JAWS Reference Table.rds")
bothDistrb <- read_rds("data/08 - unweightedandGroupJoyplot.rds")

# JAWS Page ====================

# Table
wj_display <- read_rds("data/18 - JAWS pg display table.rds") %>% 
      rename(WARtenure = WAR)

# Cleveland Dot Plots
jaws_group <- read_rds("data/18 - JAWS pg JAWS dot chart table.rds")
war_group <- read_rds("data/18 - JAWS pg WAR dot chart table.rds")

# Line Chart
war_combo_avg <- read_rds("data/18 - JAWS pg line chart table.rds")

# Profile Page =================

# Tables
# Batting
prof_batting <- read_rds("data/13 - HOF Batting.rds") 
      
prof_fielding <- read_rds("data/14 - Fielding display table.rds")
prof_postseas_b <- read_rds("data/14 - Postseason batting display table.rds")
prof_awards_b <- read_rds("data/14 - Awards display table.rds")

# Pitching
prof_pitching <- read_rds("data/13 - HOF Pitching.rds")
prof_postseas_p <- read_rds("data/14 - Postseason pitching display table.rds")
prof_awards_p <- read_rds("data/14 - Awards display table.rds")

# Deviation Chart
# Batting
phb <- read_rds("data/14 - pos normalization hof batting.rds")
# Pitching
php <- read_rds("data/14 - pos normalization hof pitching.rds")

# Stat Rank Page ================

sr_fran_bat <- read_rds("data/13 - Franchise Batting.rds")
sr_fran_pit <- read_rds("data/13 - Franchise Pitching.rds")

sr_hof_brank <- read_rds("data/13 - HOF Batting Stats and Ranks.rds")
sr_hof_prank <- read_rds("data/13 - HOF Pitching Stats and Ranks.rds")

sr_fran_brank <- read_rds("data/13 - Franchise Batting Stats and Ranks.rds")
sr_fran_prank <- read_rds("data/13 - Franchise Pitching Stats and Ranks.rds")

# Number Page ====================

awards_b_num <- read_rds("data/21 - Numbers pg HOF Batting Awards.rds")
batting_num <- read_rds("data/21 - Numbers pg HOF Batting.rds") %>% 
      rename(bWAR = WAR)
awards_shares_b_num <- read_rds("data/21 - Numbers pg HOF Batting Awards Shares.rds")
fielding_num <- read_rds("data/20 - Numbers pg HOF Fielding.rds") %>% 
      select(Name, everything())
pitching_num <- read_rds("data/21 - Numbers pg HOF Pitching.rds") %>% 
      rename(bWAR = WAR)
awards_shares_p_num <- read_rds("data/21 - Numbers pg HOF Pitching Awards Shares.rds")
awards_p_num <- read_rds("data/21 - Numbers pg HOF Pitching Awards.rds")
ps_bat_num <- read_rds("data/20 - Numbers pg HOF Postseason Batting.rds")
ps_pit_num <- read_rds("data/20 - Numbers pg HOF Postseason Pitching.rds")
seas_bat_num <- read_rds("data/20 - Numbers pg HOF seas Batting.rds") %>% 
      rename(`2B` = X2B, `3B` = X3B)
seas_pit_num <- read_rds("data/20 - Numbers pg HOF seas Pitching.rds")



# Server =======================================================

shinyServer(function(input, output, session){
      
      # Home Pg ================= 
      
      output$hTable <- renderDT({
            datatable(
                  data = jaws_ref_table,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 pageLength = 14,
                                 search = list(regex = TRUE),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 1),
                                 dom = ("Bfrtip"),
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
                                 
                  )
      })
      
      
      output$ridge <- renderPlot({
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
            
      })
      
      # JAWs Pg =================
      
      output$jTable <- renderDT({
            datatable(
                  data = wj_display,
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
      
      
      
      # Profile Pg ===============
      
      # Tables
      # Batting
      output$prof_bat_Table <- renderDT({
            prof_bat_tab <- prof_batting %>% 
                  filter(Name == input$prof_bat_player)
            datatable(
                  data = prof_bat_tab,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left; color:#C6011F;
                        font-size:100% ;', tags$b("Batting")),
                  extensions = c("FixedColumns"),
                  options = list(dom = 't',
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
                                 
            )
      })
      
      output$prof_field_Table <- renderDT({
            prof_field_tab <- prof_fielding %>% 
                  filter(Name == input$prof_bat_player)
            datatable(
                  data = prof_field_tab,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left; color:#C6011F;
                        font-size:100% ;', tags$b("Fielding")),
                  extensions = c("FixedColumns"),
                  options = list(dom = 't',
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
                  
            )
      })
      
      output$prof_psb_Table <- renderDT({
            prof_psb_tab <- prof_postseas_b %>% 
                  filter(Name == input$prof_bat_player)
            datatable(
                  data = prof_psb_tab,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left; color:#C6011F;
                        font-size:100% ;', tags$b("Postseason")),
                  extensions = c("FixedColumns"),
                  options = list(dom = 't',
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      output$prof_awab_Table <- renderDT({
            prof_awab_tab <- prof_awards_b %>% 
                  filter(Name == input$prof_bat_player)
            datatable(
                  data = prof_awab_tab,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left; color:#C6011F;
                        font-size:100% ;', tags$b("Awards")),
                  extensions = c("FixedColumns"),
                  options = list(dom = 't',
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      # Pitching
      output$prof_pit_Table <- renderDT({
            prof_pit_tab <- prof_pitching %>% 
                  filter(Name == input$prof_pit_player)
            datatable(
                      data = prof_pit_tab,
                      rownames = FALSE,
                      caption = htmltools::tags$caption(
                            style = 'caption-side: top; text-align: left; color:#C6011F;
                            font-size:100% ;', tags$b("Pitching")),
                      extensions = c("FixedColumns"),
                      options = list(dom = 't',
                                     scrollX = TRUE,
                                     search = list(regex = TRUE),
                                     fixedColumns = list(leftColumns = 1),
                                     initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                           "}"
                                     ))
            )
      })
      
      output$prof_psp_Table <- renderDT({
            prof_psp_tab <- prof_postseas_p %>% 
                  filter(Name == input$prof_pit_player)
            datatable(
                  data = prof_psp_tab,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left; color:#C6011F;
                        font-size:100% ;', tags$b("Postseason")),
                  extensions = c("FixedColumns"),
                  options = list(dom = 't',
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      output$prof_awap_Table <- renderDT({
            prof_awap_tab <- prof_awards_b %>% 
                  filter(Name == input$prof_pit_player)
            datatable(
                  data = prof_awap_tab,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left; color:#C6011F;
                        font-size:100% ;', tags$b("Awards")),
                  extensions = c("FixedColumns"),
                  options = list(dom = 't',
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      # Deviation Charts
      
      # Batting
      output$bat_dev <- renderPlot({
            
            phb_tab <- phb %>% 
                  filter(Name == input$prof_bat_player)
            dev_b <- ggdotchart(phb_tab, x = "stat", y = "score",
                            color = "sign",                                
                            palette = c("#000000", "#C6011F", "#FC4E07"), 
                            sorting = "descending",                       
                            add = "segments",                             
                            add.params = list(color = "lightgray", size = 2), 
                            group = "sign",
                            label = round(phb_tab$score, 1),
                            xlab = FALSE,
                            dot.size = 7, 
                            font.label = list(color = "white", size = 9, 
                                              vjust = 0.5),               
                            ggtheme = theme_pubr(),                       
                            rotate = TRUE
            )+
                  geom_vline(xintercept = 0, linetype = 2, color = "lightgray") + rremove("x.axis") + rremove("legend")
            
            ggpar(dev_b, title = "HOF Score", caption = "Score relates player to typical HOFer", ticks = FALSE)
      })
      
      # Pitching
      output$pit_dev <- renderPlot({
            
            php_tab <- php %>% 
                  filter(Name == input$prof_pit_player)
            dev_p <- ggdotchart(php_tab, x = "stat", y = "score",
                                color = "sign",                                
                                palette = c("#000000", "#C6011F", "#FC4E07"), 
                                sorting = "descending",                       
                                add = "segments",                             
                                add.params = list(color = "lightgray", size = 2), 
                                group = "sign",
                                label = round(php_tab$score, 1),
                                xlab = FALSE,
                                dot.size = 7, 
                                font.label = list(color = "white", size = 9, 
                                                  vjust = 0.5),               
                                ggtheme = theme_pubr(),                       
                                rotate = TRUE
            )+
                  geom_vline(xintercept = 0, linetype = 2, color = "lightgray") + rremove("x.axis") + rremove("legend")
            
            ggpar(dev_p, title = "HOF Score", caption = "Score relates player to typical HOFer", ticks = FALSE)
      })
      
      
      
      # Stat Rank Pg ===============
      
      # Tables
      # Batting
      output$sr_hof_bTable <- renderDT({
            datatable(
                  data = prof_batting,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left; color:#C6011F;
                        font-size:100% ;', tags$b("HOF")),
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      output$sr_fran_bTable <- renderDT({
            datatable(
                  data = sr_fran_bat,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left; color:#C6011F;
                        font-size:100% ;', tags$b("Franchise")),
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
                  )
      })
      
      # Pitching
      output$sr_hof_pTable <- renderDT({
            datatable(
                  data = prof_pitching,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left; color:#C6011F;
                        font-size:100% ;', tags$b("HOF")),
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
                  )
      })
      
      output$sr_fran_pTable <- renderDT({
            datatable(
                  data = sr_fran_pit,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left; color:#C6011F;
                        font-size:100% ;', tags$b("Franchise")),
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
                  )
      })
      
      
      # Density
      # Batting
      # HOF
      output$hof_bat_dens <- renderPlot({
            ggplot2::theme_set(
                  theme_bw(base_size = 12) +
                        theme(
                              plot.title = element_text(face = 'bold', hjust = 0),
                              text = element_text(colour = '#4e5c65'),
                              panel.background = element_rect('#ffffff'),
                              strip.background = element_rect('#ffffff', colour = 'white'),
                              plot.background = element_rect('#ffffff'),
                              panel.border = element_rect(colour = '#ffffff'),
                              panel.grid.major.x = element_blank(),
                              panel.grid.major.y = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              legend.background = element_rect('#ffffff'),
                              legend.title = element_blank(),
                              legend.position = 'right',
                              legend.direction = 'vertical',
                              legend.key = element_blank(),
                              strip.text = element_text(face = 'bold', size = 10),
                              axis.text = element_text(face = 'bold', size = 9),
                              axis.title = element_blank(),
                              axis.ticks = element_blank()
                        )
            )
            
            hofb_density_FUN <- function(play_sel, col_sel) {
                  
                  # Density calculation
                  df2 <-  prof_batting %>%
                        select(Name) %>% 
                        group_by(Name) %>%
                        do(tidy(density(prof_batting[, col_sel][[1]], bw = "nrd0", na.rm = TRUE))) %>% 
                        group_by() %>% 
                        mutate(ymin = max(y) / 1.5, 
                               ymax = y + ymin,
                               ylabel = ymin + min(ymin)/2,
                               xlabel = min(x) - mean(range(x))/2)
                  
                  # Median label coords using vars from density calc
                  labels2 <- prof_batting %>% 
                        select(Name, !!col_sel) %>% 
                        mutate(median = median(!!sym(col_sel), na.rm = TRUE)) %>%
                        filter(row_number() == 1) %>% 
                        select(-one_of(col_sel)) %>% 
                        left_join(df2) %>% 
                        mutate(xmed = x[which.min(abs(x - median))],
                               yminmed = ymin[which.min(abs(x - median))],
                               ymaxmed = ymax[which.min(abs(x - median))]) %>% 
                        filter(row_number() == 1)     
                  
                  # Player stat for shaded area
                  shade_bdy <- prof_batting %>% 
                        filter(Name == play_sel)
                  shade_bdy <- shade_bdy[, col_sel][[1]]
                  
                  
                  if(col_sel %in% c("wRC+", "BB%", "K%", "OPS+", "2B", "3B")) {
                        col_sel <- paste0("`", col_sel, "`")
                  } else {
                        col_sel
                  }
                  
                  # frame
                  a <- ggplot(data = prof_batting, aes_string(x = quo_name(col_sel))) +
                        geom_density(fill = "#000000", alpha = 0.7) +
                        scale_x_continuous() +
                        theme(axis.text.y = element_blank())
                  
                  d <- ggplot_build(a)$data[[1]]
                  
                  # Shaded area, median line, median label
                  a + geom_area(data = subset(d, x < shade_bdy), aes(x = x, y = y), fill = "#C6011F", alpha = 0.75) +
                        
                        geom_segment(data = labels2, aes(x = xmed, xend = xmed, y = 0, yend = ymaxmed/1.77), colour = "#F0F0F0", linetype = "dashed") +
                        
                        geom_text(data = labels2, aes(xmed - xlabel/50, (ylabel*0.05 + ylabel)), 
                                  label = "Median", colour = "#000000", hjust = 0, fontface = "italic", size = 4) +
                        geom_rug() +
                        labs(title = "HOF")
                  
            }
            
            hofb_density_FUN(input$r_bat_player, input$r_bat_stat)
      })
      
      # Franchise
      output$fran_bat_dens <- renderPlot({
            ggplot2::theme_set(
                  theme_bw(base_size = 12) +
                        theme(
                              plot.title = element_text(face = 'bold', hjust = 0),
                              text = element_text(colour = '#4e5c65'),
                              panel.background = element_rect('#ffffff'),
                              strip.background = element_rect('#ffffff', colour = 'white'),
                              plot.background = element_rect('#ffffff'),
                              panel.border = element_rect(colour = '#ffffff'),
                              panel.grid.major.x = element_blank(),
                              panel.grid.major.y = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              legend.background = element_rect('#ffffff'),
                              legend.title = element_blank(),
                              legend.position = 'right',
                              legend.direction = 'vertical',
                              legend.key = element_blank(),
                              strip.text = element_text(face = 'bold', size = 10),
                              axis.text = element_text(face = 'bold', size = 9),
                              axis.title = element_blank(),
                              axis.ticks = element_blank()
                        )
            )
            
            franb_density_FUN <- function(play_sel, col_sel) {
                  
                  # Density calculation
                  df2 <-  sr_fran_bat %>%
                        select(Name) %>% 
                        group_by(Name) %>%
                        do(tidy(density(sr_fran_bat[, col_sel][[1]], bw = "nrd0", na.rm = TRUE))) %>% 
                        group_by() %>% 
                        mutate(ymin = max(y) / 1.5, 
                               ymax = y + ymin,
                               ylabel = ymin + min(ymin)/2,
                               xlabel = min(x) - mean(range(x))/2)
                  
                  # Median label coords using vars from density calc
                  labels2 <- sr_fran_bat %>% 
                        select(Name, !!col_sel) %>% 
                        mutate(median = median(!!sym(col_sel), na.rm = TRUE)) %>%
                        filter(row_number() == 1) %>% 
                        select(-one_of(col_sel)) %>% 
                        left_join(df2) %>% 
                        mutate(xmed = x[which.min(abs(x - median))],
                               yminmed = ymin[which.min(abs(x - median))],
                               ymaxmed = ymax[which.min(abs(x - median))]) %>% 
                        filter(row_number() == 1)     
                  
                  # Player stat for shaded area
                  shade_bdy <- sr_fran_bat %>% 
                        filter(Name == play_sel)
                  shade_bdy <- shade_bdy[, col_sel][[1]]
                  
                  
                  if(col_sel %in% c("wRC+", "BB%", "K%", "OPS+", "2B", "3B")) {
                        col_sel <- paste0("`", col_sel, "`")
                  } else {
                        col_sel
                  }
                  
                  # frame
                  a <- ggplot(data = sr_fran_bat, aes_string(x = quo_name(col_sel))) +
                        geom_density(fill = "#000000", alpha = 0.7) +
                        scale_x_continuous() +
                        theme(axis.text.y = element_blank())
                  
                  d <- ggplot_build(a)$data[[1]]
                  
                  # Shaded area, median line, median label
                  a + geom_area(data = subset(d, x < shade_bdy), aes(x = x, y = y), fill = "#C6011F", alpha = 0.75) +
                        
                        geom_segment(data = labels2, aes(x = xmed, xend = xmed, y = 0, yend = ymaxmed/1.77), colour = "#F0F0F0", linetype = "dashed") +
                        
                        geom_text(data = labels2, aes(xmed - xlabel/50, (ylabel*0.05 + ylabel)), 
                                  label = "Median", colour = "#000000", hjust = 0, fontface = "italic", size = 4) +
                        geom_rug() +
                        labs(title = "Franchise")
                  
            }
            
            franb_density_FUN(input$r_bat_player, input$r_bat_stat)
      })
      
      
      # Pitching
      # HOF
      
      output$hof_pit_dens <- renderPlot({
            ggplot2::theme_set(
                  theme_bw(base_size = 12) +
                        theme(
                              plot.title = element_text(face = 'bold', hjust = 0),
                              text = element_text(colour = '#4e5c65'),
                              panel.background = element_rect('#ffffff'),
                              strip.background = element_rect('#ffffff', colour = 'white'),
                              plot.background = element_rect('#ffffff'),
                              panel.border = element_rect(colour = '#ffffff'),
                              panel.grid.major.x = element_blank(),
                              panel.grid.major.y = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              legend.background = element_rect('#ffffff'),
                              legend.title = element_blank(),
                              legend.position = 'right',
                              legend.direction = 'vertical',
                              legend.key = element_blank(),
                              strip.text = element_text(face = 'bold', size = 10),
                              axis.text = element_text(face = 'bold', size = 9),
                              axis.title = element_blank(),
                              axis.ticks = element_blank()
                        )
            )
            
            hofp_density_FUN <- function(play_sel, col_sel) {
                  
                  # Density calculation
                  df2 <-  prof_pitching %>%
                        select(Name) %>% 
                        group_by(Name) %>%
                        do(tidy(density(prof_pitching[, col_sel][[1]], bw = "nrd0", na.rm = TRUE))) %>% 
                        group_by() %>% 
                        mutate(ymin = max(y) / 1.5, 
                               ymax = y + ymin,
                               ylabel = ymin + min(ymin)/2,
                               xlabel = min(x) - mean(range(x))/2)
                  
                  # Median label coords using vars from density calc
                  labels2 <- prof_pitching %>% 
                        select(Name, !!col_sel) %>% 
                        mutate(median = median(!!sym(col_sel), na.rm = TRUE)) %>%
                        filter(row_number() == 1) %>% 
                        select(-one_of(col_sel)) %>% 
                        left_join(df2) %>% 
                        mutate(xmed = x[which.min(abs(x - median))],
                               yminmed = ymin[which.min(abs(x - median))],
                               ymaxmed = ymax[which.min(abs(x - median))]) %>% 
                        filter(row_number() == 1)     
                  
                  # Player stat for shaded area
                  shade_bdy <- prof_pitching %>% 
                        filter(Name == play_sel)
                  shade_bdy <- shade_bdy[, col_sel][[1]]
                  
                  
                  if(col_sel %in% c("W-L%", "ERA-", "FIP-", "xFIP-", "K/9", "BB/9", "K/BB", "HR/9", "K%", "BB%", "K-BB%", "ERA+")) {
                        col_sel <- paste0("`", col_sel, "`")
                  } else {
                        col_sel
                  }
                  
                  # frame
                  a <- ggplot(data = prof_pitching, aes_string(x = quo_name(col_sel))) +
                        geom_density(fill = "#000000", alpha = 0.7) +
                        scale_x_continuous() +
                        theme(axis.text.y = element_blank())
                  
                  d <- ggplot_build(a)$data[[1]]
                  
                  # Shaded area, median line, median label
                  a + geom_area(data = subset(d, x < shade_bdy), aes(x = x, y = y), fill = "#C6011F", alpha = 0.75) +
                        
                        geom_segment(data = labels2, aes(x = xmed, xend = xmed, y = 0, yend = ymaxmed/1.77), colour = "#F0F0F0", linetype = "dashed") +
                        
                        geom_text(data = labels2, aes(xmed - xlabel/50, (ylabel*0.05 + ylabel)), 
                                  label = "Median", colour = "#000000", hjust = 0, fontface = "italic", size = 4) +
                        geom_rug() +
                        labs(title = "HOF")
                  
            }
            
            hofp_density_FUN(input$r_pit_player, input$r_pit_stat)
      })
      
      # Franchise
      
      output$fran_pit_dens <- renderPlot({
            ggplot2::theme_set(
                  theme_bw(base_size = 12) +
                        theme(
                              plot.title = element_text(face = 'bold', hjust = 0),
                              text = element_text(colour = '#4e5c65'),
                              panel.background = element_rect('#ffffff'),
                              strip.background = element_rect('#ffffff', colour = 'white'),
                              plot.background = element_rect('#ffffff'),
                              panel.border = element_rect(colour = '#ffffff'),
                              panel.grid.major.x = element_blank(),
                              panel.grid.major.y = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              legend.background = element_rect('#ffffff'),
                              legend.title = element_blank(),
                              legend.position = 'right',
                              legend.direction = 'vertical',
                              legend.key = element_blank(),
                              strip.text = element_text(face = 'bold', size = 10),
                              axis.text = element_text(face = 'bold', size = 9),
                              axis.title = element_blank(),
                              axis.ticks = element_blank()
                        )
            )
            
            franp_density_FUN <- function(play_sel, col_sel) {
                  
                  # Density calculation
                  df2 <-  sr_fran_pit %>%
                        select(Name) %>% 
                        group_by(Name) %>%
                        do(tidy(density(sr_fran_pit[, col_sel][[1]], bw = "nrd0", na.rm = TRUE))) %>% 
                        group_by() %>% 
                        mutate(ymin = max(y) / 1.5, 
                               ymax = y + ymin,
                               ylabel = ymin + min(ymin)/2,
                               xlabel = min(x) - mean(range(x))/2)
                  
                  # Median label coords using vars from density calc
                  labels2 <- sr_fran_pit %>% 
                        select(Name, !!col_sel) %>% 
                        mutate(median = median(!!sym(col_sel), na.rm = TRUE)) %>%
                        filter(row_number() == 1) %>% 
                        select(-one_of(col_sel)) %>% 
                        left_join(df2) %>% 
                        mutate(xmed = x[which.min(abs(x - median))],
                               yminmed = ymin[which.min(abs(x - median))],
                               ymaxmed = ymax[which.min(abs(x - median))]) %>% 
                        filter(row_number() == 1)     
                  
                  # Player stat for shaded area
                  shade_bdy <- sr_fran_pit %>% 
                        filter(Name == play_sel)
                  shade_bdy <- shade_bdy[, col_sel][[1]]
                  
                  
                  if(col_sel %in% c("W-L%", "ERA-", "FIP-", "xFIP-", "K/9", "BB/9", "K/BB", "HR/9", "K%", "BB%", "K-BB%", "ERA+")) {
                        col_sel <- paste0("`", col_sel, "`")
                  } else {
                        col_sel
                  }
                  
                  # frame
                  a <- ggplot(data = sr_fran_pit, aes_string(x = quo_name(col_sel))) +
                        geom_density(fill = "#000000", alpha = 0.7) +
                        scale_x_continuous() +
                        theme(axis.text.y = element_blank())
                  
                  d <- ggplot_build(a)$data[[1]]
                  
                  # Shaded area, median line, median label
                  a + geom_area(data = subset(d, x < shade_bdy), aes(x = x, y = y), fill = "#C6011F", alpha = 0.75) +
                        
                        geom_segment(data = labels2, aes(x = xmed, xend = xmed, y = 0, yend = ymaxmed/1.77), colour = "#F0F0F0", linetype = "dashed") +
                        
                        geom_text(data = labels2, aes(xmed - xlabel/50, (ylabel*0.05 + ylabel)), 
                                  label = "Median", colour = "#000000", hjust = 0, fontface = "italic", size = 4) +
                        geom_rug() +
                        labs(title = "Franchise")
                  
            }
            
            franp_density_FUN(input$r_pit_player, input$r_pit_stat)
      })
      
      # 'value' boxes
      # Batting
      
      output$hof_bvalue_box <- renderUI({
            
            text_FUN <- function(name, stat) {
                  rank_var <- paste0("rank_", stat)
                  perc_var <- paste0("perc_", stat)
                  median_var <- paste0("median_", stat)
                  
                  hbv <- sr_hof_brank %>% 
                        filter(Name == name) %>% 
                        select(!!stat, !!median_var, !!rank_var, !!perc_var)
                  
                  hbv_list <- as.list(hbv)
            }
            values <- text_FUN(input$r_bat_player, input$r_bat_stat)
            HTML(paste("<b>Value:</b>  ", values[1],
                  "<br><b>Median:</b>  ", values[2],
                  "<br><b>Rank:</b>  ", values[3],
                  "<br><b>Percentile:</b>  ", values[4],
                  "<br><br><br>"))
            
      })
      
      output$fran_bvalue_box <- renderUI({
            
            text_FUN <- function(name, stat) {
                  rank_var <- paste0("rank_", stat)
                  perc_var <- paste0("perc_", stat)
                  median_var <- paste0("median_", stat)
                  
                  hbv <- sr_fran_brank %>% 
                        filter(Name == name) %>% 
                        select(!!stat, !!median_var, !!rank_var, !!perc_var)
                  
                  hbv_list <- as.list(hbv)
            }
            values <- text_FUN(input$r_bat_player, input$r_bat_stat)
            HTML(paste("<b>Value:</b>  ", values[1],
                       "<br><b>Median:</b>  ", values[2],
                       "<br><b>Rank:</b>  ", values[3],
                       "<br><b>Percentile:</b>  ", values[4],
                       "<br><br><br>"))
            
      })
      
      # Pitching
      
      output$hof_pvalue_box <- renderUI({
            
            text_FUN <- function(name, stat) {
                  rank_var <- paste0("rank_", stat)
                  perc_var <- paste0("perc_", stat)
                  median_var <- paste0("median_", stat)
                  
                  hbv <- sr_hof_prank %>% 
                        filter(Name == name) %>% 
                        select(!!stat, !!median_var, !!rank_var, !!perc_var)
                  
                  hbv_list <- as.list(hbv)
            }
            values <- text_FUN(input$r_pit_player, input$r_pit_stat)
            HTML(paste("<b>Value:</b>  ", values[1],
                       "<br><b>Median:</b>  ", values[2],
                       "<br><b>Rank:</b>  ", values[3],
                       "<br><b>Percentile:</b>  ", values[4],
                       "<br><br><br>"))
            
      })
      
      output$fran_pvalue_box <- renderUI({
            
            text_FUN <- function(name, stat) {
                  rank_var <- paste0("rank_", stat)
                  perc_var <- paste0("perc_", stat)
                  median_var <- paste0("median_", stat)
                  
                  hbv <- sr_fran_prank %>% 
                        filter(Name == name) %>% 
                        select(!!stat, !!median_var, !!rank_var, !!perc_var)
                  
                  hbv_list <- as.list(hbv)
            }
            values <- text_FUN(input$r_pit_player, input$r_pit_stat)
            HTML(paste("<b>Value:</b>  ", values[1],
                       "<br><b>Median:</b>  ", values[2],
                       "<br><b>Rank:</b>  ", values[3],
                       "<br><b>Percentile:</b>  ", values[4],
                       "<br><br><br>"))
            
      })
      
      
      
      # Numbers Page =====================
      
      # Batting
      
      output$numBatTenTab <- renderDT({
            as.datatable(
                  x = batting_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
                  )
      })
      
      output$numBatSeasTab <- renderDT({
            datatable(
                  data = seas_bat_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns", "Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 fixedColumns = list(leftColumns = 1),
                                 columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      output$numBatFldTab <- renderDT({
            datatable(
                  data = fielding_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      output$numBatPsTab <- renderDT({
            datatable(
                  data = ps_bat_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      
      output$numBatAwaTab <- renderDT({
            datatable(
                  data = awards_b_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      output$numBatAsTab <- renderDT({
            datatable(
                  data = awards_shares_b_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      
      # Pitching
      
      output$numPitTenTab <- renderDT({
            as.datatable(
                  x = pitching_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      output$numPitSeasTab <- renderDT({
            datatable(
                  data = seas_pit_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      output$numPitPsTab <- renderDT({
            datatable(
                  data = ps_pit_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      
      output$numPitAwaTab <- renderDT({
            datatable(
                  data = awards_p_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      output$numPitAsTab <- renderDT({
            datatable(
                  data = awards_shares_p_num,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv", "pdf"),
                                 scrollX = TRUE,
                                 search = list(regex = TRUE),
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtlip",
                                 initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C6011F', 'color': '#FFF'});",
                                       "}"
                                 ))
            )
      })
      
      
})






