# statrank density plots



library(tidyverse)
library(broom)
library(rlang)

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

hof_bat <- read_rds("data/13 - HOF Batting.rds")
fran_bat <- read_rds("data/13 - Franchise Batting.rds")

hof_pit <- read_rds("data/13 - HOF Pitching.rds")
fran_pit <- read_rds("data/13 - Franchise Pitching.rds")



# Just the graph, maam ==================================
a1 <- ggplot(data = pitWandJ, aes(x = redsJAWS)) +
      geom_density(fill = "#000000", alpha = 0.7)

d1 <- ggplot_build(a1)$data[[1]]

p1 <- a1 + geom_area(data = subset(d1, x < 3.5), aes(x = x, y = y), fill = "#C6011F", alpha = 0.75)

p1


# The control =============================================


df <- tradFranBat %>%
      select(bbref_playerId, OBP) %>% 
      group_by(bbref_playerId) %>%
      do(tidy(density(tradFranBat$OBP, bw = "nrd0"))) %>% 
      group_by() %>% 
      mutate(ymin = max(y) / 1.5, 
             ymax = y + ymin,
             ylabel = ymin + min(ymin)/2,
             xlabel = min(x) - mean(range(x))/2)

labels <- tradFranBat %>% 
      select(bbref_playerId, OBP) %>% 
      group_by(bbref_playerId) %>% 
      mutate(q1 = quantile(OBP)[2],
             median = quantile(OBP)[3],
             q3 = quantile(OBP)[4]) %>%
      filter(row_number() == 1) %>% 
      select(-OBP) %>% 
      left_join(df) %>% 
      mutate(xmed = x[which.min(abs(x - median))],
             yminmed = ymin[which.min(abs(x - median))],
             ymaxmed = ymax[which.min(abs(x - median))]) %>% 
      filter(row_number() == 1)     


a <- ggplot(data = tradFranBat, aes(x = OBP)) +
      geom_density(fill = "#000000", alpha = 0.7) +
      theme(axis.text.y = element_blank())

d <- ggplot_build(a)$data[[1]]

p <- a + geom_area(data = subset(d, x < .300), aes(x = x, y = y), fill = "#C6011F", alpha = 0.75) +
      geom_segment(data = labels[labels$bbref_playerId == "adamsbo03",], aes(x = xmed, xend = xmed, y = 0, yend = ymaxmed), colour = "#F0F0F0", linetype = "dashed") +
      geom_text(data = labels[labels$bbref_playerId == "adamsbo03",], aes(xmed - xlabel/50, ylabel), 
                label = "Median", colour = "#000000", hjust = 0, fontface = "italic", size = 4)

p



# The experimental ============================================

# Haven't figured out how to make the xaxis display correct precision for every stat yet

# Density calculation
df2 <-  hof_bat %>%
      select(Name, `wRC+`) %>% 
      group_by(Name) %>%
      do(tidy(density(hof_bat$`wRC+`, bw = "nrd0", na.rm = TRUE))) %>% 
      group_by() %>% 
      mutate(ymin = max(y) / 1.5, 
             ymax = y + ymin,
             ylabel = ymin + min(ymin)/2,
             xlabel = min(x) - mean(range(x))/2)

# Median label coords using vars from density calc
labels2 <- hof_bat %>% 
      select(Name, `wRC+`) %>% 
      mutate(median = quantile(`wRC+`, na.rm = TRUE)[3]) %>%
      filter(row_number() == 1) %>% 
      select(-`wRC+`) %>% 
      left_join(df2) %>% 
      mutate(xmed = x[which.min(abs(x - median))],
             yminmed = ymin[which.min(abs(x - median))],
             ymaxmed = ymax[which.min(abs(x - median))]) %>% 
      filter(row_number() == 1)     

# Player stat for shaded area
shade_bdy <- hof_bat %>% 
      filter(Name == "Adam Dunn")
shade_bdy <- shade_bdy$`wRC+`[1]

# 3 decimal places needed for stats like OBP
scaleFUN <- function(x) {sprintf("%.3f", x)}

# Frame
a <- ggplot(data = hof_bat, aes(x = `wRC+`)) +
      geom_density(fill = "#000000", alpha = 0.7) +
      # scale_y_continuous(limits = c(0, max(bench_war$WAR)+5)) +
      scale_x_continuous() +
      theme(axis.text.y = element_blank())

d <- ggplot_build(a)$data[[1]]

# Shaded area, median line, median label
p <- a + geom_area(data = subset(d, x < shade_bdy), aes(x = x, y = y), fill = "#C6011F", alpha = 0.75) +
      
      geom_segment(data = labels2, aes(x = xmed, xend = xmed, y = 0, yend = ymaxmed/1.77), colour = "#F0F0F0", linetype = "dashed") +
      
      geom_text(data = labels2, aes(xmed - xlabel/50, (ylabel*0.05 + ylabel)), 
                label = "Median", colour = "#000000", hjust = 0, fontface = "italic", size = 4) +
      geom_rug() +
      labs(title = "HOF")

p



# Functionize ========================================

hof_density_FUN <- function(play_sel, col_sel) {
      
      # Density calculation
      df2 <-  fran_pit %>%
            select(Name) %>% 
            group_by(Name) %>%
            do(tidy(density(fran_pit[, col_sel][[1]], bw = "nrd0", na.rm = TRUE))) %>% 
            group_by() %>% 
            mutate(ymin = max(y) / 1.5, 
                   ymax = y + ymin,
                   ylabel = ymin + min(ymin)/2,
                   xlabel = min(x) - mean(range(x))/2)
      
      # Median label coords using vars from density calc
      labels2 <- fran_pit %>% 
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
      shade_bdy <- fran_pit %>% 
            filter(Name == play_sel)
      shade_bdy <- shade_bdy[, col_sel][[1]]
      
      
      if(col_sel %in% c("W-L%", "ERA-", "FIP-", "xFIP-", "K/9", "BB/9", "K/BB", "HR/9", "K%", "BB%", "K-BB%", "ERA+")) {
            col_sel <- paste0("`", col_sel, "`")
      } else {
            col_sel
      }
      
      # frame
      a <- ggplot(data = fran_pit, aes_string(x = quo_name(col_sel))) +
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

hof_density_FUN("Tom Browning", "ERA+")


