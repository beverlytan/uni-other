## LAI Intermittent assessment

# Loading packages ---- 

library(tidyverse)
library(gridExtra)

# Loading data ---- 

data <- read_csv("lai/lai-data.csv")

# Extracting data by scenario ---- 

sc1 <- data %>%
  filter(scenario == 1) %>%
  select(veg_cover, max_pbl, sens_heat, lat_heat, grd_heat) %>%
  gather(type, value, select = 3:5)

sc2 <- data %>%
  filter(scenario == 2) %>%
  select(veg_cover, max_pbl, sens_heat, lat_heat, grd_heat) %>%
  gather(type, value, select = 3:5)

sc3 <- data %>%
  filter(scenario == 3) %>%
  select(veg_cover, max_pbl, sens_heat, lat_heat, grd_heat) %>%
  gather(type, value, select = 3:5)

# Making the graphs, two axes ---- 

(p1 <- ggplot(data = sc1) + 
  geom_point(aes(x = veg_cover, y = max_pbl - 800)) +
  geom_line(aes(x = veg_cover, y = max_pbl - 800)) +
  geom_area(aes(x = veg_cover, y = value, fill = type), alpha = 0.4) + 
  scale_fill_manual(name = "Heat flux", 
                    values = c("#fed976", "#feb24c", "#d95f0e"),
                    labels = c("Ground", "Latent", "Sensible")) + 
  scale_x_reverse(name = "\n Vegetation cover (%)", 
                  breaks = c(100, 80, 60, 40, 20, 0)) + 
  scale_y_continuous(
    name = "Radiation value \n", 
    sec.axis = sec_axis(~ . + 800, name ="Maximum PBL height (m) \n"), 
    limits = c(0, 800)) + 
  theme_bw() + 
  theme(panel.grid = element_blank()))

(p2 <- ggplot(data = sc2) + 
  geom_point(aes(x = veg_cover, y = max_pbl - 800)) +
  geom_line(aes(x = veg_cover, y = max_pbl - 800)) +
  geom_area(aes(x = veg_cover, y = value, fill = type), alpha = 0.4) + 
  scale_fill_manual(name = "Heat flux", 
                    values = c("#fed976", "#feb24c", "#d95f0e"),
                    labels = c("Ground", "Latent", "Sensible")) + 
  scale_x_reverse(name = "\n Vegetation cover (%)", 
                  breaks = c(100, 80, 60, 40, 20, 0)) + 
  scale_y_continuous(
    name = "Radiation value \n", 
    sec.axis = sec_axis(~ . + 800, name ="Maximum PBL height (m) \n"), 
    limits = c(0, 800)) + 
  theme_bw() + 
  theme(panel.grid = element_blank()))

(p3 <- ggplot(data = sc3) + 
  geom_point(aes(x = veg_cover, y = max_pbl - 800)) +
  geom_line(aes(x = veg_cover, y = max_pbl - 800)) +
  geom_area(aes(x = veg_cover, y = value, fill = type), alpha = 0.4) + 
  scale_fill_manual(name = "Heat flux", 
                    values = c("#fed976", "#feb24c", "#d95f0e"),
                    labels = c("Ground", "Latent", "Sensible")) + 
  scale_x_reverse(name = "\n Vegetation cover (%)", 
                  breaks = c(100, 80, 60, 40, 20, 0)) + 
  scale_y_continuous(
    name = "Radiation value \n", 
    sec.axis = sec_axis(~ . + 800, name ="Maximum PBL height (m) \n"), 
    limits = c(0, 800)) + 
  theme_bw() + 
  theme(panel.grid = element_blank()))

## Merging the three

# To combine the plots and have a common legend: 
# https://cran.r-project.org/web/packages/lemon/vignettes/legends.html

legend <- g_legend(p1 + theme(legend.position = (c(0.5,0.5)),
                              legend.box.background = element_rect(color = "grey", size = 0.3)))

grid.arrange(p1+theme(legend.position='hidden'), p2+theme(legend.position='hidden'),
             p3+theme(legend.position='hidden'), legend)




# Trying the double plot ---- 

pbl1 <- sc1 %>%
  select(veg_cover, max_pbl) %>%
  distinct()

pbl2 <- sc2 %>%
  select(veg_cover, max_pbl) %>%
  distinct()

pbl3 <- sc3 %>%
  select(veg_cover, max_pbl) %>%
  distinct()

heat1 <- sc1 %>%
  select(veg_cover, type, value)

heat2 <- sc2 %>%
  select(veg_cover, type, value)

heat3 <- sc3 %>%
  select(veg_cover, type, value)

p1a <- ggplot(pbl1) + 
  geom_point(aes(x = veg_cover, y = max_pbl)) +
  geom_line(aes(x = veg_cover, y = max_pbl)) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  scale_x_reverse(name = NULL, breaks = c(100, 80, 60, 40, 20, 0)) + 
  scale_y_continuous(
    name = "Max PBL height (m)", 
    limits = c(1000, 1600))

p1b <- ggplot(heat1) + 
  geom_area(aes(x = veg_cover, y = value, fill = type), alpha = 0.4) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  scale_x_reverse(name = NULL, breaks = c(100, 80, 60, 40, 20, 0)) + 
  scale_fill_manual(name = "Heat flux", 
                    values = c("#fed976", "#feb24c", "#d95f0e"),
                    labels = c("Ground", "Latent", "Sensible")) + 
  scale_y_continuous(
    name = "Energy (W/m2)", 
    limits = c(0, 800),
    breaks = c(0, 200, 400, 600, 800))

p2a <- ggplot(pbl2) + 
  geom_point(aes(x = veg_cover, y = max_pbl)) +
  geom_line(aes(x = veg_cover, y = max_pbl)) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  scale_x_reverse(name = NULL, breaks = c(100, 80, 60, 40, 20, 0)) + 
  scale_y_continuous(
    name = NULL, 
    limits = c(1000, 1600))

p2b <- ggplot(heat2) + 
  geom_area(aes(x = veg_cover, y = value, fill = type), alpha = 0.4) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  scale_x_reverse(name = NULL, breaks = c(100, 80, 60, 40, 20, 0)) + 
  scale_fill_manual(name = "Heat flux", 
                    values = c("#fed976", "#feb24c", "#d95f0e"),
                    labels = c("Ground", "Latent", "Sensible")) + 
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800),
    limits = c(0, 800),
    name = NULL)

p3a <- ggplot(pbl3) + 
  geom_point(aes(x = veg_cover, y = max_pbl)) +
  geom_line(aes(x = veg_cover, y = max_pbl)) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  scale_x_reverse(name = NULL, breaks = c(100, 80, 60, 40, 20, 0)) + 
  scale_y_continuous(
    name = NULL, 
    limits = c(1000, 1600))

p3b <- ggplot(heat3) + 
  geom_area(aes(x = veg_cover, y = value, fill = type), alpha = 0.4) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  scale_x_reverse(name = NULL, breaks = c(100, 80, 60, 40, 20, 0)) + 
  scale_fill_manual(name = "Heat flux", 
                    values = c("#fed976", "#feb24c", "#d95f0e"),
                    labels = c("Ground", "Latent", "Sensible")) + 
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800),
    limits = c(0, 800),
    name = NULL)

legend <- g_legend(p1b + theme(legend.position = "right"))

grid.arrange(p1a + ggtitle("Scenario A"),
             p2a + ggtitle("Scenario B"),
             p3a + ggtitle("Scenario C"), 
             nullGrob(), 
             p1b + theme(legend.position='hidden'),
             p2b + theme(legend.position='hidden'),
             p3b + theme(legend.position='hidden'), legend,
             bottom = "Vegetation cover (%)", 
             nrow = 2, ncol = 4)

