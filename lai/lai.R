## LAI Intermittent assessment

# Loading packages ---- 

library(tidyverse)

# Loading data ---- 

data <- read_csv("lai/lai-data.csv")

# Single axis ---- 

sc1_single <- data %>%
  filter(scenario == 1) %>%
  select(veg_cover, max_pbl, sens_heat, lat_heat, grd_heat) %>%
  gather(type, value, select = 3:5)

(plot <- ggplot(sc1_single, aes(x=veg_cover)) +
  geom_area(data = sc1_single, aes(y = value, fill = type), alpha=.4) + 
  theme_bw() + 
  theme(panel.grid = element_blank()))

# Test adding another axes ---- 

sc1 <- data %>%
  filter(scenario == 1) %>%
  select(veg_cover, max_pbl, sens_heat, lat_heat, grd_heat) %>%
  gather(type, value, select = 3:5)

View(sc1)

ggplot(data = sc1) + 
  geom_line(aes(x = veg_cover, y = max_pbl)) +
  geom_area(aes(x = veg_cover, y = value, fill = type), alpha = 0.4) + 
  scale_y_continuous(
    name = "Radiation value", 
    sec.axis = sec_axis(~ . + 900, name ="Maximum PBL height (m)"), 
    limits = c(0, 800)) + 
  theme_bw() + 
  theme(panel.grid = element_blank())
    
# Above graph is problematic because its not letting me plot my geom_line and geom_points
# Although the graph below works??? 

ggplot(data = sc1) + 
  geom_line(aes(x = veg_cover, y = max_pbl)) +
  geom_point(aes(x = veg_cover, y = max_pbl)) + 
  theme_bw() + 
  theme(panel.grid = element_blank())

# Trying to subset with same[1:6,1:2] still doesnt allow my rows to come out!!


