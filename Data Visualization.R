# Data Visualization
# Alice Lepissier
# alice.lepissier@gmail.com

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble

## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

#setwd("C:/cloudstorage/googledrive/Projects/Tax Justice Network/Consultancy 2 - summer 18/Risk-based IFF/") # Alice work
setwd("D:/Google Drive/Projects/Tax Justice Network/Consultancy 2 - summer 18/Risk-based IFF/") # Alice laptop
#library(plyr) # Must load before dplyr
#library(dplyr)
library(ggplot2)
#library(gmodels)
#library(RColorBrewer)
#library(rebus)
#library(reshape2)
#library(scales)
#library(tidyr)
library(tidyverse)
#library(WDI)
#library(wesanderson)
library(xlsx)



## ## ## ## ## ## ## ## ## ## ##
# CODES MASTERLIST          ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read.xlsx2("Data/Codes_Masterlist.xlsx", sheetName = "Codes") %>%
  mutate_all(as.character)



## ## ## ## ## ## ## ## ## ## ##
# CARTOGRAM                 ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge geographic data ####
map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(region != "Antarctica")

ditch_axes <- theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank())

data <- read.csv("Results/VIE averages_for jurisdictions_Secrecy Score.csv") %>%
  select(reporter, variable, value)


# .. Vulnerability in Trade ####
viz <- left_join(data, codes %>% select(Country, ISO3166.3),
                  by = c("reporter" = "Country"))  %>%
  filter(variable == "xVTrade")

viz <- left_join(map, viz,
                 by = c("ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = value), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("Vulnerability", direction = -1) +
  labs(title = "Vulnerability in trade averaged over 2008-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/World V Trade.png",
       width = 6, height = 5, units = "in")


# .. Exposure in Trade ####
viz <- left_join(data, codes %>% select(Country, ISO3166.3),
                 by = c("reporter" = "Country"))  %>%
  filter(variable == "xETrade") %>%
  mutate(value = ifelse(value == 0, NA, value))

viz <- left_join(map, viz,
                 by = c("ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = value), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("Exposure", direction = -1) +
  labs(title = "Exposure in trade averaged over 2008-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/World E Trade.png",
       width = 6, height = 5, units = "in")


# .. Vulnerability in Banking ####
viz <- left_join(data, codes %>% select(Country, ISO3166.3),
                 by = c("reporter" = "Country"))  %>%
  filter(variable == "xVBanking")

viz <- left_join(map, viz,
                 by = c("ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = value), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("Vulnerability", direction = -1) +
  labs(title = "Vulnerability in banking averaged over 2008-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/World V Banking.png",
       width = 6, height = 5, units = "in")


# .. Exposure in Banking ####
viz <- left_join(data, codes %>% select(Country, ISO3166.3),
                 by = c("reporter" = "Country"))  %>%
  filter(variable == "xEBanking") %>%
  mutate(value = ifelse(value == 0, NA, value))

viz <- left_join(map, viz,
                 by = c("ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = log10(value)), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("Exposure (log base 10)", direction = -1) +
  labs(title = "Exposure in banking averaged over 2008-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/World E Banking.png",
       width = 6, height = 5, units = "in")


# .. Vulnerability in Direct Investment ####
viz <- left_join(data, codes %>% select(Country, ISO3166.3),
                 by = c("reporter" = "Country"))  %>%
  filter(variable == "xVDirectInv")

viz <- left_join(map, viz,
                 by = c("ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = value), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("Vulnerability", direction = -1) +
  labs(title = "Vulnerability in direct investment averaged over 2008-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/World V Direct Investment.png",
       width = 6, height = 5, units = "in")


# # .. Exposure in Banking ####
# viz <- left_join(data, codes %>% select(Country, ISO3166.3),
#                  by = c("reporter" = "Country"))  %>%
#   filter(variable == "xEBanking") %>%
#   mutate(value = ifelse(value == 0, NA, value))
# 
# viz <- left_join(map, viz,
#                  by = c("ISO3166.3"))
# 
# g <- ggplot() + 
#   geom_polygon(data = viz,
#                aes(x = long, y = lat, group = group, 
#                    fill = log10(value)), color = "white", lwd = 0.2) + 
#   coord_fixed(1.3) +
#   theme_bw() + 
#   ditch_axes +
#   scale_fill_viridis_c("Exposure (log base 10)", direction = -1) +
#   labs(title = "Exposure in banking averaged over 2008-2018") +
#   theme(legend.position = "bottom") + 
#   guides(fill = guide_colourbar(title.vjust = 0.8))
# ggsave(g,
#        file = "Figures/World E Banking.png",
#        width = 6, height = 5, units = "in")


# .. Vulnerability in Portfolio Investment ####
viz <- left_join(data, codes %>% select(Country, ISO3166.3),
                 by = c("reporter" = "Country"))  %>%
  filter(variable == "xVPortInv")

viz <- left_join(map, viz,
                 by = c("ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = value), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("Vulnerability", direction = -1) +
  labs(title = "Vulnerability in portfolio investment averaged over 2008-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/World V Portfolio Investment.png",
       width = 6, height = 5, units = "in")



# ## ## ## ## ## ## ## ## ## ## ##
# # FLOW MAPS                 ####
# ## ## ## ## ## ## ## ## ## ## ##
# 
# 
# # .. Flow maps of top destinations in conduits ####
# # load("Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.Rdata")
# # load("Results/Summary data-sets/GER_Orig_Dest_Avg_Developing.Rdata")
# 
# conduits_Trade <- c("LAO", "RWA", "SUR", "UGA", "MMR", "VCT", "BDI", "KGZ", "BFA", "OMN")
# 
# ditch_axes <- theme(axis.title.x = element_blank(),
#                     axis.text.x = element_blank(),
#                     axis.ticks.x = element_blank(),
#                     axis.title.y = element_blank(),
#                     axis.text.y = element_blank(),
#                     axis.ticks.y = element_blank(),
#                     panel.border = element_blank(),
#                     panel.grid = element_blank()) 
# 
# centroids <- codes %>%
#   dplyr::select(ISO3166.3, Longitude, Latitude) %>%
#   mutate_at(vars(Longitude, Latitude),
#             funs(as.numeric))
# 
# data <- read.csv("Results/panelSJ_Secrecy Score.csv") %>%
#   select(reporter, variable, value)
# 
# GER_Orig_Dest_Avg_LMIC <- GER_Orig_Dest_Avg_LMIC %>%
#   left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
#   dplyr::rename(rLongitude = Longitude,
#                 rLatitude = Latitude) %>%
#   left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
#   dplyr::rename(pLongitude = Longitude,
#                 pLatitude = Latitude) %>%
#   filter(reporter.ISO %in% conduits_LMIC) %>%
#   group_by(reporter.ISO) %>%
#   top_n(5, Tot_IFF_hi) %>%
#   ungroup() %>%
#   mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))
# 
# map <- map_data("world")
# map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3),
#                  by = c("region" = "Country")) %>%
#   dplyr::select(-subregion) %>%
#   filter(region != "Antarctica")
# 
# viz <- left_join(GER_Orig_Dest_Avg_LMIC %>% filter(reporter.ISO %in% conduits_LMIC),
#                  map,
#                  by = c("reporter.ISO" = "ISO3166.3"))
# 
# g <- ggplot() + 
#   geom_polygon(data = map,
#                aes(x = long, y = lat, group = group), fill = "grey80", col = "white", lwd = 0.2) + 
#   coord_fixed(1.3) +
#   theme_bw() + 
#   geom_curve(data = viz, 
#              aes(x = rLongitude, y = rLatitude, 
#                  xend = pLongitude, yend = pLatitude, col = reporter),
#              curvature = -0.2, lineend = "round", ncp = 20) +
#   geom_point(data = viz %>% distinct(reporter.ISO, .keep_all = T),
#              aes(x = rLongitude, y = rLatitude, col = reporter),
#              size = 4) +
#   geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
#                    aes(label = reporter, x = rLongitude, y = rLatitude),
#                    size = 3) +
#   ditch_axes +
#   guides(col = FALSE) +
#   scale_color_brewer(type = "qual", palette = "Paired") +
#   labs(title = "Destinations of top origins in low and lower middle income",
#        subtitle = "Top origin countries by % of GDP")
# ggsave(g,
#        file = "Figures/Flow map top destinations LMIC.png",
#        width = 6, height = 5, units = "in")
