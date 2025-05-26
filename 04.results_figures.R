# Loading libraries
library(tidyverse)
library(ggplot2)
library(rworldmap)
library(sf)
library(tmap)

####################################
# Merge csvs
input_folder <- "output/ModelSummary/2yr/"
files <- dir(input_folder, "^.*\\.csv$", full.names = TRUE)
merged <- plyr::ldply(files, readr::read_csv)
colnames(merged)[1] <- "year_range"

# Removing species with Rhat > 1.1
model_sum <- merged %>% 
  filter(Rhat < 1.1) 

# Export output
write.csv(model_sum, "output/model_summary_2yr.csv")

####################################
# # Number of records vs Rhat value
# # Importing required files
# data <- read_rds("data/complete_data_carabid_2yr.rds")
# model_sum <- read_csv("output/model_summary_2yr.csv")
# colnames(model_sum)[1] <- "species"
# 
# # Total records
# sp_rec <- data %>% 
#   group_by(species) %>% 
#   summarise(all_year = NROW(species))
# 
# # Records by year range
# # sp_rec_yr <- data %>% 
# #   group_by(species, year_range) %>% 
# #   summarise(rec = NROW(species))
# # 
# # sp_rec_yr <- sp_rec_yr %>% 
# #   pivot_wider(names_from = 'year_range', values_from = 'rec')
# # 
# # colnames(sp_rec_yr) <- c("species", "yr1", "yr2", "yr3", "yr4")
# 
# # Merging year-range and total records
# sp_rec_up <- dplyr::left_join(sp_rec, model_sum, by = c("species"))

####################################
# Figures
####################################
# Rhat and number of total occurrence records
# # Grouping data by years
# ggplot(sp_rec_up, aes(all_year, Rhat)) +
#   geom_point(col = "blue", alpha = 0.3) + theme_classic() +
#   xlab("Number of occurrence records (all year)") +
#   geom_smooth()
# 
# ggsave("output/Rhat_n_rec_2yr.png")

# Figure 1
# Number of records (and survey quadrants) by year
data <- read_rds("data/complete_data_carabid_2yr.rds")

year_mtb <- data %>% 
  group_by(year) %>% 
  summarise(n = NROW(unique(site)))

g1 <- ggplot(data, aes(year)) +
  geom_bar() + xlab("") + ylab("Number of occurrence records per year") + theme_classic() +
  scale_x_continuous(breaks = c(1992, 2002, 2012, 2022))

g2 <- ggplot(year_mtb, aes(year, n)) +
  geom_bar(stat = "identity") + xlab("") + ylab("Number of survey quadrants per year") + theme_classic() +
  scale_x_continuous(breaks = c(1992, 2002, 2012, 2022))

cowplot::plot_grid(g1, g2)

ggsave("output/yearly_quad_rec_2yr.png")

# Year-wise records [distribution]
# Getting German map data
world <- getMap(resolution = "low")
germany <- world[world@data$NAME %in% "Germany", ] 

ggplot(data, aes(lon, lat, col = year_group)) +
  geom_point(size = 0.01) + theme_classic() +
  xlab("Longitude") +ylab("Latitude") + scale_color_viridis_c() +
  geom_polygon(data = germany, aes(x = long, y = lat, group = group),fill = NA, colour = "black") +
  coord_quickmap() + xlim(5, 16) + ylim(46, 56) + 
  theme(legend.title = element_blank())

ggsave("output/year-wise_rec_map_2yr.png")

######################################################
# Distribution by MTB grids
data_sum <- data %>% 
  group_by(site, year_group) %>% 
  summarise(n = NROW(site)) %>% 
  mutate(n_yr = NROW(n)) %>% 
  select(site, n_yr) %>% 
  distinct() %>% 
  group_by(n_yr) %>% 
  summarise(n = NROW(site))

# Exporting output
write_csv(data_sum, "output/n_yr_grid.csv")

####################################
# Figure 2
# Importing data
trend_sp <- read_csv("output/model_summary_2yr.csv")

# Overall trend
ggplot(trend_sp,aes(fct_reorder(species, mean_trend), mean_trend, fill = trend_status, col = trend_status)) +
  geom_bar(stat="identity", position="identity") + xlab("Species") + ylab("Long-term trend") + theme_classic() +
  scale_fill_manual(values = c("grey", "darkgoldenrod1", "deepskyblue1")) +
  scale_color_manual(values = c("grey", "darkgoldenrod1", "deepskyblue1")) +
  theme(legend.title = element_blank(),
        legend.position = "top", axis.text.x = element_blank())

ggsave("output/trends.png")

#####################################
# Importing shp
data <- st_read("data/layer/carabid_int/carabid_int.shp")

data_sum <- data %>% 
  group_by(NAME) %>% 
  summarise(n = NROW(year))

data_sum <- data_sum %>%
  mutate(quantile=cut(n, breaks=c(0, 11, 35, 70, 100, 500, 24117),
                      labels=c(1, 2, 3, 4, 5, 6)))

# Convert to sf object
sf_data <- st_as_sf(data_sum, wkt = "geometry")

ggplot(data = sf_data) +
  geom_sf(aes(col = quantile, fill = quantile)) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +  # Use a continuous color scale
  theme_classic() +
  xlab("Longitude") + ylab("Latitude") + theme(legend.title = element_blank())

ggsave("output/dist_data_grid.png")

