# Loading libraries
library(tidyverse)
library(ggplot2)

####################################
# Merge csvs
input_folder <- "output/ModelSummary/2yr/"
files <- dir(input_folder, "^.*\\.csv$", full.names = TRUE)
merged <- plyr::ldply(files, readr::read_csv)
colnames(merged)[1] <- "year_range"

# Removing species with Rhat > 1.1
model_sum <- merged %>% 
  filter(Rhat < 1.1) 

species_list <- model_sum %>% 
  group_by(species_name) %>% 
  summarise(n = NROW(species_name)) %>% 
  filter(n > 1) %>% 
  select(species_name)

model_sum <- dplyr::left_join(species_list, model_sum, by = "species_name")

# Export output
write.csv(model_sum, "output/model_summary_2yr.csv")

####################################
# Mean trend by year-range
model_sum <- read_csv("output/model_summary_2yr.csv")

# Modifying the dataframe
model_sum$year_range <- as.character(model_sum$year_range)
model_sum$group <- "Individual"

# Summary values
mean_trend_yr <- model_sum %>% 
  group_by(year_range) %>% 
  summarise(mean = mean(mean),
            sd = mean(sd),
            lower_5 = mean(lower_5),
            upper_95 = mean(upper_95),
            Rhat = mean(Rhat)) %>% 
  mutate(species_name = "All")

# Adding new columns
mean_trend_yr$group <- "All"

# Combining dataframes
model_sum <- rbind(model_sum, mean_trend_yr)

# Export output
write.csv(model_sum, "output/mean_trend_yr.csv")

####################################
# Overall mean trend by species
model_sum <- read_csv("output/model_summary_2yr.csv")

trend_sp <- model_sum %>% 
  group_by(species_name) %>% 
  summarise(mean = mean(mean),
            sd = mean(sd),
            lower_5 = mean(lower_5),
            upper_95 = mean(upper_95),
            Rhat = mean(Rhat))

# Export output
write.csv(trend_sp, "output/trend_sp.csv")

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

# ####################################
# # Selecting species with Rhat < 1.2
# model_sum_rec_up <- model_sum_rec %>% 
#   filter(Rhat <= 1.1)
# 
# colnames(model_sum_rec_up)[2] <- "mean_trend"
# 
# write_csv(model_sum_rec_up, "output/model_sum_rec_up.csv")

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
library(rworldmap)
world <- getMap(resolution = "low")
germany <- world[world@data$NAME %in% "Germany", ] 

ggplot(data, aes(lon, lat, col = year_group)) +
  geom_point(size = 0.01) + theme_classic() +
  xlab("Longitude") +ylab("Latitude") + scale_color_viridis_c() +
  geom_polygon(data = germany, aes(x = long, y = lat, group = group),fill = NA, colour = "black") +
  coord_quickmap() + xlim(5, 16) + ylim(46, 56) + 
  theme(legend.title = element_blank())

ggsave("output/year-wise_rec_map_2yr.png")

####################################
# Trend by year-range

# Importing data
mean_trend_yr <- read_csv("output/mean_trend_yr.csv")

ggplot(mean_trend_yr, aes(year_range, mean, color = group)) +
  geom_point()

####################################
# Importing data
trend_sp <- read_csv("output/trend_sp.csv")

# Overall trend
ggplot(trend_sp,aes(fct_reorder(species, mean_trend), mean_trend, fill = trend_status, col = trend_status)) +
  geom_bar(stat="identity", position="identity") + xlab("Species") + ylab("Long-term trend") + theme_classic() +
  scale_fill_manual(values = c("darkgoldenrod1", "skyblue3")) +
  scale_color_manual(values = c("darkgoldenrod1", "skyblue3")) +
  theme(legend.title = element_blank(),
        legend.position = "top", axis.text.x = element_blank())

ggsave("output/trends.png")

# Significance status
ggplot(trend_sp,aes(fct_reorder(species, mean_trend), mean_trend, 
                fill = significance_status, col = significance_status)) +
  geom_bar(stat="identity", position="identity") + xlab("Species") + ylab("Long-term trend") + theme_classic() +
  scale_fill_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
  scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
  theme(legend.title = element_blank(),
        legend.position = "top", axis.text.x = element_blank())

ggsave("output/trends_significance_2yr.png")

####################################
# Trait and Trends

trait <- read_csv("data/carabid_traits.csv")
trend <- read_csv("output/model_summary_2yr_up.csv")

trend_trait <- dplyr::left_join(trend, trait, by = c("species"))

write_csv(trend_trait, "output/trend_trait_carabid_2yr.csv")

ggplot(trend_trait, aes(meanSize, mean_trend)) +
  geom_point(col = "blue", alpha = 0.3) + theme_classic() +
  geom_smooth(method = "lm") + 
  xlab("Body size") + ylab("Long-term trend")

# ggplot(trend_trait, aes(meanSize, mean_trend, col = wings)) +
#   geom_point() + theme_classic() + 
#   xlab("Body size") + ylab("Long-term trend")

############################
# Number of records vs trends
trend_trait <- read_csv("output/trend_trait_carabid.csv")

# Quantile: 1.0  16.5  51.0 133.0 382.0
trend_trait <- trend_trait %>%
  filter(yr1 > 0) %>% 
  mutate(yr1_group = cut(yr1, breaks=c(0, 17, 40, 70, 133, 383),
                       labels=c("Very low","Low", "Medium", "High", "Very High")))

g1 <- ggplot(trend_trait, aes(yr1, mean_trend)) +
  geom_point(aes(col = significance_status), alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_classic() + 
  xlab("Number of occurrence records (1974-1985)") + ylab("Long-term trend") +
  scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
  theme(legend.title = element_blank(),
        legend.position = "none")

g2 <- ggplot(trend_trait, aes(yr2, mean_trend)) +
  geom_point(aes(col = significance_status), alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_classic() + 
  xlab("Number of occurrence records (1986-1997)") + ylab("Long-term trend") +
  scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
  theme(legend.title = element_blank(),
        legend.position = "none")

g3 <- ggplot(trend_trait, aes(yr3, mean_trend)) +
  geom_point(aes(col = significance_status), alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_classic() + 
  xlab("Number of occurrence records (1998-2009)") + ylab("Long-term trend") +
  scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
  theme(legend.title = element_blank(),
        legend.position = "none")

g4 <- ggplot(trend_trait, aes(yr4, mean_trend)) +
  geom_point(aes(col = significance_status), alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_classic() + 
  xlab("Number of occurrence records (2010-2021)") + ylab("Long-term trend") +
  scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
  theme(legend.title = element_blank(),
        legend.position = "none")

cowplot::plot_grid(g1, g2, g3, g4)

ggsave("output/yearr_trend.png")

####################################
# Trait and Trends [All species]
trait <- read_csv("data/carabid_traits.csv")
trend <- read_csv("output/model_summary_2yr_up.csv")

trend_trait <- dplyr::full_join(trend, trait, by = c("species"))

write_csv(trend_trait, "output/trend_trait_carabid_2yr_full.csv")
