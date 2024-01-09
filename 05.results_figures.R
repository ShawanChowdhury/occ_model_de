# Loading libraries
library(tidyverse)
library(ggplot2)

####################################
# Merge csvs
input_folder <- "output/ModelSummary/"
files <- dir(input_folder, "^.*\\.csv$", full.names = TRUE)
merged <- plyr::ldply(files, readr::read_csv)
write.csv(merged, "output/model_summary.csv")

####################################
# Number of records vs Rhat value
# Importing required files
data <- read_rds("data/complete_data_carabid.rds")
model_sum <- read_csv("output/model_summary_up.csv")
colnames(model_sum)[1] <- "species"

# Total records
sp_rec <- data %>% 
  group_by(species) %>% 
  summarise(all_year = NROW(species))

# Records by year range
sp_rec_yr <- data %>% 
  group_by(species, year_range) %>% 
  summarise(rec = NROW(species))

sp_rec_yr <- sp_rec_yr %>% 
  pivot_wider(names_from = 'year_range', values_from = 'rec')

colnames(sp_rec_yr) <- c("species", "yr1", "yr2", "yr3", "yr4")

# Merging year-range and total records
sp_rec_up <- dplyr::left_join(sp_rec, sp_rec_yr, by = c("species"))

# Merging records with the model summary
model_sum_rec <- dplyr::left_join(model_sum, sp_rec_up, by = c("species"))

# Exporting output
write_csv(model_sum_rec, "output/model_sum_rec.csv")

####################################
# Selecting species with Rhat < 1.2
model_sum_rec_up <- model_sum_rec %>% 
  filter(Rhat <= 1.1)

colnames(model_sum_rec_up)[2] <- "mean_trend"

write_csv(model_sum_rec_up, "output/model_sum_rec_up.csv")

####################################
# Figures
####################################
# Rhat and number of total occurrence records
ggplot(model_sum_rec, aes(all_year, Rhat)) +
  geom_point(col = "blue", alpha = 0.3) + theme_classic() +
  geom_smooth() + 
  xlab("Number of occurrence records (all year)") + ylab("Rhat")

ggsave("output/Rhat_n_rec.png")

# Number of records by year
ggplot(data, aes(year)) +
  geom_bar() + xlab("") + ylab("Number of occurrence records") + theme_classic() +
  scale_x_continuous(breaks = c(1974, 1986, 1998, 2010, 2022))

ggsave("output/yearly_rec.png")

# Number of survey quadrants by year
year_mtb <- data %>% 
  group_by(year) %>% 
  summarise(n = NROW(site))

ggplot(year_mtb, aes(year, n)) +
  geom_bar(stat = "identity") + xlab("") + ylab("Number of survey quadrants") + theme_classic() +
  scale_x_continuous(breaks = c(1974, 1986, 1998, 2010, 2022))

ggsave("output/yearly_quad.png")

# Year-wise records [distribution]
# Getting German map data
library(rworldmap)
world <- getMap(resolution = "low")
germany <- world[world@data$NAME %in% "Germany", ] 

ggplot(data, aes(lon, lat, col = year_range)) +
  geom_point(size = 0.01) + theme_classic() +
  xlab("Longitude") +ylab("Latitude") + scale_color_viridis_c() +
  geom_polygon(data = germany, aes(x = long, y = lat, group = group),fill = NA, colour = "black") +
  coord_quickmap() + xlim(5, 16) + ylim(46, 56) + 
  theme(legend.title = element_blank())

ggsave("output/year-wise_rec_map.png")

# Overall trend
ggplot(model_sum_rec_up, aes(species, mean_trend)) +
  geom_histogram() + xlab("") + ylab("") + theme_classic()

ggplot(model_sum_rec_up,aes(fct_reorder(species, mean_trend), mean_trend, fill = colour, col = colour)) +
  geom_bar(stat="identity", position="identity") + xlab("Species") + ylab("Long-term trend") + theme_classic() +
  scale_fill_manual(values = c("darkgoldenrod1", "skyblue3")) +
  scale_color_manual(values = c("darkgoldenrod1", "skyblue3")) +
  theme(legend.title = element_blank(),
        legend.position = "top", axis.text.x = element_blank())

ggsave("output/trends.png")

# Significance status
data <- read_csv("output/model_sum_rec_up.csv")

ggplot(data,aes(fct_reorder(species, mean_trend), mean_trend, 
                fill = significance_status, col = significance_status)) +
  geom_bar(stat="identity", position="identity") + xlab("Species") + ylab("Long-term trend") + theme_classic() +
  scale_fill_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
  scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
  theme(legend.title = element_blank(),
        legend.position = "top", axis.text.x = element_blank())

ggsave("output/trends_significance.png")

####################################
# Trait and Trends

trait <- read_csv("data/carabid_traits.csv")
trend <- read_csv("output/model_sum_rec_up.csv")

trend_trait <- dplyr::left_join(trend, trait, by = c("species"))

write_csv(trend_trait, "output/trend_trait_carabid.csv")

ggplot(trend_trait, aes(meanSize, mean_trend)) +
  geom_point(col = "blue", alpha = 0.3) + theme_classic() +
  geom_smooth(method = "lm") + 
  xlab("Body size") + ylab("Long-term trend")

ggplot(trend_trait, aes(wings, mean_trend, col = wings)) +
  geom_point() + theme_classic() +
  geom_smooth(method = "lm") + 
  xlab("Body size") + ylab("Long-term trend")

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
