# Loading libraries
library(tidyverse)
library(ggplot2)
library(rworldmap)
library(sf)
library(tmap)

####################################
# Merge csvs
input_folder <- "output/ModelSummary/test/"
files <- dir(input_folder, "^.*\\.csv$", full.names = TRUE)
merged <- plyr::ldply(files, readr::read_csv)
colnames(merged)[1] <- "year_range"

# Removing species with Rhat > 1.1
model_sum <- merged %>% 
  filter(Rhat < 1.1) 

# Export output
write.csv(model_sum, "output/model_summary_2yr_test.csv")

####################################
# Figures
####################################
current <- read_csv("output/model_summary_2yr_test.csv")
colnames(current)[2] <- "trend_test"
current <- current[,1:2]

prev <- read_csv("output/model_summary_2yr.csv")
prev <- prev[,1:2]

com <- dplyr::right_join(prev, current, by = "species")
com <- na.omit(com)

cor(com$mean_trend, com$trend_test) # 0.9377363

plot_data <- data.frame(
  species = rep(com$species, each = 2),
  x = c(com$mean_trend, com$trend_test),
  y = c(com$trend_test, com$mean_trend),
  type = rep(c("mean_trend", "trend_test"), times = nrow(com)),
  pair_type = rep(c("mean_vs_test", "test_vs_mean"), times = nrow(com))
)

# Step 3: Create the scatterplot with 12 points
ggplot(plot_data, aes(x = x, y = y, color = type)) +
  geom_point() +
  labs(x = "Trend with all data",
       y = "Trend with revised data") +
  theme_classic() + 
  scale_color_manual(values = c("darkgoldenrod1", "blue")) +
  geom_smooth(method = "lm") +
  theme(legend.position = "top")
