# Loading libraries
library(glmmTMB)
library(broom)
library(tidyverse)
library(lme4)
library(emmeans)
library(car)

####################################
# Trait and Trends [All species]
## One species with no trait information
# Ocys tachysoides

trait <- read_csv("data/trend_trait_iucn.csv")
trend <- read_csv("output/model_summary_2yr.csv")

trend_trait <- dplyr::left_join(trend, trait, by = c("species"))

write_csv(trend_trait, "output/trend_trait_carabid_2yr.csv")

####################################
# Reading data file
data <- read_csv("output/trend_trait_carabid_2yr.csv")
data <- data %>% 
  filter(!species == c("Ocys tachysoides")) 

# It's because there was no trait information for that species

# Quantile of body size: 2, 4.5, 7, 10.5, 37
data <- data %>%
  mutate(size_type=cut(meanSize, breaks=c(1.9, 4.5, 10.5, 37),
                       labels=c("Small","Medium","Large")))

# Converting them as factors
data$habitatPref <- as.factor(data$habitatPref)
data$wings <- as.factor(data$wings)
data$size_type <- as.factor(data$size_type)
data$trophicLevel <- as.factor(data$trophicLevel)

#######################################
m1 <- lm(mean_trend ~ size_type + habitatPref + wings + trophicLevel, data = data)
summary(m1)

# Converting it to a dataframe
df1 <- broom::tidy(m1)

write_csv(df1, "output/trait_significance.csv")

#######################################
# Running models by specifying control groups
data_re <- within(data, b <- relevel(size_type, ref = "Medium"))
data_re <- within(data_re, c <- relevel(trophicLevel, ref = "Predator"))
data_re <- within(data_re, d <- relevel(habitatPref, ref = "Open"))
data_re <- within(data_re, e <- relevel(wings, ref = "Dimorphic"))

# Linear model
m2 <- lm(mean_trend ~ b + c + d + e, data = data_re)
summary(m2)

Anova(m2)

# Converting it to a dataframe
df2 <- broom::tidy(m2)

# Calculating upper and lower CIs
trend_trait <- as.data.frame(confint(m2))

trend_trait <- trend_trait %>% 
  mutate(trait = row.names(trend_trait))
colnames(trend_trait) <- c("lower_5", "upper_95", "trait")

# Combining data frames
merge <- cbind(df2, trend_trait)

merge <- merge[2:13,]

# Exporting output
write_csv(merge, "output/model_trait.csv")

# Plot
ggplot(merge, aes(term, estimate)) +
  geom_crossbar(aes(ymin = lower_5, ymax = upper_95), width = 0.2) +
  xlab("") + ylab("Effect size on trend") + theme_classic() + coord_flip() +
  geom_hline(yintercept = 0, linetype="dashed", color = "black")

ggsave("output/effect_size_trend.png")

################################################
# # Figures
# head(data)
# colnames(data)
# 
# data_long <- data %>% 
#   pivot_longer(names_to = "var",
#                cols = c("wings", "trophicLevel", "size_type", "habitatPref"),
#                values_to = "val")
# 
# # ggplot(data_long, aes(mean_trend, val)) +
# #   geom_boxplot(outlier.shape = NA) +
# #   geom_point(position = position_jitter(width = 0.01), alpha = 0.3, aes(col = significance_status)) +
# #   theme_classic() + xlab("Long-term trends") + ylab("") +
# #   scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
# #   theme(legend.title = element_blank(), legend.position = "top")
# # 
# # ggplot(data, aes(mean_trend, size_type)) +
# #   geom_boxplot(outlier.shape = NA) +
# #   geom_point(position = position_jitter(width = 0.01), alpha = 0.3, aes(col = significance_status)) +
# #   theme_classic() + xlab("Long-term trends") + ylab("") +
# #   scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
# #   theme(legend.title = element_blank(), legend.position = "top")
# 
# ggplot(data, aes(meanSize, mean_trend)) +
#   geom_point(alpha = 0.5, aes(col = trend_status)) +
#   theme_classic() + xlab("Body size (mm)") + ylab("Long-term trends") +
#   scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
#   theme(legend.title = element_blank(), legend.position = "top") + 
#   geom_smooth(method = "lm")
# 
# 
# # ggplot(data_long, aes(mean_trend, val)) +
# #   geom_boxplot() +
# #   theme_classic() + xlab("Long-term trends") + ylab("")
# 
# ggsave("output/trend_size_2yr.png")

#########################
# Figure 3
# Trend vs IUCN categories
# Reading data file
data <- read_csv("output/trend_trait_carabid_2yr.csv")

data <- data %>% 
  filter(!species == c("Ocys tachysoides")) 
# It's because there was no trait information for that species

ggplot(data, aes(thrt_status, mean_trend)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.05, alpha = 0.5, aes(col = trend_status)) +
  theme_classic() + xlab("") + ylab("Long-term trend") +
  scale_color_manual(values = c("grey", "darkgoldenrod1", "deepskyblue1")) +
  theme(legend.position = "top",
        legend.title = element_blank())

ggsave("output/trend_thrt_status.png")

#################
# Summary by IUCN categories
data_thrt <- data %>% 
  group_by(RL_IUCN, thrt_status) %>% 
  summarise(n = NROW(species), mean_trend = mean(mean_trend), median_trend = median(mean_trend))

write_csv(data_thrt, "output/trend_threat_sum.csv")

data_thrt_type <- data %>% 
  group_by(RL_IUCN, thrt_status, trend_status) %>% 
  summarise(n = NROW(species))

write_csv(data_thrt_type, "output/threat_significance_n.csv")

#################
# Figure 4
# Trend vs traits
data <- data %>% 
  filter(!species == c("Ocys tachysoides")) 

# It's because there was no trait information for that species
g1 <- ggplot(data, aes(wings, mean_trend)) +
  geom_boxplot() +
  theme_classic() + xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())

# Trend vs trophic level
g2 <- ggplot(data, aes(trophicLevel, mean_trend)) +
  geom_boxplot() +
  theme_classic() + xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())

# Trend vs body size
g3 <- ggplot(data, aes(meanSize, mean_trend)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() + xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())

# Trend vs habitat preference
g4 <- ggplot(data, aes(habitatPref, mean_trend)) +
  geom_boxplot() +
  theme_classic() + xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())

cowplot::plot_grid(g1, g2, g3, g4)

ggsave("output/trend_trait_2yr_up.png")

#################
# Summary by traits
data_trait <- data %>% 
  group_by(wings, trophicLevel, habitatPref) %>% 
  summarise(n = NROW(species), mean_trend = mean(mean_trend), median_trend = median(mean_trend))

write_csv(data_trait, "output/trend_trait_sum.csv")

# Pivot longer
data_long <- data %>% 
  pivot_longer(cols = c(wings, trophicLevel, habitatPref),
               names_to = "trait",
               values_to = "sub_trait")

# Summarise
data_long_sum <- data_long %>% 
  group_by(trait, sub_trait) %>% 
  summarise(n = NROW(species), mean_trend = mean(mean_trend),
            median_trend = median(mean_trend))

write_csv(data_long_sum, "output/trend_trait_sum_unique.csv")

########################
# Summary by traits and threat status
data_thrt_trait <- data %>% 
  group_by(RL_IUCN, thrt_status, wings, trophicLevel, habitatPref) %>% 
  summarise(n = NROW(species), mean_trend = mean(mean_trend), median_trend = median(mean_trend))

write_csv(data_thrt_trait, "output/trend_trait_threat_sum.csv")
