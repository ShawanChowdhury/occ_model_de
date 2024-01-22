# Loading libraries
library(glmmTMB)
library(broom)
library(tidyverse)
library(lme4)
library(emmeans)
library(car)

# Reading data file
data <- read_csv("output/trend_trait_carabid_2yr.csv")

data <- data %>% 
  filter(!is.na(species))

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
data_re <- within(data, b <- relevel(size_type, ref = "Medium"))
data_re <- within(data_re, c <- relevel(trophicLevel, ref = "Predator"))
data_re <- within(data_re, d <- relevel(habitatPref, ref = "Open"))
data_re <- within(data_re, e <- relevel(wings, ref = "Dimorphic"))

# Linear model
m2 <- lm(mean_trend ~ b + c + d + e, data = data_re)
summary(m2)

Anova(m1)

# Converting it to a dataframe
df2 <- broom::tidy(m2)

################################################
# Figures
head(data)
colnames(data)

data_long <- data %>% 
  pivot_longer(names_to = "var",
               cols = c("wings", "trophicLevel", "size_type", "habitatPref"),
               values_to = "val")

# ggplot(data_long, aes(mean_trend, val)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_point(position = position_jitter(width = 0.01), alpha = 0.3, aes(col = significance_status)) +
#   theme_classic() + xlab("Long-term trends") + ylab("") +
#   scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
#   theme(legend.title = element_blank(), legend.position = "top")
# 
# ggplot(data, aes(mean_trend, size_type)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_point(position = position_jitter(width = 0.01), alpha = 0.3, aes(col = significance_status)) +
#   theme_classic() + xlab("Long-term trends") + ylab("") +
#   scale_color_manual(values = c("deepskyblue", "darkgoldenrod1", "skyblue3")) +
#   theme(legend.title = element_blank(), legend.position = "top")

ggplot(data, aes(meanSize, mean_trend)) +
  geom_point(alpha = 0.5, aes(col = trend_status)) +
  theme_classic() + xlab("Body size (mm)") + ylab("Long-term trends") +
  scale_color_manual(values = c("darkgoldenrod1", "skyblue3")) +
  theme(legend.title = element_blank(), legend.position = "top") + 
  geom_smooth(method = "lm")


# ggplot(data_long, aes(mean_trend, val)) +
#   geom_boxplot() +
#   theme_classic() + xlab("Long-term trends") + ylab("")

ggsave("output/trend_size_2yr.png")

#################
# Trend vs wing types
g1 <- ggplot(data, aes(wings, mean_trend)) +
  geom_boxplot() +
  theme_classic() + xlab("") + ylab("Long-term trends") +
  geom_hline(yintercept=0, linetype="dashed", color = "red")

# Trend vs trophic level
g2 <- ggplot(data, aes(trophicLevel, mean_trend)) +
  geom_boxplot() +
  theme_classic() + xlab("") + ylab("Long-term trends") +
  geom_hline(yintercept=0, linetype="dashed", color = "red")

# Trend vs body size
g3 <- ggplot(data, aes(meanSize, mean_trend)) +
  geom_point() +
  theme_classic() + xlab("Mean body size") + ylab("Long-term trends") +
  geom_hline(yintercept=0, linetype="dashed", color = "red")

# Trend vs habitat preference
g4 <- ggplot(data, aes(habitatPref, mean_trend)) +
  geom_boxplot() +
  theme_classic() + xlab("") + ylab("Long-term trends") +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

cowplot::plot_grid(g1, g2, g3, g4)

ggsave("output/trend_trait_2yr.png")
