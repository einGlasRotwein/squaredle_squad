library(googlesheets4)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(ggpubr)
library(patchwork)

custom_theme <- 
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = .5),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "top"
  )

sheet_url <- "sheet_url" # enter sheet url

# Log in with google account (can be pre-authorized via googlesheets4)
data <- read_sheet(sheet_url)

# convert completion time to minutes and date to string
data <- 
  data %>% 
  pivot_longer(
    jan:nils,
    names_to = "player",
    values_to = "time"
  ) %>% 
  mutate(
    time = 
      (as.numeric(gsub(":[0-9]+$", "", time)) * 60 +
         as.numeric(gsub("^[0-9]+:", "", time))) / 60,
    date = as.character(date)
  )

normal <- data %>% filter(modus == "normal")
express <- data %>% filter(modus == "express")

## NORMAL ----------------------------------------------------------------------

lm_normal <- lmer(time ~ player + (1 | date), data = normal)
emmeans_normal <- emmeans(lm_normal, "player")

emmeans_normal_df <- as.data.frame(emmeans_normal)
emmeans_normal_df$player <- 
  factor(
    emmeans_normal_df$player, 
    levels = emmeans_normal_df$player[order(emmeans_normal_df$emmean, decreasing = TRUE)],
    ordered = TRUE
  )

t_tests_normal <- 
  contrast(emmeans_normal, "pairwise") %>% 
  as.data.frame() %>% 
  separate(contrast, into = c("group1", "group2"), sep = "\\s-\\s") %>%
  mutate(
    p_label = case_when(
      p.value < .001 ~ "***",
      p.value < .01  ~ "**",
      p.value < .05  ~ "*",
      TRUE           ~ "ns"
    )
  ) %>% 
  filter(p.value < .05)

# compute y-positions for the brackets (stacked above the highest bar/CI)
y_top <- max(emmeans_normal_df$upper.CL, na.rm = TRUE)
step  <- 0.06 * (max(emmeans_normal_df$upper.CL) - min(emmeans_normal_df$lower.CL)) # spacing

t_tests_normal <-
  t_tests_normal %>%
  arrange(p.value) %>% # smallest p at the bottom
  mutate(y.position = y_top + step * row_number())

plot_normal <- 
  ggplot(
    aes(x = player, y = emmean),
    data = emmeans_normal_df
  ) +
  geom_col(colour = "black", alpha = .7) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = .25
  ) +
  stat_pvalue_manual(
    t_tests_normal,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    bracket.size = 0.6,
    size = 5
  ) +
  labs(title = "normal", y = "completion time (mins)") +
  custom_theme

## EXPRESS ---------------------------------------------------------------------

lm_express <- lmer(time ~ player + (1 | date), data = express)
emmeans_express <- emmeans(lm_express, "player")

emmeans_express_df <- as.data.frame(emmeans_express)
emmeans_express_df$player <- 
  factor(
    emmeans_express_df$player, 
    levels = emmeans_express_df$player[order(emmeans_express_df$emmean, decreasing = TRUE)],
    ordered = TRUE
  )

t_tests_express <- 
  contrast(emmeans_express, "pairwise") %>% 
  as.data.frame() %>% 
  separate(contrast, into = c("group1", "group2"), sep = "\\s-\\s") %>%
  mutate(
    p_label = case_when(
      p.value < .001 ~ "***",
      p.value < .01  ~ "**",
      p.value < .05  ~ "*",
      TRUE           ~ "ns"
    )
  ) %>% 
  filter(p.value < .05)

# currently no significant tests
# # compute y-positions for the brackets (stacked above the highest bar/CI)
# y_top <- max(emmeans_express_df$upper.CL, na.rm = TRUE)
# step  <- 0.06 * (max(emmeans_express_df$upper.CL) - min(emmeans_express_df$lower.CL)) # spacing
# 
# t_tests_express <-
#   t_tests_express %>%
#   arrange(p.value) %>% # smallest p at the bottom
#   mutate(y.position = y_top + step * row_number())

plot_express <- 
  ggplot(
    aes(x = player, y = emmean),
    data = emmeans_express_df
  ) +
  geom_col(colour = "black", alpha = .7) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = .25
  ) +
  labs(title = "express", y = "completion time (mins)") +
  custom_theme

## COMBINED PLOT ---------------------------------------------------------------

(plot_normal / plot_express)
