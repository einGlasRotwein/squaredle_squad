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
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "top"
  )

player_cols <- c(
  jan   = "#1b9e77",
  juli  = "#d95f02",
  julia = "#7570b3",
  nils  = "#e7298a",
  simon = "#1da6f1"
)

sheet_url <- "sheet_url" # enter sheet url

# Log in with google account (can be pre-authorized via googlesheets4)
gs4_auth("julivnagel@gmail.com")
data <- read_sheet(sheet_url)

# convert completion time to minutes and date to string
data <- 
  data %>% 
  pivot_longer(
    jan:simon,
    names_to = "player",
    values_to = "time"
  ) %>% 
  mutate(
    time = 
      map_dbl(str_split(time, ":"), ~{
      x <- as.numeric(.x)
      if (length(x) == 2) x[1] * 60 + x[2]
      else if (length(x) == 3) x[1] * 3600 + x[2] * 60 + x[3]
      else NA_real_
    }) / 60
  )

normal <- data %>% filter(modus == "normal")
express <- data %>% filter(modus == "express")

## NORMAL ----------------------------------------------------------------------

lm_normal <- lmer(time ~ player + (1 | date), data = normal)
emmeans_normal <- emmeans(lm_normal, "player")

# TO DO
# https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
eff_size(emmeans_normal, sigma = sigma(lm_normal), edf = 11.7)

emmeans_normal_df <- as.data.frame(emmeans_normal)
emmeans_normal_df$player <- 
  factor(
    emmeans_normal_df$player, 
    levels = emmeans_normal_df$player[order(emmeans_normal_df$emmean, decreasing = TRUE)],
    ordered = TRUE
  )

normal$player <- 
  factor(
    normal$player, 
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

# compute y-positions for the brackets (stacked above the highest completion time)
y_top <- max(normal$time, na.rm = TRUE)
step  <- 0.07 * (max(normal$time, na.rm = TRUE) - min(normal$time, na.rm = TRUE)) # spacing

t_tests_normal <-
  t_tests_normal %>%
  arrange(p.value) %>% # smallest p at the bottom
  mutate(y.position = y_top + step * row_number())

plot_normal <- 
  ggplot(
    aes(x = player, y = time, fill = player),
    data = normal
  ) +
  geom_point(position = position_jitter(width = .2, height = 0), alpha = .6) +
  geom_violin(aes(colour = player), alpha = .4) +
  geom_errorbar(
    aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
    width = .25,
    data = emmeans_normal_df
  ) +
  geom_point(
    aes(y = emmean, colour = player), size = 4, colour = "black", shape = 21,
    data = emmeans_normal_df
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
  scale_fill_manual(values = player_cols, drop = FALSE) +
  scale_colour_manual(values = player_cols, drop = FALSE) +
  guides(fill = "none", colour = "none") +
  labs(title = "normal", y = "completion time (mins)") +
  custom_theme

## EXPRESS ---------------------------------------------------------------------#

lm_express <- lmer(time ~ player + (1 | date), data = express)
emmeans_express <- emmeans(lm_express, "player")

# TO DO
# https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
eff_size(emmeans_express, sigma = sigma(lm_express), edf = 11.7)

emmeans_express_df <- as.data.frame(emmeans_express)
emmeans_express_df$player <- 
  factor(
    emmeans_express_df$player, 
    levels = emmeans_express_df$player[order(emmeans_express_df$emmean, decreasing = TRUE)],
    ordered = TRUE
  )

express$player <- 
  factor(
    express$player, 
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

# compute y-positions for the brackets (stacked above the highest completion time)
y_top <- max(express$time, na.rm = TRUE)
step  <- 0.07 * (max(express$time, na.rm = TRUE) - min(express$time, na.rm = TRUE)) # spacing

t_tests_express <-
  t_tests_express %>%
  arrange(p.value) %>% # smallest p at the bottom
  mutate(y.position = y_top + step * row_number())

plot_express <- 
  ggplot(
    aes(x = player, y = time, fill = player),
    data = express
  ) +
  geom_point(position = position_jitter(width = .2, height = 0), alpha = .6) +
  geom_violin(aes(colour = player), alpha = .4) +
  geom_errorbar(
    aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
    width = .25,
    data = emmeans_express_df
  ) +
  geom_point(
    aes(y = emmean, colour = player), size = 4, colour = "black", shape = 21,
    data = emmeans_express_df
  ) +
  stat_pvalue_manual(
    t_tests_express,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    bracket.size = 0.6,
    size = 5
  ) +
  scale_fill_manual(values = player_cols, drop = FALSE) +
  scale_colour_manual(values = player_cols, drop = FALSE) +
  guides(fill = "none", colour = "none") +
  labs(title = "express", y = "completion time (mins)") +
  custom_theme

## COMBINED PLOT ---------------------------------------------------------------

# TO DO: fix order
(plot_normal / plot_express)
