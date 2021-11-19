# Best performers
# Set up ------------------------------------------------------------------
subproj_dir <- "research/forecast-eval"
here::i_am(paste0(subproj_dir, "/R/model-best-performers.R"))
# packages
library(dplyr)
library(forcats)
library(ggplot2)
# latest evaluation scores
source(here::here(subproj_dir, "R", "get-eval.R"))

# Variety of best performers ----------------------------------------------
# Variety of models making up the "best" performing for each target
# Target = location * variable * 1 horizon
top_target <- eval %>%
  filter(horizon %in% c(1,2) &
           location != "Overall") %>% # 
  # Keep only the best model by relative AE
  group_by(target_variable, location, horizon) %>%
  slice_min(rel_ae, n = 1) %>%
  select(location, horizon, target_variable, 
         team_name, model_name, model,
         rel_ae, model_score, model_score_source)

# Summarise how many targets each model was top for
top_target_models <- top_target %>%
  group_by(model, target_variable, horizon) %>%
  summarise(n = n(),
            n_pct = round(n / nrow(top_target) * 100, 1))

# Plot
top_models_plot <- top_target_models %>%
  group_by(model) %>%
  mutate(horizon = fct_reorder(as.character(horizon), horizon, sum)) %>%
  group_by(target_variable) %>%
  ggplot(aes(y = model, x = n)) + 
  geom_col(aes(fill = horizon), position = position_stack()) +
  # scico::scale_fill_scico_d(palette = "bamako", end = 0.8, direction = -1) +
  scale_fill_viridis_d(alpha = 0.6, guide = "legend") +
  labs(y = NULL, x = "Number of targets out of 32 for which a model ranked first 
       on absolute error relative to baseline",
       fill = "Weeks ahead") +
  facet_wrap(~ target_variable, scales = "fixed") +
  theme_bw() +
  theme(legend.position = "bottom")

plot(top_models_plot)

ggsave(filename = paste0(file_path, "/figures/", "top-models.png"),
       height = 4, width = 5,
       plot = top_models_plot)

# Percent of all targets of cases and deaths in 32 locations
# over 1 and 2 week horizons in which model ranked best among all models relative to baseline forecast
# (available targets N = 64, top ranks = 69 due to tied rankings