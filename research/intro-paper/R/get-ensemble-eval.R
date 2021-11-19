subproj_dir <- "research/intro-paper"
here::i_am(paste0(subproj_dir, "/R/get-ensemble-eval.R"))

library(here)
library(readr)
library(dplyr)
library(tidyr)

# Get latest evaluation dataset ------------------------------------------------
eval_date <- dir(here("covid19-forecast-hub-europe",
                      "ensembles", "evaluation", "weekly-summary"))
eval_date <- sort(as.Date(gsub("(evaluation-)|(\\.csv)", "", eval_date)))
eval_date <- eval_date[length(eval_date)]
eval_file <- here("covid19-forecast-hub-europe", 
                  "ensembles", "evaluation", "weekly-summary", 
                  paste0("evaluation-", eval_date, ".csv"))
                  
# Prep dataset ------------------------------------------------------------
# neater factor labels
clean_target_names <- c("inc case" = "Cases", "inc death" = "Deaths")
main_ensembles_names <- c(
  "mean" = "Unweighted mean",
  "All_relative_skill_weighted_mean" = "Weighted mean",
  "median" = "Unweighted median",
  "All_relative_skill_weighted_median" = "Weighted median"
)

# Tidy up
eval_ensemble <- read_csv(eval_file) %>%
  # keep only 1-4 horizons
  filter(horizon <= 4) %>%
  # clean up team-model names
  separate(model, into = c("team_name", "model"), 
           sep = "-", remove = FALSE) %>%  
  mutate(
    target_variable = recode(target_variable, !!!clean_target_names),
    method_average = case_when(grepl("mean", model) ~ "Mean",
                                 grepl("median", model) ~ "Median"),
    method_weight = case_when(grepl("weighted", model) ~ "Weighted",
                                 TRUE ~ "Unweighted"),
    method_history = case_when(grepl("[0-9]+", model) ~
                               paste(suppressWarnings(
                                 parse_number(gsub("-", "", model))),
                                     "weeks history"),
                               TRUE ~ "All history"),
    method_cutoff = case_when(grepl("cutoff", model) ~ "Cutoff rel. WIS < 1",
                              TRUE ~ "No cutoff"),
    ensemble_name = recode(model, !!!main_ensembles_names),
    main_ensemble = model %in% names(main_ensembles_names),
    # ensure scores are numeric
    across(c(horizon, rel_ae:n_loc), as.numeric),
    # set horizon as ordered factor
    horizon = factor(horizon, ordered = TRUE)) %>%
  select(model, ensemble_name, main_ensemble,
         starts_with("method_"), weeks_included,
         location, location_name, target_variable, horizon,
         n, rel_wis, cov_50, cov_95) %>%
  filter(!location %in% "Overall" &
          !grepl("baseline|horizon", model) &
          target_variable %in% clean_target_names)

main_ensembles <- eval_ensemble %>%
  filter(main_ensemble &
         weeks_included == "All")

fig_date <- paste("Evaluation as at", eval_date)

rm(eval_file, clean_target_names, main_ensembles_names)
