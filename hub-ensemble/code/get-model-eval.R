subproj_dir <- "research/intro-paper"
here::i_am(paste0(subproj_dir, "/R/get-model-eval.R"))

# Get evaluation dataset
library(here)
library(purrr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(covidHubUtils)

# find evaluation  ------------------------------------------------------
# Find latest evaluation
eval_date <- dir(here("covid19-forecast-hub-europe", "evaluation", "weekly-summary"))
eval_date <- sort(as.Date(gsub("(evaluation-)|(\\.csv)", "", eval_date)))
eval_date <- eval_date[length(eval_date)]
eval_file <- here("covid19-forecast-hub-europe", "evaluation", "weekly-summary", paste0("evaluation-", eval_date, ".csv"))

# clean variable names
clean_variables <- c("inc case" = "Cases", "inc death" = "Deaths")

# get model designations - in order to remove "other" except baseline
model_desig <- covidHubUtils::get_model_designations(source = "local_hub_repo",
                                                     hub_repo_path = here("covid19-forecast-hub-europe")) %>%
  mutate(designation = case_when(model == "EuroCOVIDhub-baseline" ~ "secondary",
                                 TRUE ~ designation))

# clean eval dataset ------------------------------------------------------

# Get evaluation and tidy up
model_eval <- read_csv(eval_file) %>%
  # keep only 1-4 horizons
  filter(horizon <= 4) %>%
  # clean up team-model names
  separate(model, into = c("team_name", "model_name"), 
           sep = "-", remove = FALSE) %>%  
  mutate(
    # add neat variables, esp useful for plots
    target_variable = recode(target_variable, !!!clean_variables),
    # ensure scores are numeric
    across(c(horizon, rel_ae:n_loc), as.numeric)) %>%
  
  filter() %>%
  mutate(model = factor(model, ordered = TRUE)) %>%
  # Remove from evaluation:
  filter(
    # hospitalisations
    target_variable %in% clean_variables &
    # models designated "other"
    !model %in% filter(model_desig, designation == "other")$model &
    # 10 week evaluations (keep only evaluation based on all history)
    weeks_included == "All")


# tidy up -----------------------------------------------------------------
# separate out ensemble as comparator
score_ensemble <- model_eval %>%
  filter(grepl("hub-ensemble", model)) %>%
  select(ensemble_rel_wis = rel_wis,
         ensemble_rel_ae = rel_ae,
         weeks_included, target_variable, horizon, location) %>%
  mutate(baseline_rel_wis = 1,
         baseline_rel_ae = 1)

model_eval_wide <- model_eval %>%
  filter(!grepl("hub-baseline", model)) %>% # leave ensemble as row as well as col
  full_join(score_ensemble)

fig_date <- paste("Evaluation as at", eval_date)

###
rm(model_desig, clean_variables, score_ensemble)
   