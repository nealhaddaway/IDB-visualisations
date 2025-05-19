# tidying
library(tidyverse)

# load data
provisional_data <- read_csv("provisional_data.csv")

# rename columns
colnames(provisional_data) <- c("timestamp", "name", "rayyan_ID", "title", "country", "region", "start_year", "end_year", "ag_subsector", "primary_crop", "processed_crop", "livestock", "primary_livestock", "processed_livestock", "intervention", "diverse_population", "climate_perspective", "data_type", "observation_unit", "methodology", "dependent_variables", "effect_size", "email", "comments", "Direction and Significance of Results: FACILITATORS.", "RESULTS: SIGNS AND SIGNIFICANCE (Facilitators)", "RESULTS: SIGNS AND SIGNIFICANCE (Barriers)", "RESULTS: Indicate whether the study CONDUCTS heterogeneity analysis across groups or time periods, and whether it FINDS heterogeneous impacts across groups or time periods.   ", "BIASES: Does the study present a risk of any of the following sources of bias?", "BIASES. Were efforts made to address the risks of the biases mentioned above?", "Column 24
")

# tidy study country
provisional_data$country <- str_replace_all(provisional_data$country, pattern = "[0-9]+\\.\\s", "")

# separate_rows based on capitalisation (because some responses contain commas, so comma can not be used as a delimeter)
provisional_data %>% 
  separate_rows(primary_crop, sep=",(?=[A-Z])")
