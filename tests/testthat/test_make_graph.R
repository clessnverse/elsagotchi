source("R/datagotchi_barplot.R")
source("R/make_graph.R")

data <- readRDS("tests/data/data_app_clean_2024_09_26_weighted.rds")

make_graph(data)

df_graph <- data %>%
  mutate(republican_binary = ifelse(true_party_id %in% c("soft_republican", "hard_republican"), 1, 0)) %>%
  group_by(ses_income_large_groups, ses_ethnicity) %>%
  summarise(id = mean(republican_binary, na.rm = TRUE))
