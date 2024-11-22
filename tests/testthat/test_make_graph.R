source("R/datagotchi_barplot.R")
source("R/make_graph.R")

data <- readRDS("tests/data/data_app_clean_2024_09_26_weighted.rds")

make_graph(data)
