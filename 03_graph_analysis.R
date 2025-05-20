# Packages
library(tidyverse)
library(igraph)

# Import data
od_all_proc <- read_rds(file = "od_all_proc.rds")

# Out degree
out_all_proc <- od_all_proc |>
  # Remove loops
  filter(!(munic_res == munic_mov)) |>
  # Sum frequencies 
  group_by(munic_res, dt_inter) |>
  summarise(out_freq = sum(freq)) |>
  ungroup()

# Example
out_all_proc |>
  filter(munic_res == 420540) |>
  ggplot(aes(x = dt_inter, y = out_freq)) +
  geom_line(stat = "identity") +
  labs(title = "Florianópolis")

# Graph, all time
g_all <-  od_all_proc |>
  # Remove loops
  filter(!(munic_res == munic_mov)) |>
  # Aggregate all dates
  group_by(munic_res, munic_mov) |>
  summarise(freq = sum(freq)) |>
  ungroup() |>
  # Rename fields
  rename(from = munic_res, to = munic_mov, weight = freq) |>
  # Graph structure
  graph_from_data_frame(directed = TRUE)

degree(g_all, mode = "out")
