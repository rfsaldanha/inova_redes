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
  filter(munic_res == 110001) |>
  ggplot(aes(x = dt_inter, y = out_freq)) +
  geom_line(stat = "identity")
