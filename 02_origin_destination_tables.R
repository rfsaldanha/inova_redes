# Packages
library(tidyverse)
library(duckdb)

# Create database connection
con <- dbConnect(duckdb(), dbdir = "aih.duckdb", read_only = FALSE)

# AIH Table alias
aih_tbl <- tbl(con, "aih")

# Preview the table, first 100 rows
aih_tbl |>
  head(100) |>
  collect() |>
  View()

# All procedures, by date of admission
od_all_proc <- aih_tbl |>
  # Compute frequency
  group_by(dt_inter, munic_res, munic__mov) |>
  summarise(freq = n()) |>
  ungroup() |>
  # Retrieve from database to memory
  collect() |>
  # Rename variable
  rename(munic_mov = munic__mov) |>
  # Change to date type
  mutate(dt_inter = as_date(dt_inter))

# Export
write_rds(x = od_all_proc, file = "od_all_proc.rds")
