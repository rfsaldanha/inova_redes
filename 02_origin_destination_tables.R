# Packages
library(tidyverse)
library(duckdb)

# Create database connection
con <- dbConnect(duckdb(), dbdir = "aih.duckdb", read_only = TRUE)

# AIH Table alias
aih_tbl <- tbl(con, "aih")

# Preview the table, first 100 rows
aih_tbl |>
  head(100) |>
  collect() |>
  View()

# All procedures, by date of admission
od_all_proc <- aih_tbl |>
  # Dates to month
  mutate(
    dt_inter = as_date(dt_inter),
    year = year(dt_inter),
    month = month(dt_inter)
  ) |>
  # Compute frequency
  group_by(year, month, munic_res, munic__mov) |>
  summarise(freq = n()) |>
  ungroup() |>
  # Retrieve from database to memory
  collect() |>
  # Rename variable
  rename(munic_mov = munic__mov) |>
  # Recreate date variable
  mutate(dt_inter = as_date(paste0(year, "-", month, "-01"))) |>
  # Select and relocate variables
  select(-year, -month) |>
  relocate(dt_inter) |>
  # Filter date
  filter(dt_inter >= as_date("2013-01-01"))

# Export
write_rds(x = od_all_proc, file = "od_all_proc.rds")

# Disconnect database
dbDisconnect(con)
