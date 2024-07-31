library(tidyverse)
library(duckdb)

# Read data
aih_teste <- read_delim(
  file = "../test_AIH_emergencias.csv",
  delim = ";", na = c("", "NA")
)

# Create database
con <- dbConnect(duckdb(), dbdir = "aih_teste.duckdb", read_only = FALSE)

# Populate with AIH data frame
dbWriteTable(con, "aih", aih_teste)

# Remove data frame
rm(aih_teste)
gc()

# Table alias
aih_tbl <- tbl(con, "aih")

# Check number of rows
aih_tbl |>
  tally()

# Preview the table, first 100 rows
aih_tbl |>
  head(100) |>
  collect() |>
  View()

# Disconnect database
dbDisconnect(con)
