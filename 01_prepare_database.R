# Packages
library(tidyverse)
library(duckdb)

# Create database connection
con <- dbConnect(duckdb(), dbdir = "aih.duckdb", read_only = FALSE)

# Import AIH csv
duckdb_read_csv(
  conn = con, 
  name = "aih", 
  files = "../tb_aih_emergencias.csv",
  delim = ";", 
  header = TRUE, 
  na.strings = "NA"
)

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
