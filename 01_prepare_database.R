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
  col.types = c(
    munic_res = "INTEGER",
    munic__mov = "INTEGER",
    cnes = "INTEGER",
    idade = "INTEGER",
    sex = "VARCHAR",
    dt_inter = "DATE",
    dt_saida = "DATE",
    diag_princ = "VARCHAR",
    marca_uti = "VARCHAR",
    raca_cor = "INTEGER",
    proc_rea = "INTEGER",
    munreslat = "DOUBLE",
    munreslon = "DOUBLE"
  ),
  header = TRUE, 
  na.strings = "NA"
)

# Import estimates data
duckdb_read_csv(
  conn = con, 
  name = "estimativas", 
  files = "../tb_emergencias_model_reduzido.csv",
  delim = ";", 
  col.types = c(
    data = "DATE",
    obs = "DOUBLE",
    pred = "DOUBLE",
    max = "DOUBLE",
    min = "DOUBLE",
    cod6 = "INTEGER",
    tipo = "DOUBLE",
    uf = "INTEGER",
    anomes = "INTEGER",
    regional = "INTEGER",
    marcaRS = "VARCHAR",
    fl_inf = "VARCHAR",
    id_prov = "INTEGER",
    fl_max = "DOUBLE",
    fl_pred = "DOUBLE",
    perc_max = "DOUBLE",
    perc_pred = "DOUBLE",
    perc_min = "DOUBLE",
    fl_min = "DOUBLE"
  ),
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
