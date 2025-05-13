# Packages
library(tidyverse)
library(duckdb)
library(glue)

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
dbExecute(conn = con, "INSTALL postgres;")
dbExecute(conn = con, "LOAD postgres;")
dbExecute(
  conn = con,
  glue(
    "CREATE TABLE mod_enviados AS SELECT * FROM postgres_scan('host=localhost port=5432 user={Sys.getenv('psql_local_user')} password={Sys.getenv('psql_local_psw')} dbname=obs', 'inova', 'resul_mod_enviados');"
  )
)
dbExecute(
  conn = con,
  glue(
    "CREATE TABLE mod_enviados_mes AS SELECT * FROM postgres_scan('host=localhost port=5432 user={Sys.getenv('psql_local_user')} password={Sys.getenv('psql_local_psw')} dbname=obs', 'inova', 'resul_mod_enviados_mes');"
  )
)
dbExecute(
  conn = con,
  glue(
    "CREATE TABLE mod_enviados_semana AS SELECT * FROM postgres_scan('host=localhost port=5432 user={Sys.getenv('psql_local_user')} password={Sys.getenv('psql_local_psw')} dbname=obs', 'inova', 'resul_mod_enviados_semana');"
  )
)

dbExecute(
  conn = con,
  glue(
    "CREATE TABLE mod_munic_diario AS SELECT * FROM postgres_scan('host=localhost port=5432 user={Sys.getenv('psql_local_user')} password={Sys.getenv('psql_local_psw')} dbname=obs', 'inova', 'resul_mod_munic_diario');"
  )
)
dbExecute(
  conn = con,
  glue(
    "CREATE TABLE mod_munic_mes AS SELECT * FROM postgres_scan('host=localhost port=5432 user={Sys.getenv('psql_local_user')} password={Sys.getenv('psql_local_psw')} dbname=obs', 'inova', 'resul_mod_munic_mes');"
  )
)
dbExecute(
  conn = con,
  glue(
    "CREATE TABLE mod_munic_semana AS SELECT * FROM postgres_scan('host=localhost port=5432 user={Sys.getenv('psql_local_user')} password={Sys.getenv('psql_local_psw')} dbname=obs', 'inova', 'resul_mod_munic_semana');"
  )
)

dbExecute(
  conn = con,
  glue(
    "CREATE TABLE mod_recebidos AS SELECT * FROM postgres_scan('host=localhost port=5432 user={Sys.getenv('psql_local_user')} password={Sys.getenv('psql_local_psw')} dbname=obs', 'inova', 'resul_mod_recebidos');"
  )
)
dbExecute(
  conn = con,
  glue(
    "CREATE TABLE mod_recebidos_mes AS SELECT * FROM postgres_scan('host=localhost port=5432 user={Sys.getenv('psql_local_user')} password={Sys.getenv('psql_local_psw')} dbname=obs', 'inova', 'resul_mod_recebidos_mes');"
  )
)
dbExecute(
  conn = con,
  glue(
    "CREATE TABLE mod_recebidos_semana AS SELECT * FROM postgres_scan('host=localhost port=5432 user={Sys.getenv('psql_local_user')} password={Sys.getenv('psql_local_psw')} dbname=obs', 'inova', 'resul_mod_recebidos_semana');"
  )
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
