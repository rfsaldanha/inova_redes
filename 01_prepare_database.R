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
# Vanderlei comments: 
# Foi considerado os CID-10 do capítulo XX, 
# que reúne outras causas externas de lesões acidentais
# (W00 a X59 e X-30 a X-39), 
# o capítulo XIX, que reúne as lesões, envenenamento e algumas 
# outras consequências de causas externas (S00 a T98) 
# e o capítulo I, que reúne algumas doenças infecciosas e parasitárias 
# (A27).

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
