# Packages
library(tidyverse)
library(janitor)

# Import data
events <- read_csv2(file = "../bd_emergencia_desastres.csv") |>
  clean_names() |>
  select(
    code_muni = cod6,
    date = registro,
    categoria = nome_categ
  ) 
  

saveRDS(events, file = "events.rds")
