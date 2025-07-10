# Packages
library(tidyverse)
library(janitor)

# Import data
events <- read_csv2(file = "../dados/desastres_09_07_25.csv")

events <- read_csv2(file = "../bd_emergencia_desastres.csv") |>
  clean_names() |>
  select(
    code_muni = cod6,
    date = registro,
    categoria = nome_categ
  )


saveRDS(events, file = "events.rds")

events <- readRDS("../inova_redes/events.rds")
