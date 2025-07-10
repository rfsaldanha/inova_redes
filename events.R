# Packages
library(tidyverse)

# Data source
events_raw <- read_csv2(
  "../dados/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado.csv"
)

events_raw |> 
  select(code_muni = Cod_IBGE_Mun, date = Data_Evento, cod_cobrade = Cod_Cobrade) |> 
