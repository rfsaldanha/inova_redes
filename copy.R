library(tidyverse)
library(geobr)
library(brclimr)
library(DBI)
library(rlang)
library(glue)
library(duckdb)
library(checkmate)
library(brclimr)
library(readr)

remotes::install_github(repo = "rfsaldanha/brclimr")

mun <- read_municipality(code_muni = "all", year = 2022)


banco_desastres_2013_2024_UTF8_ <- readr::read_delim("https://atlasdigital.mdr.gov.br/arquivos/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado.csv",
                              delim = ";",
                              locale = locale(encoding = "Latin1"))

banco_desastres_2013_2024_UTF8_ <- banco_desastres_2013_2024_UTF8_ %>% 
  distinct(Protocolo_S2iD, .keep_all = TRUE)

# Realizando um inner join desastres codigo municipio via nome
ufs_ibge <- tibble(
  UF = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", 
         "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", 
         "RS", "SC", "SE", "SP", "TO"),
  coduf = c(12, 27, 13, 16, 29, 23, 53, 32, 52, 21, 31, 50, 51, 15, 25, 
                26, 22, 41, 33, 24, 11, 14, 43, 42, 28, 35, 17))

banco_desastres_2013_2024_UTF8_ <- banco_desastres_2013_2024_UTF8_ %>%
  left_join(ufs_ibge, by = c("Sigla_UF" = "UF"))

banco_2000_2024 <-banco_desastres_2013_2024_UTF8_ %>% 
  filter(Status == "Reconhecido") %>%
  mutate(codmunic = as.integer(substr(as.character(Cod_IBGE_Mun), 1, nchar(Cod_IBGE_Mun) - 1))) %>% 
  mutate(codsist = as.integer(substr(as.character(Cod_Cobrade), 1, nchar(Cod_Cobrade) - 4))) %>% 
  rename("COBRADE" = "Cod_Cobrade")

banco_2000_2024 <- exemplo_aplicacao_dplyr(banco_2000_2024)


# Importar o shapefile
library(sf)
shapefile_path <- "sedes.shp"
shapefile <- st_read(shapefile_path)

banco_2000_2024_sedes <- banco_2000_2024 %>%
  left_join(shapefile %>% select(Id, coordY, coordX), by = c("codmunic" = "Id")) %>% 
  mutate(grupo_de_desastre = case_when(
    grupo_de_desastre %in% c("climatológico", "Climatológico") ~ "Climatológico",
    grupo_de_desastre %in% c("geológico", "Geológico") ~ "Geológico",
    grupo_de_desastre %in% c("hidrológico", "Hidrológicos") ~ "Hidrológico",
    grupo_de_desastre %in% c("Meteorológico", "meteorológicos") ~ "Meteorológico",
    TRUE ~ grupo_de_desastre  
  )) %>% 
  mutate(codigo_sistema = case_when(
    grupo_de_desastre == "Climatológico" ~ 1,
    grupo_de_desastre == "Geológico" ~ 2,
    grupo_de_desastre == "Hidrológico" ~ 3,
    grupo_de_desastre == "Meteorológico" ~ 4,
    TRUE ~ NA_integer_
  ))


desastres <- banco_2000_2024_sedes %>% 
  mutate(COBRADE = as.character(COBRADE)) %>%
  mutate(COBRADE = case_when(
    COBRADE == "12101" ~ "13215",
    COBRADE == "12102" ~ "13111",
    COBRADE == "12103" ~ "13111",
    COBRADE == "12104" ~ "13211",
    COBRADE == "12201" ~ "13322",
    COBRADE == "12205" ~ "13213",
    COBRADE == "12206" ~ "13322",
    COBRADE == "12303" ~ "12300",
    COBRADE == "12304" ~ "12100",
    COBRADE == "12402" ~ "14120",
    COBRADE == "12404" ~ "14131",
    COBRADE == "12410" ~ "14110",
    COBRADE == "13301" ~ "11321",
    COBRADE == "13302" ~ "11332",
    COBRADE == "13303" ~ "11331",
    COBRADE == "13309" ~ "11410",
    TRUE ~ COBRADE  # mantém os demais valores
  )) %>% 
  rename("nome" = "Nome_Municipio",
         "nomesist" = "grupo_de_desastre",
         "desalojados" = "DH_DESALOJADOS",
         "desabrigados" = "DH_DESABRIGADOS",
         "desaparecidos" = "DH_DESAPARECIDOS",
         "feridos" = "DH_FERIDOS",
         "enfermas" = "DH_ENFERMOS",
         "mortas" = "DH_MORTOS",
         "afetadas" = "DH_total_danos_humanos_diretos",
         "codar" = "COBRADE") %>% 
  dplyr::select(coduf, coduf, codmunic ,nome, codigo_grupo, nomesist ,codar,
                desalojados,desabrigados,desaparecidos,feridos,enfermas,mortas,
                afetadas,Data_Evento, tipologia, descricao_tipologia, coordY,coordX)




library(geojsonio)

st_write(desastres, "banco_2000_2024_sedes.geojson", driver = "GeoJSON")
write.csv2(desastres, "banco_2000_2024_sedes_ponto_virgula.csv", row.names = FALSE)
save(desastres, file = "banco_2000_2024_sedes.RData")















library(foreign)

# Carregar o arquivo DBF

file_path <- "C:/Users/diego/Documents/OBSERVATORIO/ARTIGO_DESASTRES/ARTIGO_DESASTRES_BRCLIM/unidades_dist_risco.dbf"
dados <- read.dbf(file_path, as.is = TRUE) %>% 
  
  
  dados <- dados %>%
  left_join(ufs_ibge, "codigo_uf" = "coduf" )

ufs_ibge<-ufs_ibge %>% 
  rename(coduf = codigo_uf)

library(geobr)

mun<-read_municipality(year = 2020)

dados <- dados %>%
  left_join(ufs_ibge, by = "coduf")

dados<-dados %>% 
  left_join(mun, by =  )

# Carregar a biblioteca ggplot2
library(ggplot2)

# Criar o gráfico de violino
ggplot(dados, aes(x = local, y = HubDist, fill = local)) +
  geom_col() +
  #facet_wrap(~ UF, ncol = 4, scales = "free") +
  labs(title = "Distância ao Hub por Local",
       x = " ",
       y = "Distância ao Hub (km)") +
  theme_minimal() +
  theme(legend.position = "bottom")




library(dplyr)
library(readr)
library(purrr) # Para usar map()

# Defina o caminho para o diretório
dir_path <- "C:/Users/diego/Documents/OBSERVATORIO/ARTIGO_DESASTRES/ARTIGO_DESASTRES_BRCLIM/CNES_01_24"

# Lista todos os arquivos CSV na pasta
arquivos <- list.files(path = dir_path, pattern = "*.csv", full.names = TRUE)

# Função para garantir que todas as colunas sejam do tipo character
read_csv_as_character <- function(file) {
  read_csv(file, col_types = cols(.default = "c"))
}

# Ler e combinar todos os arquivos
dados_combinados <- arquivos %>%
  map(read_csv_as_character) %>% # Ler todos os arquivos CSV com colunas como "character"
  bind_rows()                    # Combinar todos os data frames em um só

# Salvar o data frame combinado em um novo arquivo CSV
write_csv(dados_combinados, "C:/Users/diego/Documents/OBSERVATORIO/ARTIGO_DESASTRES/ARTIGO_DESASTRES_BRCLIM/CNES_01_24/dados_combinados.csv")














# Municipios com problemas na concatenação
nao_incluidas <- anti_join(banco_desastres_2013_2024, df_novo, by = "nome")

# Municipios sem decretos na série
mun_sem_desastre <- anti_join( df_novo, banco_desastres_2013_2024, by = "nome")

# Filtro município - desastres
test <- df_analise %>% 
  filter(mun == "Nova Friburgo") %>% 
  rename(date = Registro) %>% 
  group_by(date, mun, nome_categ) %>%
  summarise(desastres = n(), .groups = 'drop')

# Filtro município - pluviosidade
pr <- fetch_data(
  code_muni = 3303401,
  product = "brdwgd",
  indicator = "pr",
  statistics = "sum",
  date_start = as.Date("2017-01-01"),
  date_end = as.Date("2024-03-23")) %>% 
  rename(precip = value)

# União bases de dados por datas
banco_teste <- full_join(pr,test,  by = "date")

#Plotar os resultados

library(cowplot)
library(patchwork)

p1 <- ggplot(banco_teste, aes(date, precip)) + 
  geom_line() + theme(axis.line = element_line())

p2 <- ggplot(banco_teste, aes(date, desastres)) + 
  geom_point(color = "red") + theme(axis.line = element_line())
facet_wrap(~nome_categ)

p1/p2

