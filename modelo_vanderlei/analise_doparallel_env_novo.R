library(readr)
library(tidyverse)
library(lubridate)
library(minpack.lm)
library(foreach)
library(conflicted)

consulta<-function(x){
  library(RPostgres)
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",       # Nome do banco de dados
    host = "localhost",             # Endereço do servidor (ou IP)
    port = 5432,                    # Porta padrão do PostgreSQL
    user = "postgres",           # Usuário do banco de dados
    password = "vm190399"          # Senha do usuário
  )
  
  resu<-RPostgres::dbGetQuery(con, x)
  
  RPostgres::dbDisconnect(con)
  return(resu)
}

setwd("/home/pascoal/Documentos/fv_proj_emergencias/")

# ##### Verificar cod6
# library(tidyverse)
# cod_geo <- read.csv("/home/pascoal/Documentos/_hamonize/corr_cod6.csv", sep = ",")
# cod_ <- read.csv("/home/pascoal/Documentos/_hamonize/pop_br.csv", sep = ",")
# 
# cod_$cod6 <- substring(cod_$cd_mun,1,6)
# 
# 
# cod_geo$cod6_t <- as.character(cod_geo$cod6_t)
# cod_geo<- right_join(x = cod_geo, y = cod_, by = c("cod6_t" = "cod6"))
# rm(cod_)    
# 
# verif_cod <- function(cod){
#   
#   cod<-as.character(cod)
#   
#   if(is.na(cod)){
#     result = NA 
#   }else{
#     
#     if(any(cod == .GlobalEnv$cod_geo$cod6)){
#       result = .GlobalEnv$cod_geo$cod6_t[which(.GlobalEnv$cod_geo$cod6 == cod)]
#     }else{
#       if(substr(cod,1,2)=="53"){
#         result = 530010
#       }else{
#       result = NA
#       }
#     }
#     
#   }
#   return(result)
# }


library(readr)
processamento_log <- read_delim("processamento_log_recebidos_semana.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

conflicts_prefer(dplyr::filter)
setwd("/home/pascoal/Documentos/fv_proj_emergencias/")

tb_emergencias_tips <-consulta('SELECT distinct "MUNIC_RES", "MUNIC_MOV", "TIPO"	FROM inova.tb_emergencias_tips;')


# Carregar os dados
# tb_emergencias_tips <- read_delim("~/Documentos/fv_proj_emergencias/tb_emergencias_tips.csv", 
#                                   delim = ";", escape_double = FALSE, trim_ws = TRUE) 

# tb_emergencias_tips$MUNIC_RES <- unlist(lapply(X = tb_emergencias_tips$MUNIC_RES, FUN = function(X){
#     .GlobalEnv$verif_cod(cod = X)}))
# 
# tb_emergencias_tips$MUNIC_MOV <- unlist(lapply(X = tb_emergencias_tips$MUNIC_MOV, FUN = function(X){
#   .GlobalEnv$verif_cod(cod = X)}))


# Converter colunas para fatores
# tb_emergencias_tips <- tb_emergencias_tips %>%
#   mutate(MUNIC_RES = as.factor(MUNIC_RES),
#          MUNIC_MOV = as.factor(MUNIC_MOV),
#          TIPO = as.factor(TIPO))

# Paralelização
# n.cores <- max(1, detectCores() - 4)
# cl <- makeCluster(n.cores, type = "PSOCK")
# registerDoParallel(cl)
# 
# message("Usando ", n.cores, " núcleos para processamento paralelo.")

p <- Sys.time()

def_munic_res <- unique(tb_emergencias_tips$MUNIC_RES)
tipos <- unique(tb_emergencias_tips$TIPO)

# Criar arquivo de log
log_file <- "processamento_log_recebidos_semana.csv"
# write.table(data.frame(Tipo = character(), Municipio = character(), Status = character(), Tempo = character()),
#             file = log_file, sep = ";", row.names = FALSE, col.names = TRUE, append = FALSE)

# Definir função do modelo
modelo <- function(t, alpha0, alpha1, alpha2, alpha3, gamma1, delta1, gamma2, delta2, gamma3, delta3) {
  return(alpha0 + alpha1 * t + alpha2 * t^2 + alpha3 * t^3 +
           gamma1 * cos(2 * pi * t / 12) + delta1 * sin(2 * pi * t / 12) +
           gamma2 * cos(2 * pi * t / 6) + delta2 * sin(2 * pi * t / 6) +
           gamma3 * cos(2 * pi * t / 3) + delta3 * sin(2 * pi * t / 3))
}

# Função para ajustar e plotar
ajustar_e_plotar <- function(df, t, municipio) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  
  df_mun <- df %>% filter(municipio_geocodigo == municipio)
  df_mun <- df_mun %>%
    mutate(date = lubridate::floor_date(as.Date(DT_INTERN), unit = "week")) %>%
    group_by(date) %>%
    summarise(casos = sum(casos, na.rm = TRUE), .groups = 'drop')
  
  if (nrow(df_mun) < 10) return(NULL)
  
  t_vals <- 1:nrow(df_mun)
  y <- df_mun$casos
  
  tryCatch({
    fit <- nlsLM(y ~ modelo(t_vals, alpha0, alpha1, alpha2, alpha3, gamma1, delta1, gamma2, delta2, gamma3, delta3),
                 start = list(alpha0 = 1, alpha1 = 1, alpha2 = 1, alpha3 = 1, 
                              gamma1 = 1, delta1 = 1, gamma2 = 1, delta2 = 1, gamma3 = 1, delta3 = 1))
    
    y_fit <- predict(fit, list(t_vals = t_vals))
    sigma <- sqrt(deviance(fit) / df.residual(fit))
    y_fit_upper <- y_fit + 1.96 * sigma
    y_fit_lower <- y_fit - 1.96 * sigma
    
    plot_data <- data.frame(date = df_mun$date, casos = df_mun$casos, 
                            y_fit = y_fit, y_fit_upper = y_fit_upper, y_fit_lower = y_fit_lower, 
                            cod6 = municipio, tipo = df$TIPO[[1]])
    
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = "postgres",       # Nome do banco de dados
      host = "localhost",             # Endereço do servidor (ou IP)
      port = 5432,                    # Porta padrão do PostgreSQL
      user = "postgres",           # Usuário do banco de dados
      password = "vm190399"          # Senha do usuário
    )
    
    
    RPostgres::dbAppendTable(
      con,
      name = DBI::Id(schema = "inova", table = "resul_mod_recebidos_semana"),
      value = plot_data
    )
    RPostgres::dbDisconnect(con)
    
    #    write.table(plot_data, file = "/home/pascoal/Documentos/fv_proj_emergencias/resul_mod_enviados_para_novo.csv", append = TRUE, sep = ";", row.names = FALSE, col.names = FALSE)
    
    # Registrar sucesso no log
    write.table(data.frame(Tipo = t, Municipio = municipio, Status = "Concluído", Tempo = Sys.time()), 
                file = log_file, sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE)
    
  }, error = function(e) {
    write.table(data.frame(Tipo = t, Municipio = municipio, Status = "Erro", Tempo = Sys.time()), 
                file = log_file, sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE)
  })
}

rm(tb_emergencias_tips)
#munic <- def_munic_res[1]
#t = tipos[1]

# Executando em paralelo
for(t in tipos[6:11]){
  print(t)  
  tb_emergencias_tips <-consulta(paste0('SELECT "MUNIC_RES", "MUNIC_MOV", "TIPO", "DT_INTERN", n
	FROM inova.tb_emergencias_tips where "TIPO" = ',"'",t,"';"))
  
  for(munic in def_munic_res) {
    
    
    # teste <- tb_emergencias_tips %>% 
    #   dplyr::filter(MUNIC_MOV == munic)
    # 
    # if(dim(teste)[1]>2){
    #   print(munic)
    #   next()
    # }    
    
    
    ###Testar log
    # if(any(paste0(processamento_log$Tipo, processamento_log$Municipio) %in% paste0(t,munic)) == T){
    #   print(paste("pulou ", t, " - ", munic))
    #   next()
    # }
    #   
    
    df <- tb_emergencias_tips %>%
      filter(MUNIC_RES == munic, MUNIC_RES != MUNIC_MOV, TIPO == t) %>%
      mutate(municipio_geocodigo = MUNIC_RES, ano_e = format(DT_INTERN, "%Y"),
             se_e = lubridate::epiweek(DT_INTERN), casos = n) %>%
      group_by(municipio_geocodigo, TIPO, DT_INTERN, ano_e, se_e) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = 'drop') %>%
      as.data.frame()
    
    
    ajustar_e_plotar(df, t, munic)
  }
}

###Troquei a lógica
