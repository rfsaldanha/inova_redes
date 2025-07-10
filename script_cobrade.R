# Script R para classificação de grupos de desastres baseado na COBRADE
# Versão usando dplyr (requer instalação do pacote)
# Classificação e Codificação Brasileira de Desastres

# Carregar biblioteca necessária
# install.packages("dplyr") # Descomente se necessário
library(dplyr)

# Função para extrair o grupo de desastre baseado no código COBRADE
classificar_grupo_cobrade <- function(cobrade) {
  # Extrair o primeiro dígito do código COBRADE para identificar o grupo principal
  grupo_principal <- substr(cobrade, 1, 1)

  # Se for grupo 1 (NATURAIS), extrair o segundo dígito para identificar o subgrupo
  if (grupo_principal == "1") {
    subgrupo <- substr(cobrade, 2, 2)

    grupo_desastre <- case_when(
      subgrupo == "1" ~ "Geológico",
      subgrupo == "2" ~ "Hidrológico",
      subgrupo == "3" ~ "Meteorológico",
      subgrupo == "4" ~ "Climatológico",
      subgrupo == "5" ~ "Biológico",
      TRUE ~ "Não classificado"
    )
  } else if (grupo_principal == "2") {
    # Se for grupo 2 (TECNOLÓGICOS)
    grupo_desastre <- "Tecnológico"
  } else {
    # Outros casos não previstos
    grupo_desastre <- "Não classificado"
  }

  return(grupo_desastre)
}

# Função para converter grupo de desastre em código numérico
# Baseado no modelo solicitado pelo usuário
codificar_grupo_numerico <- function(grupo_de_desastre) {
  codigo <- case_when(
    grupo_de_desastre == "Climatológico" ~ 1,
    grupo_de_desastre == "Geológico" ~ 2,
    grupo_de_desastre == "Hidrológico" ~ 3,
    grupo_de_desastre == "Meteorológico" ~ 4,
    grupo_de_desastre == "Biológico" ~ 5,
    grupo_de_desastre == "Tecnológico" ~ 6,
    TRUE ~ 0 # Para casos não classificados
  )

  return(codigo)
}

# Função completa que combina as duas etapas
processar_cobrade <- function(cobrade) {
  grupo <- classificar_grupo_cobrade(cobrade)
  codigo <- codificar_grupo_numerico(grupo)

  return(list(
    cobrade = cobrade,
    grupo = grupo,
    codigo = codigo
  ))
}

# Exemplo de aplicação em um data frame usando dplyr
exemplo_aplicacao_dplyr <- function(df, coluna_cobrade = "COBRADE") {
  # Verificar se a coluna existe
  if (!coluna_cobrade %in% names(df)) {
    stop(paste("Coluna", coluna_cobrade, "não encontrada no data frame"))
  }

  # Aplicar a classificação usando dplyr
  df <- df %>%
    mutate(
      grupo_de_desastre = sapply(
        .data[[coluna_cobrade]],
        classificar_grupo_cobrade
      ),
      codigo_grupo = case_when(
        grupo_de_desastre == "Climatológico" ~ 1,
        grupo_de_desastre == "Geológico" ~ 2,
        grupo_de_desastre == "Hidrológico" ~ 3,
        grupo_de_desastre == "Meteorológico" ~ 4,
        grupo_de_desastre == "Biológico" ~ 5,
        grupo_de_desastre == "Tecnológico" ~ 6,
        TRUE ~ 0
      )
    )

  return(df)
}

# Exemplo de data frame para teste com TODOS os códigos COBRADE
df_teste <- data.frame(
  COBRADE = c(
    # 1.1 - GEOLÓGICO
    "11110", # Terremoto - Tremor de terra
    "11120", # Terremoto - Tsunami
    "11200", # Emanação vulcânica
    "11311", # Movimento de massa - Quedas, tombamentos e rolamentos - Blocos
    "11312", # Movimento de massa - Quedas, tombamentos e rolamentos - Lascas
    "11313", # Movimento de massa - Quedas, tombamentos e rolamentos - Matacões
    "11314", # Movimento de massa - Quedas, tombamentos e rolamentos - Lajes
    "11321", # Movimento de massa - Deslizamentos de solo e/ou rocha
    "11331", # Movimento de massa - Corridas de massa - Solo/Lama
    "11332", # Movimento de massa - Corridas de massa - Rocha/Detrito
    "11340", # Movimento de massa - Subsidências e colapsos
    "11410", # Erosão - Erosão costeira/Marinha
    "11420", # Erosão - Erosão de margem fluvial
    "11431", # Erosão - Erosão continental - Laminar
    "11432", # Erosão - Erosão continental - Ravinas
    "11433", # Erosão - Erosão continental - Boçorocas

    # 1.2 - HIDROLÓGICO
    "12100", # Inundações
    "12200", # Enxurradas
    "12300", # Alagamentos

    # 1.3 - METEOROLÓGICO
    "13111", # Sistemas de grande escala - Ciclones - Ventos costeiros
    "13112", # Sistemas de grande escala - Ciclones - Marés de tempestade
    "13120", # Sistemas de grande escala - Frentes frias/Zonas de convergência
    "13211", # Tempestades - Tempestade local/Convectiva - Tornados
    "13212", # Tempestades - Tempestade local/Convectiva - Tempestade de raios
    "13213", # Tempestades - Tempestade local/Convectiva - Granizo
    "13214", # Tempestades - Tempestade local/Convectiva - Chuvas intensas
    "13215", # Tempestades - Tempestade local/Convectiva - Vendaval
    "13310", # Temperaturas extremas - Onda de calor
    "13321", # Temperaturas extremas - Onda de frio - Friagem
    "13322", # Temperaturas extremas - Onda de frio - Geadas

    # 1.4 - CLIMATOLÓGICO
    "14110", # Seca - Estiagem
    "14120", # Seca - Seca
    "14131", # Incêndio florestal em parques e áreas de proteção ambiental
    "14132", # Incêndios em áreas não protegidas
    "14140", # Baixa umidade do ar

    # 1.5 - BIOLÓGICO
    "15110", # Epidemias - Doenças infecciosas virais
    "15120", # Epidemias - Doenças infecciosas bacterianas
    "15130", # Epidemias - Doenças infecciosas parasíticas
    "15140", # Epidemias - Doenças infecciosas fúngicas
    "15210", # Infestações/Pragas - Infestações de animais
    "15221", # Infestações/Pragas - Infestações de algas - Marés vermelhas
    "15222", # Infestações/Pragas - Infestações de algas - Cianobactérias
    "15230", # Infestações/Pragas - Outras infestações

    # 2.1 - TECNOLÓGICOS - Desastres relacionados com substâncias radioativas
    "21110", # Desastres siderais com riscos radioativos - Queda de satélite
    "21210", # Desastres com substâncias e equipamentos radioativos - Fontes radioativas
    "21310", # Desastres relacionados com riscos de intensa poluição ambiental - Outras fontes

    # 2.2 - TECNOLÓGICOS - Desastres relacionados a produtos perigosos
    "22110", # Desastres em plantas e distritos industriais - Liberação de produtos químicos para a atmosfera
    "22210", # Desastres relacionados à contaminação da água - Liberação de produtos químicos nos sistemas de água potável
    "22220", # Desastres relacionados à contaminação da água - Derramamento de produtos químicos em ambiente lacustre, fluvial, marinho e aquífero
    "22310", # Desastres relacionados a conflitos bélicos - Liberação de produtos químicos e contaminação
    "22410", # Desastres relacionados a transporte de produtos perigosos - Transporte rodoviário
    "22420", # Desastres relacionados a transporte de produtos perigosos - Transporte ferroviário
    "22430", # Desastres relacionados a transporte de produtos perigosos - Transporte aéreo
    "22440", # Desastres relacionados a transporte de produtos perigosos - Transporte dutoviário
    "22450", # Desastres relacionados a transporte de produtos perigosos - Transporte marítimo
    "22460", # Desastres relacionados a transporte de produtos perigosos - Transporte aquaviário

    # 2.3 - TECNOLÓGICOS - Desastres relacionados a incêndios urbanos
    "23110", # Incêndios urbanos - Incêndios em plantas e distritos industriais
    "23120", # Incêndios urbanos - Incêndios em aglomerados residenciais

    # 2.4 - TECNOLÓGICOS - Desastres relacionados a obras civis
    "24100", # Colapso de edificações
    "24200", # Rompimento/colapso de barragens

    # 2.5 - TECNOLÓGICOS - Desastres relacionados a transporte de passageiros e cargas não perigosas
    "25100", # Transporte rodoviário
    "25200", # Transporte ferroviário
    "25300", # Transporte aéreo
    "25400", # Transporte marítimo
    "25500" # Transporte aquaviário
  ),

  evento = c(
    # 1.1 - GEOLÓGICO
    "Tremor de terra",
    "Tsunami",
    "Emanação vulcânica",
    "Quedas, tombamentos e rolamentos - Blocos",
    "Quedas, tombamentos e rolamentos - Lascas",
    "Quedas, tombamentos e rolamentos - Matacões",
    "Quedas, tombamentos e rolamentos - Lajes",
    "Deslizamentos de solo e/ou rocha",
    "Corridas de massa - Solo/Lama",
    "Corridas de massa - Rocha/Detrito",
    "Subsidências e colapsos",
    "Erosão costeira/Marinha",
    "Erosão de margem fluvial",
    "Erosão continental - Laminar",
    "Erosão continental - Ravinas",
    "Erosão continental - Boçorocas",

    # 1.2 - HIDROLÓGICO
    "Inundações",
    "Enxurradas",
    "Alagamentos",

    # 1.3 - METEOROLÓGICO
    "Ciclones - Ventos costeiros",
    "Ciclones - Marés de tempestade",
    "Frentes frias/Zonas de convergência",
    "Tempestade local - Tornados",
    "Tempestade local - Tempestade de raios",
    "Tempestade local - Granizo",
    "Tempestade local - Chuvas intensas",
    "Tempestade local - Vendaval",
    "Temperaturas extremas - Onda de calor",
    "Temperaturas extremas - Friagem",
    "Temperaturas extremas - Geadas",

    # 1.4 - CLIMATOLÓGICO
    "Seca - Estiagem",
    "Seca - Seca",
    "Incêndio florestal em áreas protegidas",
    "Incêndios em áreas não protegidas",
    "Baixa umidade do ar",

    # 1.5 - BIOLÓGICO
    "Epidemias - Doenças virais",
    "Epidemias - Doenças bacterianas",
    "Epidemias - Doenças parasíticas",
    "Epidemias - Doenças fúngicas",
    "Infestações de animais",
    "Infestações de algas - Marés vermelhas",
    "Infestações de algas - Cianobactérias",
    "Outras infestações",

    # 2.1 - TECNOLÓGICOS - Substâncias radioativas
    "Queda de satélite radioativo",
    "Fontes radioativas em processos industriais",
    "Outras fontes de poluição radioativa",

    # 2.2 - TECNOLÓGICOS - Produtos perigosos
    "Explosão/incêndio em plantas industriais",
    "Contaminação de água potável",
    "Derramamento em ambiente aquático",
    "Conflitos bélicos com produtos químicos",
    "Transporte rodoviário de produtos perigosos",
    "Transporte ferroviário de produtos perigosos",
    "Transporte aéreo de produtos perigosos",
    "Transporte dutoviário de produtos perigosos",
    "Transporte marítimo de produtos perigosos",
    "Transporte aquaviário de produtos perigosos",

    # 2.3 - TECNOLÓGICOS - Incêndios urbanos
    "Incêndios em plantas industriais",
    "Incêndios em aglomerados residenciais",

    # 2.4 - TECNOLÓGICOS - Obras civis
    "Colapso de edificações",
    "Rompimento/colapso de barragens",

    # 2.5 - TECNOLÓGICOS - Transporte não perigoso
    "Transporte rodoviário não perigoso",
    "Transporte ferroviário não perigoso",
    "Transporte aéreo não perigoso",
    "Transporte marítimo não perigoso",
    "Transporte aquaviário não perigoso"
  )
)

cat("Exemplo com data frame usando dplyr:\n")
cat("====================================\n")
df_resultado <- exemplo_aplicacao_dplyr(desastres)
print(df_resultado)

# Resumo da classificação
cat("\n\nResumo da classificação numérica:\n")
cat("=================================\n")
cat("1 - Climatológico\n")
cat("2 - Geológico\n")
cat("3 - Hidrológico\n")
cat("4 - Meteorológico\n")
cat("5 - Biológico\n")
cat("6 - Tecnológico\n")
cat("0 - Não classificado\n")
