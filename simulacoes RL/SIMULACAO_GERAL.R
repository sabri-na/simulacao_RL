#===========================================================
## Carregamento de Pacotes
#===========================================================

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(RecordLinkage, tidyverse, data.table, tictoc) 

#===========================================================
## Funcoes Auxiliares
#===========================================================

source("Funcoes\\evaluation_functions.R")
source("Funcoes\\kmeans_functions.R")
source("Funcoes\\sbe_functions.R")
source("Funcoes\\prob_functions.R")


#===========================================================
## Funcao para Executar os cenarios
#===========================================================

run_record_linkage_scenario1 <- function(file_path_1, file_path_2, scenario_name) {
  
  tic(paste("CENARIO COMPLETO:", scenario_name))
  
  # --- 1. Carregamento e Pre-processamento ---
  tic("Carregar ds1 e ds2")
  ds1 <- fread(file_path_1)
  ds2 <- fread(file_path_2)
  toc(log = TRUE)
  colnames(ds1) <- c("id", "sobrenome", "nome", "dt_obito", "dt_nacs")
  colnames(ds2) <- c("id", "sobrenome", "nome", "dt_obito", "dt_nacs")
  
  
  # --- 2. Comparacao (Gerar Pares) ---
  tic("Comparacao - compare.linkage")
  rpairs <- compare.linkage(
    ds1, ds2,
    blockfld = c("sobrenome"),
    strcmp = TRUE,
    strcmpfun = jarowinkler,
    exclude = "id"
  )
  toc(log = TRUE)
  rpairs_compare <- rpairs$pairs[, 3:6]
  
  
  # --- 3. Modelo SBE ---
  set.seed(2287)
  tic("Rodar SBE")
  
  sbe_model <- sbe_kmeans(rpairs_compare, K=2, gamma0=0.0001, M=100,
                          alpha=0.75, beta=0.95, omaxit=40, imaxit=5)
  toc(log = TRUE)
  metrics_sbe <- classify_and_evaluate(sbe_model$centroids, sbe_model$assignments, ds1, ds2, rpairs$pairs)$metrics
  metrics_sbe$Model <- "SBE"
  
  # --- 4. K-means Tradicional ---
  set.seed(2287)
  tic("Rodar K-means tradicional")
  traditional_centroids <- traditional_kmeans(rpairs_compare, K = 2)
  toc(log = TRUE)
  metrics_kmeans <- classify_and_evaluate(traditional_centroids$centroids, traditional_centroids$assignments, ds1, ds2, rpairs$pairs)$metrics
  metrics_kmeans$Model <- "Traditional-Kmeans"
  
  # --- 5. Linkage Probabilistico ---
  tic("Rodar Probabilistico")
  rl_prob <- probabilistic_linkage(rpairs, threshold = 0.93)
  toc(log = TRUE)
  
  metrics_prob <- rl_prob_metrics(rl_prob)
  
  # --- 6. Coleta de Resultados e Tempos ---
  
  # Junta todas as metricas em um unico data.frame
  all_metrics <- bind_rows(metrics_sbe, metrics_kmeans, metrics_prob)
  
  toc(log = TRUE) 
  
  # Retorna resultados e tempos para analise
  return(list(
    scenario = scenario_name,
    metrics = all_metrics,
    timelog = tic.log(format = FALSE)
  ))
}


run_record_linkage_scenario2 <- function(file_path_1, file_path_2, scenario_name) {
  
  tic(paste("CENARIO COMPLETO:", scenario_name))
  
  # --- 1. Carregamento e Pre-processamento ---
  tic("Carregar ds1 e ds2")
  ds1 <- fread(file_path_1)
  ds2 <- fread(file_path_2)
  toc(log = TRUE)
  colnames(ds1) <- c("id", "sobrenome", "nome", "dt_obito", "dt_nacs")
  colnames(ds2) <- c("id", "sobrenome", "nome", "dt_obito", "dt_nacs")
  
  # --- 2. padronizacao das variaveis---
  
  char_cols <- names(ds1)[sapply(ds1, is.character)]
  ds1[, (char_cols) := lapply(.SD, tolower), .SDcols = char_cols]
  ds1 <- unique(ds1, by = c("id", "sobrenome", "nome", "dt_obito", "dt_nacs"))
  
  char_cols <- names(ds2)[sapply(ds2, is.character)]
  ds2[, (char_cols) := lapply(.SD, tolower), .SDcols = char_cols]
  ds2 <- unique(ds2, by = c("id", "sobrenome", "nome", "dt_obito", "dt_nacs"))
  
 
  # --- 4. Comparacao (Gerar Pares) ---
  tic("Comparacao - compare.linkage")
  rpairs <- compare.linkage(
    ds1, ds2,
    blockfld = c("sobrenome"),
    strcmp = TRUE,
    strcmpfun = jarowinkler,
    exclude = "id"
  )
  toc(log = TRUE)
  rpairs_compare <- rpairs$pairs[, 3:6]
  
  
  # --- 5. Modelo SBE ---
  set.seed(2287)
  tic("Rodar SBE")
  
  sbe_model <- sbe_kmeans(rpairs_compare, K=2, gamma0=0.0001, M=100,
                          alpha=0.75, beta=0.95, omaxit=40, imaxit=5)
  toc(log = TRUE)
  metrics_sbe <- classify_and_evaluate(sbe_model$centroids, sbe_model$assignments, ds1, ds2, rpairs$pairs)$metrics
  metrics_sbe$Model <- "SBE"
  
  # --- 6. K-means Tradicional ---
  set.seed(2287)
  tic("Rodar K-means tradicional")
  traditional_centroids <- traditional_kmeans(rpairs_compare, K = 2)
  toc(log = TRUE)
  metrics_kmeans <- classify_and_evaluate(traditional_centroids$centroids, traditional_centroids$assignments, ds1, ds2, rpairs$pairs)$metrics
  metrics_kmeans$Model <- "Traditional-Kmeans"
  
  # --- 7. Linkage Probabilistico ---
  tic("Rodar Probabilistico")
  rl_prob <- probabilistic_linkage(rpairs, threshold = 0.93)
  toc(log = TRUE)
  
  metrics_prob <- rl_prob_metrics(rl_prob)
  
  # --- 8. Coleta de Resultados e Tempos ---
  
  # Junta todas as metricas em um unico data.frame
  all_metrics <- bind_rows(metrics_sbe, metrics_kmeans, metrics_prob)
  
  toc(log = TRUE) 
  
  # Retorna resultados e tempos para analise
  return(list(
    scenario = scenario_name,
    metrics = all_metrics,
    timelog = tic.log(format = FALSE)
  ))
}


run_record_linkage_scenario3 <- function(file_path_1, file_path_2, scenario_name) {
  
  tic(paste("CENARIO COMPLETO:", scenario_name))
  
  # --- 1. Carregamento e Pre-processamento ---
  tic("Carregar ds1 e ds2")
  ds1 <- fread(file_path_1)
  ds2 <- fread(file_path_2)
  toc(log = TRUE)
  colnames(ds1) <- c("id", "sobrenome", "nome", "dt_obito", "dt_nacs")
  colnames(ds2) <- c("id", "sobrenome", "nome", "dt_obito", "dt_nacs")
  
  # --- 2. padronizacao das variaveis---
  
  char_cols <- names(ds1)[sapply(ds1, is.character)]
  ds1[, (char_cols) := lapply(.SD, tolower), .SDcols = char_cols]
  ds1 <- unique(ds1, by = c("id", "sobrenome", "nome", "dt_obito", "dt_nacs"))
  
  char_cols <- names(ds2)[sapply(ds2, is.character)]
  ds2[, (char_cols) := lapply(.SD, tolower), .SDcols = char_cols]
  ds2 <- unique(ds2, by = c("id", "sobrenome", "nome", "dt_obito", "dt_nacs"))
  
    
  # --- 4. Criacao do campo de blocking ---
  
  ds1 <- ds1 %>%
    mutate(
      block_field = paste0(
        sobrenome,
        #str_sub(nome, 1, 2),              # 3 primeiros caracteres do nome
        str_sub(sprintf("%08d", dt_obito), -4) # ultimos 4 digitos da data (ano)
       )
    )
  
  
  ds2 <- ds2 %>%
    mutate(
      block_field = paste0(
        sobrenome,
        #str_sub(nome, 1, 2),              # 3 primeiros caracteres do nome
        str_sub(sprintf("%08d", dt_obito), -4) # ultimos 4 digitos da data (ano)
      )
    )
  
  
  # --- 5. Comparacao (Gerar Pares) ---
  tic("Comparacao - compare.linkage")
  rpairs <- compare.linkage(
    ds1, ds2,
    blockfld = c("block_field"),
    strcmp = TRUE,
    strcmpfun = jarowinkler,
    exclude = c("id", "block_field")
  )
  toc(log = TRUE)
  rpairs_compare <- rpairs$pairs[, 3:6]
  
  
  # --- 6. Modelo SBE ---
  set.seed(2287)
  tic("Rodar SBE")
  
  sbe_model <- sbe_kmeans(rpairs_compare, K=2, gamma0=0.0001, M=100,
                          alpha=0.75, beta=0.95, omaxit=40, imaxit=5)
  toc(log = TRUE)
  metrics_sbe <- classify_and_evaluate(sbe_model$centroids, sbe_model$assignments, ds1, ds2, rpairs$pairs)$metrics
  metrics_sbe$Model <- "SBE"
  
  # --- 7. K-means Tradicional ---
  set.seed(2287)
  tic("Rodar K-means tradicional")
  traditional_centroids <- traditional_kmeans(rpairs_compare, K = 2)
  toc(log = TRUE)
  metrics_kmeans <- classify_and_evaluate(traditional_centroids$centroids, traditional_centroids$assignments, ds1, ds2, rpairs$pairs)$metrics
  metrics_kmeans$Model <- "Traditional-Kmeans"
  
  # --- 8. Linkage Probabilistico ---
  tic("Rodar Probabilistico")
  rl_prob <- probabilistic_linkage(rpairs, threshold = 0.93)
  toc(log = TRUE)
  
  metrics_prob <- rl_prob_metrics(rl_prob)
  
  # --- 9. Coleta de Resultados e Tempos ---
  
  # Junta todas as metricas em um unico data.frame
  all_metrics <- bind_rows(metrics_sbe, metrics_kmeans, metrics_prob)
  
  toc(log = TRUE) 
  
  # Retorna resultados e tempos para analise
  return(list(
    scenario = scenario_name,
    metrics = all_metrics,
    timelog = tic.log(format = FALSE)
  ))
}

#===========================================================
## Execucao do Cenario 1


# Limpa o log de tempo antes de comecar
tic.clearlog()
base_path <- "dados\\"

# D1.1
result_scenario_1.1 <- run_record_linkage_scenario1(
  file_path_1 = paste0(base_path, "ds1.1.1"),
  file_path_2 = paste0(base_path, "ds1.1.2"),
  scenario_name = "Cenario 1: DS1"
)

View(result_scenario_1.1$metrics)

#D2.1
result_scenario_1.2 <- run_record_linkage_scenario1(
  file_path_1 = paste0(base_path, "ds2.1.1"),
  file_path_2 = paste0(base_path, "ds2.1.2"),
  scenario_name = "Cenario 1: DS2"
)

View(result_scenario_1.2$metrics)

#D3.1
result_scenario_1.3 <- run_record_linkage_scenario1(
  file_path_1 = paste0(base_path, "ds3.1.1"),
  file_path_2 = paste0(base_path, "ds3.1.2"),
  scenario_name = "Cenario 1: DS3"
)

View(result_scenario_1.3$metrics)

#D4.1
# result_scenario_1.3 <- run_record_linkage_scenario1(
#   file_path_1 = paste0(base_path, "ds4.1.1"),
#   file_path_2 = paste0(base_path, "ds4.1.2"),
#   scenario_name = "Cenario 1: DS3"
# )
# 
# View(result_scenario_1.3$metrics)
#===========================================================

#===========================================================
## Execucao do Cenario 2


# Limpa o log de tempo antes de comecar
tic.clearlog()
base_path <- "dados\\"

# D1.1
result_scenario_2.1 <- run_record_linkage_scenario2(
  file_path_1 = paste0(base_path, "ds1.1.1"),
  file_path_2 = paste0(base_path, "ds1.1.2"),
  scenario_name = "Cenario 2: DS1"
)

View(result_scenario_2.1$metrics)

#D2.1
result_scenario_2.2 <- run_record_linkage_scenario2(
  file_path_1 = paste0(base_path, "ds2.1.1"),
  file_path_2 = paste0(base_path, "ds2.1.2"),
  scenario_name = "Cenario 2: DS2"
)

View(result_scenario_2.2$metrics)

#D3.1
result_scenario_2.3 <- run_record_linkage_scenario2(
  file_path_1 = paste0(base_path, "ds3.1.1"),
  file_path_2 = paste0(base_path, "ds3.1.2"),
  scenario_name = "Cenario 2: DS3"
)


View(result_scenario_2.3$metrics)
#===========================================================


#===========================================================
## Execucao do Cenario 3
#===========================================================

# Limpa o log de tempo antes de comecar
tic.clearlog()
base_path <- "dados\\"

# D1.1
result_scenario_3.1 <- run_record_linkage_scenario3(
  file_path_1 = paste0(base_path, "ds1.1.1"),
  file_path_2 = paste0(base_path, "ds1.1.2"),
  scenario_name = "Cenario 3: DS1"
)

View(result_scenario_3.1$metrics)

#D2.1
result_scenario_3.2 <- run_record_linkage_scenario3(
  file_path_1 = paste0(base_path, "ds2.1.1"),
  file_path_2 = paste0(base_path, "ds2.1.2"),
  scenario_name = "Cenario 3: DS2"
)

View(result_scenario_3.2$metrics)

#D3.1
result_scenario_3.3 <- run_record_linkage_scenario3(
  file_path_1 = paste0(base_path, "ds3.1.1"),
  file_path_2 = paste0(base_path, "ds3.1.2"),
  scenario_name = "Cenario 3: DS3"
)


View(result_scenario_3.3$metrics)



