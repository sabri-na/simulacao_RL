# # Funcoes de avaliacao e classificacao dos clusters



classify_and_evaluate <- function(centroids, assignments, ds1, ds2, pairs) {
  
  # 1. Classificar os clusters como L (Link) ou N (Non-Link)
  classify_LN <- function(centroids, assignments, ds1, ds2, pairs) {
    # CRITÉRIO: O cluster mais próximo do vetor 'ones' (1,1,1...) é o cluster de Links (L).
    # O vetor 'ones' representa a similaridade perfeita em todas as dimensões.
    ones <- rep(1, ncol(centroids))
    dist_to_ones <- apply(centroids, 1, function(v) sqrt(sum((v - ones)^2)))
    
    cluster_labels <- rep("N", length(dist_to_ones))
    # Atribui 'L' ao centróide cuja distância para 'ones' é a mínima
    cluster_labels[which.min(dist_to_ones)] <- "L"
    
    pares_classificados <- data.frame(
      id_ds1 = ds1$id[pairs[,1]],
      id_ds2 = ds2$id[pairs[,2]],
      # Atribuição da classificação L/N aos pares baseada no cluster
      classificacao = cluster_labels[assignments],
      stringsAsFactors = FALSE
    )
    return(pares_classificados)
  }
  
  # 2. Função de avaliação (ajustada para retornar data.frame)
  evaluate_measures <- function(pares_com_ids, model_name) {
    
    # --- Preparação de Dados e Verdade ---
    if (nrow(pares_com_ids) == 0) {
      # Retorno vazio para evitar erro
      return(data.frame(Model = model_name, TP=0, FP=0, FN=0, TN=0, precisao=NA, sensibilidade=NA, especificidade=NA, f1=NA, acuracia=NA))
    }
    
    truth <- pares_com_ids$id_ds1 == pares_com_ids$id_ds2
    pred <- pares_com_ids$classificacao == "L"
    
    valid_cases <- complete.cases(truth, pred)
    truth <- truth[valid_cases]
    pred <- pred[valid_cases]
    
    if (length(truth) == 0) {
      return(data.frame(Model = model_name, TP=0, FP=0, FN=0, TN=0, precisao=NA, sensibilidade=NA, especificidade=NA, f1=NA, acuracia=NA))
    }
    
    # --- Cálculo da Matriz de Confusão ---
    TP <- sum(truth & pred)    # Verdadeiro Positivo
    TN <- sum(!truth & !pred)  # Verdadeiro Negativo
    FP <- sum(!truth & pred)   # Falso Positivo
    FN <- sum(truth & !pred)   # Falso Negativo
    total <- TP + TN + FP + FN
    
    # --- Cálculo das Métricas ---
    precisao <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
    sensibilidade <- ifelse((TP + FN) > 0, TP / (TP + FN), 0) # Recall
    acuracia <- ifelse(total > 0, (TP + TN) / total, 0)
    especificidade <- ifelse((TN + FP) > 0, TN / (TN + FP), 0)
    
    # Cálculo F1-Score
    f1 <- ifelse((precisao + sensibilidade) > 0,
                 2 * (precisao * sensibilidade) / (precisao + sensibilidade), 0)
    
    # --- Retorno como DATA FRAME de UMA LINHA ---
    metrics_df <- data.frame(
      Model = model_name,
      TP = TP, 
      FP = FP, 
      FN = FN, 
      TN = TN,
      precisao = round(precisao, 4),
      sensibilidade = round(sensibilidade, 4),
      especificidade = round(especificidade, 4),
      f1 = round(f1, 4),
      acuracia = round(acuracia, 4),
      stringsAsFactors = FALSE
    )
    
    return(metrics_df)
  }
  
  # 3. Executar classificação e avaliação (Chamada)
  pares_classificados <- classify_LN(centroids, assignments, ds1, ds2, pairs)
  
  # Adiciona o nome do modelo na chamada, se necessário 
  metrics_result <- evaluate_measures(pares_classificados, model_name = "ClusterModel") 
  
  # 4. Retornar apenas as métricas (o que você estava usando)
  return(list(
    # Você não precisa retornar 'pares_classificados' se só for usar as métricas.
    # Se precisar, mantenha: pares_classificados = pares_classificados,
    metrics = metrics_result # Agora é um data.frame de 1 linha.
  ))
}


