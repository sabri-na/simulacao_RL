# Funcao Rl Probailistico

probabilistic_linkage <- function(rpairs, threshold) {
  
  # 1. Aplicar o modelo probabilistico (calculo dos pesos)
  rl_matches <- RecordLinkage::epiWeights(rpairs)
  
  # 2. Obter todos os pares com os pesos calculados
  all_pairs <- as.data.frame(RecordLinkage::getPairs(rl_matches, min.weight = 0, max.weight = 1, single.rows = TRUE))
  
  # 3. Atribuicao binaria
  all_pairs$pred_match_model <- ifelse(all_pairs$Weight >= threshold, 1, 0)
  
  # 4. Atribuir a verdade (necessario para o calculo das metricas)
  all_pairs$true_match <- ifelse(all_pairs$id.1 == all_pairs$id.2, 1, 0)
  
  return(all_pairs)
}


rl_prob_metrics <- function(classified_pairs) {
  
  # Logica de calculo de metricas (Matriz de Confusao)
  table_conf <- table(Predito = classified_pairs$pred_match_model, Verdadeiro = classified_pairs$true_match)
  if(any(is.na(table_conf))) table_conf[is.na(table_conf)] <- 0
  
  # Extracao dos valores com protecao contra classes faltantes
  TP <- ifelse("1" %in% rownames(table_conf) && "1" %in% colnames(table_conf), table_conf["1", "1"], 0)
  FP <- ifelse("1" %in% rownames(table_conf) && "0" %in% colnames(table_conf), table_conf["1", "0"], 0)
  FN <- ifelse("0" %in% rownames(table_conf) && "1" %in% colnames(table_conf), table_conf["0", "1"], 0)
  TN <- ifelse("0" %in% rownames(table_conf) && "0" %in% colnames(table_conf), table_conf["0", "0"], 0)
  
  total <- TP + TN + FP + FN
  
  # Calculo das metricas existentes
  precisao <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
  sensibilidade <- ifelse((TP + FN) > 0, TP / (TP + FN), 0) # Recall
  f1 <- ifelse((precisao + sensibilidade) > 0, 2 * (precisao * sensibilidade) / (precisao + sensibilidade), 0)
  especificidade <- ifelse((TN + FP) > 0, TN / (TN + FP), 0)
  acuracia <- ifelse(total > 0, (TP + TN) / total, 0)
  
  # Retorna um data.frame com todas as metricas
  return(data.frame(
    Model = "Probabilistico", 
    TP, FP, FN, TN, 
    precisao, sensibilidade, 
    especificidade, 
    acuracia,       
    f1
  ))
}
