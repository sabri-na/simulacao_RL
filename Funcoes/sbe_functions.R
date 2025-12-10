
calculate_gradient <- function(data, centroids, batch_indices) {
  batch_data <- data[batch_indices, , drop = FALSE]
  n_batch <- nrow(batch_data)
  K <- nrow(centroids)
  
  # Calculo da distancia quadrática 
  distances <- 
    matrix(rowSums(batch_data^2), n_batch, K) +
    matrix(rowSums(centroids^2), n_batch, K, byrow = TRUE) -
    2 * (batch_data %*% t(centroids))
  
  # Atribuições
  assignments <- max.col(-distances) 
  
  # Calcular o gradiente 
  gradient <- matrix(0, nrow = K, ncol = ncol(centroids))
  for (j in 1:K) {
    points_in_cluster <- batch_data[assignments == j, , drop = FALSE]
    if (nrow(points_in_cluster) > 0) {
      # Gradiente é a diferença entre o centróide e a média do cluster
      gradient[j, ] <- as.numeric(centroids[j, ] - colMeans(points_in_cluster))
    }
  }
  return(gradient)
}

# Função que retorna apenas os centroides
sbe_kmeans_centroids <- function(data, K, gamma0, M, alpha, beta, omaxit, imaxit) {

  data <- as.matrix(data)
  initial_indices <- sample(1:nrow(data), K)
  centroids <- data[initial_indices, , drop = FALSE] 

  current_centroids <- centroids
  gamma_k <- gamma0
  
  for (k in 1:omaxit) { 
    y0_k <- current_centroids   # y^{0,k} = x^{k-1}
    xk <- y0_k                  # x^k inicializado
    
    y_l <- y0_k                 # comeca com y^{0,k}
    
    for (l in 1:imaxit) { 
      # Mini-batch
      batch_indices <- sample(1:nrow(data), M)
      
      # Gradiente avaliado em y_{l-1,k}
      gradient_l_phi <- calculate_gradient(data, y_l, batch_indices)
      
      # Update de y^{l,k} = x^{k-1} - ??^k * grad(y^{l-1,k})
      y_l <- y0_k - gamma_k * gradient_l_phi
      
      # Atualização da média exponencial:
      xk <- alpha * xk + (1 - alpha) * y_l
    }
    
    # Atualizar os centroides
    current_centroids <- xk
    
    # Decaimento do step size
    gamma_k <- beta * gamma_k
  }
  
  return(current_centroids)
}


# Função que retorna assignments e objective com pesos (opcional) por variável
assign_points_and_objective <- function(data, centroids, weights = NULL) {
  # Garante que os dados e centróides são matrizes numéricas
  data <- as.matrix(data)
  centroids <- as.matrix(centroids)
  
  n <- nrow(data)
  K <- nrow(centroids)
  p <- ncol(data)
  
  # Se o usuário não passar pesos, usa pesos iguais
  if (is.null(weights)) {
    weights <- rep(1, p)
  }
  
  # Normaliza os pesos para somarem 1 (
  weights <- weights / sum(weights)
  
  # Matriz n x K com distâncias quadráticas ponderadas
  distances_to_centroids <- matrix(0, n, K)
  
  for (k in 1:K) {
    # calcula soma dos quadrados ponderada das diferenças
    diff <- data - matrix(centroids[k, ], n, p, byrow = TRUE)
    # aplica o peso em cada coluna (variável)
    distances_to_centroids[, k] <- rowSums(weights * (diff^2))
  }
  
  # atribuição: índice do centróide mais próximo (distância menor)
  assignments <- max.col(-distances_to_centroids)
  
  # função objetivo: soma da menor distância quadrática ponderada por ponto
  objective <- sum(apply(distances_to_centroids, 1, min))
  
  return(list(assignments = assignments, objective = objective))
}


# Função original mantida para compatibilidade (opcional)
sbe_kmeans <- function(data, K, gamma0, M, alpha, beta, omaxit, imaxit, weights = NULL) {
  centroids <- sbe_kmeans_centroids(data, K, gamma0, M, alpha, beta, omaxit, imaxit)
  assignments_obj <- assign_points_and_objective(data, centroids, )
  
  return(list(
    centroids = centroids,
    assignments = assignments_obj$assignments,
    objective = assignments_obj$objective 
  ))
}

