# Implentacao do algoritmo Kmeans

traditional_kmeans <- function(data, K, max_iter = 100) {
  # Converter para matriz (mais rapido que data.frame)
  data <- as.matrix(data)
  
  # Inicializacao aleatoria dos centroides
  initial_indices <- sample(1:nrow(data), K)
  centroids <- data[initial_indices, , drop = FALSE]
  
  for (iter in 1:max_iter) {
    old_centroids <- centroids
    
    # Calcular distancias ponto-centroide
    # ||x - y||^2 = ||x||^2 + ||y||^2 - 2 * x?Ey
    distances <- 
      matrix(rowSums(data^2), nrow(data), K) +
      matrix(rowSums(centroids^2), nrow(data), K, byrow = TRUE) -
      2 * (data %*% t(centroids))
    
    # Atribuicoes
    assignments <- max.col(-distances) 
    
    # Atualizar centroides
    centroids <- rowsum(data, assignments) / as.vector(tabulate(assignments, nbins = K))
    
    # Verificar convergencia
    if (all(abs(centroids - old_centroids) < 1e-6, na.rm = TRUE)) {
      break
    }
  }
  
  list(
    centroids = centroids,
    assignments = assignments
  )

}
