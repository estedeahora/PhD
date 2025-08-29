
PhD_lp_setup_model <- function(solver, cost, supply, capacity, k, verbose){

  if(solver == "Rglpk"){
    res <- PhD_lp_setup_model_Rglpk(cost, supply, capacity, k, verbose)
  }else if(solver == "lpSolve"){
    res <- PhD_lp_setup_model_lpSolve(cost, supply, capacity, verbose)
  }

  return(res)
}

PhD_lp_setup_model_Rglpk <- function(cost, supply, capacity, k, verbose){

  m <- nrow(cost)
  n <- ncol(cost)

  # 1. Definir valor de k
  # ---------------------*

  if(k > n){
    if(verbose){
      cli::cli_alert_warning("k ({k}) es mayor que n ({n}). Se ajusta k = n")
    }
    k <- n
  } else if(k < 1){
    k <- round(n * k)
  }
  if(verbose){
    cli::cli_alert_info("Usando k = {k} ({round(k/n*100, 1)}% de escuelas).")
  }

  # 2. Obtener arcos activos
  # -----------------------*
  # Obtener el conjunto de arcos (E) de los `k` menores costos por fila

  E <- PhD_lp_make_edges(cost, k)
  if(verbose){
    cli::cli_alert_info("Cantidad inicial de arcos: {nrow(E)}.")
  }

  # 3. Definir Componentes
  # ---------------------*
  p <- nrow(E)

    # 3.A Objetivo (`obj`)

    obj <- cost[E]

    # 3.B Matriz de restricciones (`mat`)

    # Restricciones SPACE: sum_j x_ij = supply_i
    A_rows <- slam::simple_triplet_matrix(
      i = E[, "i"],         # filas (orígenes)
      j = seq_len(p), # columnas (arcos)
      v = rep(TRUE, p),
      nrow = m, ncol = nrow(E))  # m filas, p columnas

    # Restricciones SCHOOL: sum_i x_ij <= capacity_j
    A_cols <- slam::simple_triplet_matrix(
      i = E[, "j"],         # filas
      j = seq_len(p),       # columnas
      v = rep(TRUE, p),
      nrow = n, ncol = nrow(E))

    cat("Dimensiones A_rows",dim(A_rows))
    cat("Dimensiones A_cols",dim(A_cols))

    mat <- rbind(A_rows, A_cols)

    # 3.C Dirección de las desigualdades de filas y columnas (`dir`)
    if(sum(supply) <= sum(capacity)){
      dir <- c(rep ("==", m), rep ("<=", n))
    }else{
      dir <- c(rep ("<=", m), rep ("==", n))
    }

    # 3.D Lado derecho de las desigualdades (`rhs`)
    rhs <- c(supply, capacity)

    # 3.E Límites de las variables (`bounds`)
    bounds <- list(lower = list(ind = seq_len(p), val = rep(0, p)))

  # 4. Resultado
  # -----------*

  res <- list(obj = obj, mat = mat, dir = dir, rhs = rhs,
              bounds = bounds, dims = c(m = m, n = n), E = E)
  return(res)
}

PhD_lp_setup_model_lpSolve <- function(cost, supply, capacity, verbose){

  if(sum(supply) <= sum(capacity)){
    supply_signs <- rep ("=", nrow(cost))
    capacity_signs <- rep ("<=", ncol(cost))
  }else{
    supply_signs <- rep ("<=", nrow(cost))
    capacity_signs <- rep ("=", ncol(cost))
  }

  # Resultado
  # --------*

  res <- list(cost = cost,
              supply = supply, capacity = capacity,
              supply_signs = supply_signs,
              capacity_signs = capacity_signs)

  return(res)
}
