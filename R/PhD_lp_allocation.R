PhD_lp_allocation <- function(cost, supply, capacity, solver = "Rglpk",
                              k = ncol(cost), verbose = TRUE,
                              limit = NA, scale = 1/20){

  if(!is.matrix(cost)) cli::cli_abort("`cost` debe ser un objeto de la clase `matrix`")

  cost <- PhD_lp_scale_cost(cost, scale = scale, limit = limit, verbose = verbose)

  m <- nrow(cost)
  n <- ncol(cost)

  if(length(supply) != m) cli::cli_abort("longitud `supply` != nrow(cost)")
  if(length(capacity) != n) cli::cli_abort("longitud `capacity` != ncol(cost)")
  if(any(supply < 0) || any(capacity < 0)) cli::cli_abort("`supply` o `capacity` negativos")

  if(!solver %in% c("Rglpk", "lpSolve")){
    cli::cli_abort("`solver` debe ser 'Rglpk' o 'lpSolve'")
  }

  # Definir componentes del modelo de programación lineal
  lp_comps <- PhD_lp_setup_model(solver = solver,
                                 cost = cost, supply = supply,
                                 capacity = capacity, k = k,
                                 verbose = verbose)

  invisible(gc())

  # Resolver el modelo de programación lineal
  cli::cli_alert_info("Solver iniciado...")

  res <- PhD_lp_solve(lp = lp_comps, solver = solver,
                      scale = scale, verbose = verbose)

  rownames(res$solution) <- rownames(cost)
  colnames(res$solution) <- colnames(cost)

  return(res)
}


