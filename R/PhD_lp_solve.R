PhD_lp_solve <- function(lp, solver, scale, verbose){

  if(solver == "Rglpk"){
    res <- PhD_lp_solve_Rglpk(lp, scale, verbose)
  }else if(solver == "lpSolve"){
    res <- PhD_lp_solve_lpSolve(lp, scale, verbose)
  }

  return(res)
}

PhD_lp_solve_Rglpk <- function(lp, scale, verbose){

  sol <- Rglpk::Rglpk_solve_LP(obj = lp$obj, mat = lp$mat,
                               dir = lp$dir, rhs = lp$rhs,
                               bounds = lp$bounds, max = FALSE,
                               types = rep("C", length(lp$obj)),
                               control = list(presolve = TRUE, verbose = verbose))

  x <- matrix(0, lp$dims["m"], lp$dims["n"])
  x[lp$E] <- sol$solution
  sol$solution <- x

  sol$optimum <- PhD_lp_scale_cost(sol$optimum, scale = scale,
                                   inverse = TRUE, verbose = verbose)

  return(sol)
}

PhD_lp_solve_lpSolve <- function(lp, scale, verbose){

  sol <- lpSolve::lp.transport(cost.mat = lp$cost, direction = "min", presolve = 1,
                               row.rhs = lp$supply, row.signs = lp$supply_signs,
                               col.rhs = lp$capacity, col.signs = lp$capacity_signs)

  sol$objval <-  PhD_lp_scale_cost(sol$objval, scale = scale,
                                   inverse = TRUE, verbose = verbose)

  return(sol)
}
