PhD_lp_scale_cost <- function(x, scale = NA, limit = NA, inverse = FALSE, verbose){

  if(!is.na(limit)){
    x[x > limit] <- limit
    if(verbose){
      cli::cli_alert_info("Valores mayores a {limit} son truncados")
    }
  }

  if(is.na(scale)){
    return(x)
  }

  if(!inverse){
    res <- round(x * scale, 3)
    if(verbose){
      cli::cli_alert_info("Escalamiento realizado (escala: {scale}). Rango del resultado: {paste(round(range(res), 6), collapse = ' ; ')}" )
    }
  }else{
    res <- x / scale
    if(verbose){
      cli::cli_alert_info("Revirtiendo escalamiento de costos")
    }
  }

  return(res)
}
