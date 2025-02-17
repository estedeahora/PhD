#' @description Modifica dataframe con educación de miembros del hogar
#'    para agregar columnas de recuento necesarias para el cálculo de
#'    la homogamia educativa del hogar.
#'
#' @param db data frame que incluya las variables `EDU`, `P1` y `P2`, con
#'            el nivel educativo del hogar y el nivel educativo de la(s) personas
#'            cabeceras del hogar (PSH + conyuge). El nivel educativo debe ser
#'            una variable numérico y contener 3 o 4 niveles educativos, de los
#'            cuales los dos primeros son interpretados como `PRI` (1) y `SEC` (2)
#'            y el tercero será `SUP` (3) si se trata de 3 niveles y `SNU` (3) y
#'            `UNI` (4) si se trata de 4 niveles.
#' @param type Tipo de probabilidad a calcular. Puede ser "P" para
#'              individual o "H" para hogar (ver `details`).
#'
#' @details
#' Si se selecciona `tipe= "H"` se devuelve, para cada nivel educativo de los
#' hogares conyugales, un valor TRUE si hay al menos una persona en el hogar
#' con ese nivel educativo. Si se selecciona `type = "P"` se devuelve, para cada
#' nivel educativo de los hogares conyugales, un valor numérico que indica la
#' cantidad de personas en el hogar con ese nivel educativo. Los hogares
#' no-conyugales tendrán `NA` en estas columnas. Estas probabilidades se utilizan
#' luego para calcular la probabilidad de homogamia educativa.
#'
#' @return el dataframe original con las nuevas columnas agregadas. Los nombres
#' de las columnas serán el `nivel_educativo` + `_p` (ejemplo: ).
#'
#' @export

# ex: pivot_edu
PhD_homog_hog <-  function(db, type = "P"){

  db_vars <- names(db)

  if(!"data.frame" %in% class(db)){
    cli::cli_abort("`db` debe ser un dataframe.")
  }

  if(!all(c("EDU", "P1", "P2") %in% db_vars)){
    cli::cli_abort("Las variables `EDU`, `P1` y `P2` deben estar presents en el dataframe.")
  }

  edu_levels <- unique(db$EDU) |> sort()

  if(!is.numeric(edu_levels)){
    cli::cli_abort("EDU debe ser una variable numérica.")
  }

  if(length(edu_levels) == 3){
    names(edu_levels) <- paste0(c("PRI", "SEC", "SUP"), "_hogcount")
  }else if(length(edu_levels) == 4){
    names(edu_levels) <- paste0(c("PRI", "SEC", "SNU", "UNI"), "_hogcount")
  }else{
    cli::cli_abort("Número de niveles educativos no soportado. `EDU` debe tener 3 o 4 valores.")
  }

  if(type == "H"){
    # Identificación de existencia de personas con cada nivel educativo
    db_p <- purrr::map(edu_levels, \(level) db$P1 == level | db$P2 == level) |>
      as.data.frame()
  }else if(type == "P") {
    # Conteo de personas en el hogar por cada nivel educativo
    db_p <- purrr::map(edu_levels, \(level) (db$P1 == level) + (db$P2 == level) ) |>
      as.data.frame()
  }else{
    cli::cli_abort("`type` inválido. Debe ser 'H' o 'P'.")
  }

  res <- dplyr::bind_cols(db, db_p)
  attr(res, "homog_type") <- type

  return(res)
}
