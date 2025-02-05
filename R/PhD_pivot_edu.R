#' @title
#' @description Genera dataframe con cantidad de hogares
#'
#' @param db data frame que incluya las variables `EDU`, `P1` y `P2`, con
#'            el nivel educativo del hogar y el nivel educativo de la(s) personas
#'            cabeceras del hogar (PSH + conyuge). El nivel educativo debe ser
#'            una variable numérico y contener 3 o 4 niveles educativos, de los
#'            cuales los dos primeros son interpretados como `PRI` y `SEC` y
#'            el tercero será `SUP` si se trata de 3 niveles y `SNU` y `UNI`
#'            si se trata de 4 niveles.
#' @param type Tipo de probabilidad a calcular. Puede ser "P" para
#'              individual o "H" para hogar (ver `details`).
#'
#' @details
#' Si se selecciona `tipe= "H"`, se devuelve  devuelve un viene dado como la probabilidad
#' $P(x)$ de que dado en en un hogar haya una persona de nivel educativo $x$,
#' haya entonces otra persona con el mismo nivel educativo. La probabilidad
#' individual ("P") es la probabilidad $P(X)$ de que dado un hogar con una persona de
#'
#'        # Probabilidad individual:
#   P(x) DE QUE DADA UNA PERSONA DE MNI_X, TENGA UNA PAREJA
#   DE IGUAL MNI

#'
#' @return el dataframe original con las nuevas columnas agregada
#'
#'
#' @export

PhD_pivot_edu <-  function(db, type = "P"){

  db_vars <- names(db)

  if(class(db) != "data.frame"){
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
    names(edu_levels) <- paste0(c("PRI", "SEC", "SUP"), "_p")
  }else if(length(edu_levels) == 4){
    names(edu_levels) <- paste0(c("PRI", "SEC", "SNU", "UNI"), "_p")
  }else{
    cli::cli_abort("Número de niveles educativos no soportado. `EDU` debe tener 3 o 4 valores.")
  }

  if(type == "H"){
    db_p <- purrr::map(edu_levels, \(level) ifelse(db$P1 == level | db$P2 == level, T, F)) |>
      as.data.frame()
  }else if(type == "P") {
    db_p <- purrr::map(edu_levels, \(level) (db$P1 == level) + (db$P2 == level) ) |>
      as.data.frame()
  }else{
    cli::cli_abort("`type` inválido. Debe ser 'H' o 'P'.")
  }

  res <- dplyr::bind_cols(db, db_p)

  return(res)
}
