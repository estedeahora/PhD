#' Calcula de forma conjunta diferentes indices de segregación
#'
#' @param db data.frame con una fila con cada unidad y que contenga una columna
#'                `ID_RADIO` con el ID de radios y las columnas `PRI`, `SEC` y
#'                `SUP` con el recuento de cada uno de estos grupos en cada unidad.
#' @param filtro Character. Se utiliza para filtrar la columna `ID_RADIO` de `db`.
#'                  Por defecto se utiliza `.` para no filtrar ningún caso.
#' @param filtro_NA Logical. ¿Deben filtrarse las unidades sin casos en los grupos?
#' @param verbose Logical. Indica si se desea mostrar información sobre el proceso.
#' @param bootstrap Integer. Número de bootstrap a realizar para el cálculo de
#'                 los índices de segregación. Por defecto 100.
#' @param se Logical. Indica si se desea calcular el error estándar de la
#'               estimación (por defecto `TRUE`).
#'
#' @description Se calculan los siguientes índices de segregación:
#'    * Índice de disimilitud entre primario y superior ($D_{pri;sup}$)
#'    * Índice de disimilitud multi-grupo ($D^{\*}$)
#'    * Índice de información mutua ($M$)
#'    * Índice de segregación primario ($IS_{pri}$)
#'    * Índice de segregación secundario ($IS_{sec}$)
#'    * Índice de segregación superior ($IS_{sup}$)
#'    * Índice de aislamiento normalizado primario ($ETA_{pri}^2$)
#'    * Índice de aislamiento normalizado secundario ($ETA_{sec}^2$)
#'    * Índice de aislamiento normalizado superior ($ETA_{sup}^2$)
#'    * Índice de exposición normalizado multi-grupo ($P^{\*}$)
#'
#' @return data.frame con dos columnas. La columna `Index` tiene el nombre
#' de cada índice (formateado en LaTeX) y la columna `Value` tiene los
#'  valores de los índices de segregación.
#'
#' @export

PhD_seg_summarise <- function(db, filtro = ".",
                        filtro_NA = TRUE, verbose = FALSE,
                        bootstrap = 100, se = T){

  db <- db |>
    # Filtra las unidades en función del `filro`
    dplyr::filter(stringr::str_starts(ID_RADIO, filtro)) |>
    # Retiene sólo las columnas con los recuentos de los grupos.
    dplyr::select(PRI:SUP)

  # Filtra unidades sin recuentos en los grupos.
  if(filtro_NA) {
    sel <- apply(db, 1, sum)
    db <- db[sel > 0 & !is.na(sel), ]
    if(verbose){
      cli::cli_alert_info("Se quitan casos sin casos en la unidad espacial")
    }
  }

  IND <- c("Dissimilarity Primary and Higher education ($D_{pri;hig}$)",
           "Multi-group dissimilarity ($D^{*}$)",
           "Mutual Information Index ($M$)",
           "Segregation: Primary ($IS_{pri}$)",
           "Segregation: Secondary ($IS_{sec}$)",
           "Segregation: Higher education ($IS_{sup}$)",
           # "Multi-group Gini Multi-group ($G^{*}$)",
           "Normalized Isolation: Primary ($ETA_{pri}^2$)",
           "Normalized Isolation: Secondary ($ETA_{sec}^2$)",
           "Normalized Isolation: Higher education ($ETA_{sup}^2$)",
           "Multi-group normalized exposure ($P^{*}$)")

  res <- data.frame(Index = IND,
                    Value = c(OasisR::DIDuncan(db)[1, 3],
                              OasisR::DMulti(db),
                              MMutual(db, index = "M", se = se,
                                      bootstrap = bootstrap),
                              OasisR::ISDuncan(db),
                              # GiniMulti(b),
                              OasisR::Eta2(db),
                              OasisR::PMulti(db)))

  n <- names(db)

  if(verbose){
    cat("Group names: ", paste(n, collapse = ", "), "\n")
  }

  attr(res, which = "group") <- n

  return(res)
}

#' Calcula Index M en base a data.frame donde cada columna es el recuento de
#' cada uno de los grupos.

MMutual <- function(db, total = T, index = c("M", "H", "all"),
                    se = T, seed = 321, bootstrap = 100){

  if(!any(index %in% c("M", "H", "all"))){
    stop("'index' must be one of 'M', 'H' or 'all'")
  }else{
    index <- index[1]
  }

  set.seed(seed)

  res <- db |>
    dplyr::mutate(id = 1:n()) |>
    tidyr::pivot_longer(cols = -id, values_to = "n") |>
    segregation::mutual_total(group = "name", unit = "id", weight = "n",
                              se = se, n_bootstrap = bootstrap)

  if(index == "all"){
    return(res)
  }else{
    res <- round(res$est[res$stat == index], 4)
    names(res) <- index

    return(res)
  }

}


