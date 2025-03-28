#' Calcula la segregación total, intra e inter grupos
#'
#' @param db Data.Frame. Contiene los datos de segregación. Debe tener las
#'            siguientes columnas:
#'            * id de las unidades (por defecto en la columna `ID_RADIO`);
#'            * nombre de los grupos segregados (por defecto en la columna `name`);
#'            * valor de la variable de interés (por defecto en la columna `value`).
#' @param entre Nombre de la variable que divide a las unidades en los grupos
#'                para los cuales se pretende analizar la segregación entre grupos.
#' @param est Character. Estimador de segregación a utilizar. Puede ser "M" (Mutual
#'                Information Index) o "H" (Multigroup Entropy Index or Theil's
#'                Index), valor por defecto "M".
#' @param se Logical. Indica si se desea calcular el error estándar de la
#'                estimación (por defecto `TRUE`).
#' @param unidad Character. Nombre de la variable que identifica a las unidades
#'                de análisis (por defecto `ID_RADIO`).
#' @param peso Character. Nombre de la variable que contiene el peso de cada
#'               grupo en cada unidad de análisis (por defecto `value`).
#' @param grupo Character. Nombre de la variable que contiene el nombre de los
#'              grupos sobre los que se realiza el análisis de segregación
#'              (por defecto `name`).
#'
#' @return vector numérico con tres valores, correspondientes a la segregación
#'        total, intra e inter grupos, respectivamente.
#'
#' @export

PhD_seg_between_within <- function(db, entre, est = "M", se = T,
                                   unidad = "ID_RADIO", peso = "value",
                                   grupo = "name") {

  if(!any(est %in% c("M", "H"))){
    stop("'index' must be one of 'M' or 'H'")
  }else{
    estimador <- c(M = 1, H = 2)[est]
  }

  res <- c(TOTAL = segregation::mutual_total(db, unit = unidad, se = se,
                                             group = grupo, weight = peso)$est[estimador],
           WITHIN = segregation::mutual_total(db, unit = unidad, se = se,
                                              group = grupo, weight = peso,
                                              within = entre)$est[1],
           BETWEEN = segregation::mutual_total(db, group = grupo, se = se,
                                               unit = entre, weight = peso)$est[estimador])

  return(res)
}
