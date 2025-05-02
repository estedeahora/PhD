#' @title PhD_conv_phom
#' @description Permite convertir entre la medida de probabilidad de homogamia
#'  individual (`P1(x)`) y probabilidad de homogamia del hogar (`P2(x)`).
#'
#' @param P Probabilidad de homogamia individual (`P1(x)`) o del hogar (`P2(x)`).
#' @param conversion Tipo de conversión a realizar. Puede ser "to_P1" para
#'                  convertir de homogamia del hogar a homogamia individual
#'                  o "to_P2" (default) para convertir de homogamia individual
#'                  a homogamia del hogar.
#'
#' @details
#' La función permite convertir entre la probabilidad de homogamia individual
#' (`P1(x)`, en el anexo \eqn{P^\prime(H_a|a)}) y la probabilidad de homogamia
#' del hogar (`P2(x)`, en el anexo \eqn{P^{\prime\prime}(H_A|A)}). La conversión
#' se realiza de la siguiente manera:
#'
#' \eqn{P1 = \frac{2 . P2}{1 + P2}}
#'
#' \eqn{P2 = \frac{P1}{2 - P1}}
#'
#' Ver *Anexo C* para más detalles.
#'
#' @return Vector numérico con la probabilidad de homogamia individual (`P1(x)`)
#' o del hogar (`P1(x)`).
#'
#' @export

PhD_conv_phom <- function(P, conversion = "to_P2"){

  if(conversion == "to_P2"){
    P_res <- P / ( 2 - P )
  }else if(conversion == "to_P1"){
    P_res <- 2 * P/ (1 + P)
  }else{
    cli::cli_abort("El atributo `conversion` debe ser 'to_P1' o 'to_P2'.")
  }

  return(P_res)
}
