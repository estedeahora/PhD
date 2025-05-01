#' @title PhD_conv_phom
#' @description Permite convertir entre homogamia del hogar (`P1(x)`) y
#' homogamia individual (`P2(x)`).
#'
#' @param P Probabilidad de homogamia del hogar (`P1(x)`) o individual (`P2(x)`).
#' @param conversion Tipo de conversión a realizar. Puede ser "to_P2" (default)
#'                  para convertir de homogamia del hogar a homogamia individual
#'                  o "to_P1" para convertir de homogamia individual a homogamia
#'                  del hogar.
#'
#' @details
#' La función permite convertir entre la probabilidad de homogamia del hogar
#' (`P1(x)`, en el anexo \eqn{P(H_A|A)}) y la probabilidad de homogamia individual
#' (`P2(x)`, en el anexo \eqn{P^\prime(H_a|a)}). La conversión se realiza de la siguiente manera:
#'
#' \eqn{P2 = \frac{P1}{2 - P1}}
#'
#' \eqn{P1 = \frac{2 . P2}{P2 + 1}}
#'
#' @return Probabilidad de homogamia del hogar (`P1(x)`) o individual (`P2(x)`).
#'
#' @export

PhD_conv_phom <- function(P, conversion = "to_P2"){

  if(conversion == "to_P2"){
    P_res <- P / ( 2 - P )
  }else if(conversion == "to_P1"){
    P_res <- 2 * P/ (P + 1)
  }else{
    cli::cli_abort("El atributo `conversion` debe ser 'toP1' o 'toP2'.")
  }

  return(P_res)
}

# P1 -> P2
# P2 = P1 / (2 - P1)
#
# P2 -> P1 (Inversa)
# P1 = 2 * P2/ (P2 + 1)
#
# conv_phom <- function(P1){
#
#   P2 <- P1 / ( 2 - P1 )
#   return(P2)
#
#   # Inversa:
#   # P1 <- 2 * P2/ (P2 + 1)
# }
