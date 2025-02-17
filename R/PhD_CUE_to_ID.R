#' @title CUE_ME2
#'
#' @description Enmascara CUE para coincidir con ID1 (ME 2019 en adelante)
#'
#' @param x CUEANEXO
#'

PhD_CUE_to_ID <- function(CUEANEXO){

  op <- options(scipen = 999)
  on.exit(options(op), add = T)

  k <- str_sub(CUEANEXO, start = 6, end = 7) |> as.numeric()

  sel <- as.numeric(CUEANEXO) <= 500001000 & k %in% 0:10

  clave <- ifelse(sel, k + 1, k + 4)

  k1 <- str_sub(CUEANEXO, 1, 5) |> as.numeric()
  k2 <- str_sub(CUEANEXO, 5, 9) |> as.numeric()

  x1 <- k1 * clave
  x2 <- k2 * clave

  ID1 <- ifelse(sel, paste0(x1, x2), paste0(x2, x1)) |>
    # Repetir tres veces resultado
    strrep(times = 3) |>
    # Cortar ID en lenght = 15 elementos
    str_sub(end = 15)

  return(ID1)
}
