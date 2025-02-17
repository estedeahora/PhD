#' @title PhD_homog_montecarlo
#' @description Genera dataframe con probabilidad de homogamia por grupo educativo

PhD_homog_montecarlo <- function(seed = 123, db, replace = F){

  set.seed(seed)

  # Se distribuyen aleatoriamente los valores de MNI3 dentro de cada radio
  db <- db |>
    dplyr::group_by(ID_RADIO) |>
    dplyr::mutate(s = sample(MNI3, size = dplyr::n(), replace = replace) )

  # Se calcula la homogamia por hogar
  db_HOG <-  db |>
    tidyr::pivot_wider(id_cols = c("ID_RADIO", "VIVIENDA_REF_ID", "HOGAR_REF_ID"),
                       names_from = "REL", values_from = "s" ) |>
    dplyr::mutate(HOM = ifelse(CON == PSH, T, F),
                  P1 = pmin(PSH, CON),
                  P2 = pmax(PSH, CON)) |>
    PhD::PhD_homog_hog(db_MAT, type = "P") |>
    dplyr::select(ID_RADIO, HOM, PRI_hogcount, SEC_hogcount, SUP_hogcount)

  db_RAD <- db_HOG |>
    group_by(ID_RADIO) |>
    summarise(
      # Porcentaje de hogares con homogamia, n_homog / n_conyg (sobre total de hogares conyugales)
      # phom = sum(HOM, na.rm = T) / sum(!is.na(HOM)),
      # Probabilidad de homogamia (por grupo educativo)
      across(.cols = tidyselect::ends_with("_hogcount"),
             .fns  = ~sum(.x * HOM, na.rm = T) / sum(.x, na.rm = T)),
      across(.cols  = tidyselect::ends_with("_hogcount"),
             .fns   = ~PhD_conv_phom(.x, conversion = "to_P2"))
    )

  return(db_RAD)
}
