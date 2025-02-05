#' @title PhD_homog_montecarlo
#' @description Genera dataframe con probabilidad de homogamia por grupo educativo

PhD_homog_montecarlo <- function(seed = 123, db, replace = F){

  set.seed(seed)

  db <- db |>
    dplyr::group_by(ID_RADIO) |>
    dplyr::mutate(s = sample(MNI3, size = dplyr::n(), replace = replace) )

  db_MAT <-  db |>
    tidyr::pivot_wider(id_cols = c("ID_RADIO", "VIVIENDA_REF_ID", "HOGAR_REF_ID"),
                       names_from = "REL", values_from = "s" ) |>
    dplyr::mutate(SEG1 = ifelse(CON == PSH, T, F),
                  P1 = pmin(PSH, CON),
                  P2 = pmax(PSH, CON))

  db_MAT <- cbind(db_MAT, pivot_edu(db_MAT, type = "P")) |>
    dplyr::select(ID_RADIO, SEG1, PRI_p, SEC_p, SUP_p)

  db_RAD <- db_MAT |>
    group_by(ID_RADIO) |>
    summarise(
      # Porcentaje de hogares con homogamia, n_homog / n_conyg (sobre total de hogares conyugales)
      # phom = sum(SEG1, na.rm = T) / sum(!is.na(SEG1)),
      # Probabilidad de homogamia (por grupo educativo)
      across(.cols = etidyselectnds_with("_p"),
             .fns  = ~sum(.x * SEG1, na.rm = T) / sum(.x, na.rm = T)),
      across(.cols  = tidyselect::ends_with("_p"),
             .fns   = ~conv_phom(.x))
    )

  return(db_RAD)
}

# definir
# conv_phom()
