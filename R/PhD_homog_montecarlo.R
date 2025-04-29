#' @title PhD_homog_montecarlo
#' @description Genera dataframe con probabilidad de homogamia por grupo educativo

PhD_homog_montecarlo <- function(seed = 123, db, replace = F, p = NULL){

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
                  P2 = pmax(PSH, CON),
                  # EDU: MNI del hogar (sea conyugal o no)
                  EDU = pmax(PSH, CON, na.rm = T) ) |>
    PhD_homog_hog(type = "P") |>
    dplyr::select(ID_RADIO, HOM, tidyselect::ends_with("_hogcount"))

  db_RAD <- db_HOG |>
    dplyr::group_by(ID_RADIO) |>
    dplyr::summarise(
      # Probabilidad de homogamia (por grupo educativo): usa P1.
      dplyr::across(.cols = tidyselect::ends_with("_hogcount"),
                    .fns  = ~sum(.x * HOM, na.rm = T) / sum(.x, na.rm = T))#,
      # # dplyr::across(.cols  = tidyselect::ends_with("_hogcount"),
      # #               .fns   = ~PhD_conv_phom(.x, conversion = "to_P2"))
    ) |>
    # Renombrar columnas
    dplyr::rename_with(.cols = tidyselect::ends_with("_hogcount"),
                       .fn = ~stringr::str_replace(.,"_hogcount", "_p"))

  if(!is.null(p)){
    p()
  }

  return(db_RAD)
}
