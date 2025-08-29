#' Genera tablas con datos resumidos de homogamia según variable de control
#'
#'
# ex HOM_summarise
PhD_homog_summarise <- function(.data, group_var){

  .data |>
    dplyr::filter(!is.na(TH)) |>
    dplyr::group_by(E = {{group_var}}) |>
    dplyr::summarise(# Total de hogares
      HOG = n(),
      # Total de hogares conyugales
      HOG_CON = sum(!is.na(HOM)),
      # Porcentaje de hogares conyugales
      CON_p = HOG_CON / HOG * 100,
      # Porcentaje de hogares con homogamia (sobre total de hogares conyugales)
      POR_h = sum(HOM, na.rm = T) / HOG_CON * 100,
      # Porcentaje de hogares con homogamia por grupo educativo (sobre total de hogares conyugales)
      POR_h_PRI = sum(HOM & (P1 == 1), na.rm = T) / HOG_CON * 100,
      POR_h_SEC = sum(HOM & (P1 == 2), na.rm = T) / HOG_CON * 100,
      POR_h_SUP = sum(HOM & (P1 == 3), na.rm = T) / HOG_CON * 100,
      # Probabilidad de homogamia (por grupo educativo), usa P1
      dplyr::across(.cols = PRI_hogcount:SUP_hogcount,
             .fns  = ~sum(.x * HOM, na.rm = T) / sum(.x, na.rm = T)),
      # across(.cols  = PRI_p:SUP_p,
      #        .fns   = ~conv_phom(.x)),
      # DIS_ED = mean(DIS, na.rm = T)
    ) |>
    dplyr::rename_with(~str_replace(.x, "_hogcount", "_p1"), ends_with("_hogcount")) |>
    dplyr::select(-HOG_CON)
}
