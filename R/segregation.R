# MMutual: Calcula Index M
MMutual <- function(b, total = T, index = c("M", "H", "all"),
                    se = T, seed = 321, bootstrap = 100){
  if(!any(index %in% c("M", "H", "all"))){
    stop("'index' must be one of 'M', 'H' or 'all'")
  }else{
    index <- index[1]
  }
  set.seed(seed)
  res <- b |>
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

# SEG_summary: Calcula diferentes indices de segregación
SEG_summary <- function(b, filtro0 = T, bootstrap = 100, verbose = F, se = T){
  if(filtro0) {
    a <- apply(b, 1, sum)
    b <- b[a > 0 & !is.na(a), ]
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
                    Value = c(OasisR::DIDuncan(b)[1, 3],
                              OasisR::DMulti(b),
                              OasisR::MMutual(b, index = "M", se = se,
                                      bootstrap = bootstrap),
                              OasisR::ISDuncan(b),
                              # GiniMulti(b),
                              OasisR::Eta2(b),
                              OasisR::PMulti(b)))

  n <- names(b)
  if(verbose){
    cat("Group names: ", paste(n, collapse = ", "), "\n")
  }
  attr(res, which = "group") <- n
  return(res)
}

# seg_tab: Calcula SEG_summary utilizando un filtro
SEG_tab <- function(b = db_aux, filtro = "."){
  b |>
    dplyr::filter(stringr::str_starts(ID_RADIO, filtro)) |>
    dplyr::select(PRI:SUP)|>
    SEG_summary(se = T)
}

# pivot_edu(): Genera dataframe con cantidad de personas/hogares  -------------------------------

pivot_edu <-  function(db, type = "P"){

  # a <- unique(db$EDU) |> sort()
  a <- 1:3

  # if(length(a) == 3){
  names(a) <- paste0(c("PRI", "SEC", "SUP"), "_p")
  # }else{
  # names(a) <- paste0(c("PRI", "SEC", "SNU", "UNI"), "_p")
  # }

  if(type == "H"){
    # Probabilidad del hogar
    # P(x) DE QUE DADO QUE EN UN HOGAR HAYA UNA PERSONA DE MNI_X, HAYA 2 PERSONAS DE MNI_X
    res <- map(a, \(x) ifelse(db$P1 == x | db$P2 == x, T, F)) |>
      as.data.frame()

  }else if(type == "P") {
    # Probabilidad individual
    # P(x) DE QUE DADA UNA PERSONA DE MNI_X, TENGA UNA PAREJA DE IGUAL MNI
    res <- map(a, \(x) (db$P1 == x) + (db$P2 == x) ) |>
      as.data.frame()
  }else{
    stop("'type' inválido. Debe ser 'H' o 'P'.")
  }
  return(res)
}

# conv_phom(): Transforma entre P1(x) y P2(x) ----------------------------------

conv_phom <- function(P1){

  P2 <- P1 / ( 2 - P1 )
  return(P2)

  # Inversa:
  # P1 <- 2 * P2/ (P2 + 1)
}

# gg_zoom: mapeo con zoom ------------------------------------------------------

gg_zoom <- function(gdb, tit, cbox = NULL, dpt = NULL){

  if(is.null(dpt)){
    dpt <- gdb |> count(coddpto)
  }

  p <- gdb |>
    filter(!is.na(TH)) |>
    ggplot() +
    geom_sf(data = CARTO$AMGBA, alpha = 0.7) +
    geom_sf(aes(fill = ls), alpha = 0.9, color = NA) +
    geom_sf(data = dpt, fill = NA, color = "grey50") +
    scale_fill_viridis_c(option = "B", limits = c(0, 1.01)) +
    labs(title = tit) +
    theme_void()

  if(!is.null(cbox)){
    p <- p +
      coord_sf(xlim = cbox[c(1,3)],
               ylim = cbox[c(2,4)])
  }

  return(p)
}

# geom_detail: recuadro/texto de recuadro con detalle --------------------------

geom_detail <- function(bb, color_cuad = "darkolivegreen2"){

  geom_sf(data = bb |> st_as_sfc(),
          color = color_cuad, fill = NA)

}

geom_detail_text <- function(bb,
                             lab_x = 1, lab_y = 4, vjust = -.3, hjust = 0,
                             label, color_lab = "cornsilk4" ){

  geom_text(x = bb[lab_x], y =  bb[lab_y], label = label,
            vjust = vjust, hjust = hjust, color = color_lab)

}

# HOM_summarise: Summarise para homogamia --------------------------------------

HOM_summarise <- function(.data, group_var){

  .data |>
    filter(!is.na(TH)) |>
    group_by(E = {{group_var}}) |>
    summarise(HOG = n(),
              HOG_MAT = sum(!is.na(SEG1), na.rm = T),
              MAT_p = HOG_MAT / HOG * 100,
              POR_h = sum(SEG1, na.rm = T) / HOG_MAT * 100,
              POR_h_PRI = sum(SEG1 & (P1 == 1), na.rm = T) / HOG_MAT * 100,
              POR_h_SEC = sum(SEG1 & (P1 == 2), na.rm = T) / HOG_MAT * 100,
              POR_h_SUP = sum(SEG1 & (P1 == 3), na.rm = T) / HOG_MAT * 100,
              across(.cols = PRI_p:SUP_p,
                     .fns  = ~sum(.x * SEG1, na.rm = T) / sum(.x, na.rm = T)),
              # across(.cols  = PRI_p:SUP_p,
              #        .fns   = ~conv_phom(.x)),
              # DIS_ED = mean(DIS, na.rm = T)
    ) |>
    select(-HOG_MAT)
}


# HOM_montecarlo: --------------------------------------------------------------

HOM_montecarlo <- function(x = 123, db, replace = F){

  set.seed(x)

  db <- db |>
    group_by(ID_RADIO) |>
    mutate(s = sample(MNI3, size = n(), replace = replace) )

  db_MAT <-  db |>
    pivot_wider(id_cols = c("ID_RADIO", "VIVIENDA_REF_ID", "HOGAR_REF_ID"),
                names_from = "REL", values_from = "s" ) |>
    mutate(SEG1 = ifelse(CON == PSH, T, F),
           P1 = pmin(PSH, CON),
           P2 = pmax(PSH, CON))

  db_MAT <- cbind(db_MAT, pivot_edu(db_MAT, type = "P")) |>
    select(ID_RADIO, SEG1, PRI_p, SEC_p, SUP_p)

  db_RAD <- db_MAT |>
    group_by(ID_RADIO) |>
    summarise(
      # Porcentaje de hogares con homogamia, n_homog / n_conyg (sobre total de hogares conyugales)
      # phom = sum(SEG1, na.rm = T) / sum(!is.na(SEG1)),
      # Probabilidad de homogamia (por grupo educativo)
      across(.cols = ends_with("_p"),
             .fns  = ~sum(.x * SEG1, na.rm = T) / sum(.x, na.rm = T)),
      across(.cols  = ends_with("_p"),
             .fns   = ~conv_phom(.x))
    )

  return(db_RAD)
}


