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
