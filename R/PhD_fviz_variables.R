
PhD_fviz_variables <- function(variable, MCA_obj, V, d, wrap_lim = 55){

  lim1 <- d$Dim.1 |> range()
  lim2 <- d$Dim.2 |> range()

  cat <- V$Categ[V$Variable == variable]
  tit <- V$tit[V$Variable == variable][1] |>
    stringr::str_wrap(wrap_lim)

  var_cat <- d |> dplyr::filter(Variable %in% cat)

  res <- MCA_obj |>
    factoextra::fviz_mca_var(geom = "point", title = tit,
                 alpha = 0, invisible = "quali.sup") +
    ggplot2::geom_point(ggplot2::aes(x = Dim.1, y = Dim.2), data = var_cat) +
    ggplot2::geom_path(ggplot2::aes(x = Dim.1, y = Dim.2), data = var_cat) +
    ggrepel::geom_text_repel(ggplot2::aes(x = Dim.1, y = Dim.2, label = lab),
                             data = var_cat, color = "tomato4") +
    xlim(lim1) +
    ylim(lim2)

  return(res)
}
