
PhD_fviz_variables <- function(variable, MCA_obj, V, d, wrap_lim = 55){

  lim1 <- d$Dim.1 |> range()
  lim2 <- d$Dim.2 |> range()

  cat <- V$Categ[V$Variable == variable]
  tit <- V$tit[V$Variable == variable][1] |>
    str_wrap(wrap_lim)

  var_cat <- d |> filter(Variable %in% cat)

  res <- MCA_obj |>
    fviz_mca_var(geom = "point", title = tit,
                 alpha = 0, invisible = "quali.sup") +
    geom_point(aes(x = Dim.1, y = Dim.2), data = var_cat) +
    geom_path(aes(x = Dim.1, y = Dim.2), data = var_cat) +
    geom_text_repel(aes(x = Dim.1, y = Dim.2, label = lab),
                    data = var_cat, color = "tomato4") +
    xlim(lim1) +
    ylim(lim2)

  return(res)
}
