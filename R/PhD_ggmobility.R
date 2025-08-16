
PhD_ggmobility <- function(variable, mp, db){

  db |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = !!sym(variable) ), colour = NA) +
    ggplot2::geom_sf(data = CARTO$DPTO, fill = NA, color = "grey40" ) +
    ggplot2::scale_fill_gradient2("% de tiempo\nen destino",
                                  low = "#b14324", high = "#1c7470",
                                  mid = "#f2e6bb", midpoint = mp) +
    ggplot2::coord_sf(xlim = AUX$lim_AMBA[c(1,3)],
             ylim = AUX$lim_AMBA[c(2,4)]) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom")
}
