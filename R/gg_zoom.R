#' @title gg_zoom
#' @description Funciones para mapear datos de la base de datos de la AMBA.
#'
#' @param gdb Base de datos espaciales (objeto de clasesf)
#' @param

gg_zoom <- function(gdb, tit, cbox = NULL, dpt = NULL){

  if(is.null(dpt)){
    dpt <- gdb |> count(DPTO)
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
