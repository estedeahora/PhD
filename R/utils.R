#' @title gg_zoom
#' @description Funciones para mapear datos de la base de datos de la AMBA.
#'
#' @param gdb Base de datos espaciales (objeto de clasesf)
#' @param


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

#' @title
#'
#'

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

