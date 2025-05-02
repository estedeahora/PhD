#' @title PhD_ggzoom
#' @description Funciones para mapear datos de la base de datos de la AMBA.
#'
#' @param gdb Base de datos espaciales (objeto de clase `sf`)
#' @param tit Título del gráfico.
#' @param cbox Bounding box (como es devuelto por `st_bbox`).
#' @param dpt Objeto de la clase `sf` con departamentos seleccionados

PhD_ggzoom <- function(gdb, tit, cbox = NULL, dpt = NULL,
                       variable = "ls", lim = NULL){

  if(is.null(dpt)){
    dpt <- gdb |> count(DPTO)
  }

  p <- gdb |>
    mutate(v = gdb[[variable]]) |>
    filter(!is.na(TH)) |>
    ggplot() +
    geom_sf(data = CARTO$AMGBA, alpha = 0.7) +
    labs(title = tit) +
    theme_void()

  if(variable == "color"){
    if(is.null(lim)){
      p <- p +
        geom_sf(aes(fill = I(v)), alpha = 0.6, color = NA)
    }else{
      p <- p +
        geom_sf(data = gdb |> mutate(v = gdb[[variable]]) |>
                  filter(!is.na(TH) & ls > lim),
                aes(fill = I(v)), color = NA)

    }
  }else{
    p <- p +
      geom_sf(aes(fill = v), alpha = 0.9, color = NA) +
      scale_fill_viridis_c(option = "B", limits = c(0, 1.1))
  }

  # Agregar límite de partidos
  p <-  p + geom_sf(data = dpt, fill = NA, color = "grey50")

  if(!is.null(cbox)){
    p <- p +
      coord_sf(xlim = cbox[c(1,3)],
               ylim = cbox[c(2,4)])
  }


  return(p)
}


# gg_zoom <- function(gdb, tit, cbox = NULL, dpt = NULL){
#
#   if(is.null(dpt)){
#     dpt <- gdb |> count(DPTO)
#   }
#
#   p <- gdb |>
#     filter(!is.na(TH)) |>
#     ggplot() +
#     geom_sf(data = CARTO$AMGBA, alpha = 0.7) +
#     geom_sf(aes(fill = ls), alpha = 0.9, color = NA) +
#     geom_sf(data = dpt, fill = NA, color = "grey50") +
#     scale_fill_viridis_c(option = "B", limits = c(0, 1.01)) +
#     labs(title = tit) +
#     theme_void()
#
#   if(!is.null(cbox)){
#     p <- p +
#       coord_sf(xlim = cbox[c(1,3)],
#                ylim = cbox[c(2,4)])
#   }
#
#   return(p)
# }
