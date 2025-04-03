
#' PhD_geom_detail
#'
#'

PhD_geom_detail <- function(bb, color_cuad = "darkolivegreen2"){

  geom_sf(data = bb |> st_as_sfc(),
          color = color_cuad, fill = NA)

}

#' PhD_geom_detail_text

PhD_geom_detail_text <- function(bb,
                             lab_x = 1, lab_y = 4, vjust = -.3, hjust = 0,
                             label, color_lab = "cornsilk4" ){

  geom_text(x = bb[lab_x], y =  bb[lab_y], label = label,
            vjust = vjust, hjust = hjust, color = color_lab)

}

