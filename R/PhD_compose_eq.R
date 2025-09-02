
PhD_compose_eq <- function(ft, j = "Index"){
  ft |>
    flextable::compose(j = j,
                       value = flextable::as_paragraph(
                         stringr::str_split(Index, pattern = "\\$" ,
                                            simplify = T)[, 1],
                         flextable::as_equation(
                           stringr::str_split(Index, pattern = "\\$",
                                              simplify = T)[, 2],
                           width = 2, height = .5),
                         stringr::str_split(Index, pattern = "\\$" ,
                                            simplify = T)[, 3]))
}
