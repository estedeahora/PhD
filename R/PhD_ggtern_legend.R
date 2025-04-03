#' @description Function to create a ternary legend for PhD thesis
#' @param step Numeric value for the step size in the grid
#' @param alpha Numeric value for the transparency of the points
#'
#' @return A ggtern plot with a ternary legend
#'
#' @export

PhD_ggtern_legend <- function(step = 1, alpha = 0.1){

  sequence <- c(seq(0, 100, by = step), 100) |> unique()
  expand.grid(PRI = sequence,
              SEC = sequence,
              SUP = sequence) |>
    mutate(TOTAL = PRI + SEC + SUP,
           col = rgb(red = PRI, green = SEC, blue = SUP, maxColorValue = 100)) |>
    filter(TOTAL == 100) |>
    ggtern::ggtern(aes(x = PRI, y = SEC, z = SUP,
                       color = I(col))) +
    geom_point(alpha = alpha) +
    theme(plot.background = element_rect(fill='transparent', color = NA))
}
