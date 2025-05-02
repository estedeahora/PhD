#' @title PhD_ggtern_legend
#' @description Function to create a ternary legend for PhD thesis
#' @param step Numeric value for the step size of the sequence. Must be a
#'              divisor of 100 (Default = 1).
#' @param alpha Numeric value for the transparency of the points (Default = 0.1).
#'
#' @return A ggtern plot with a ternary legend
#'
#' @export

PhD_ggtern_legend <- function(.step = 1, .alpha = 0.1){

  sequence <- seq(0, 100, by = .step)

  if(!is.element(100, sequence)){
    cli::cli_abort("`.step` must be a divisor of 100")
  }

  expand.grid(PRI = sequence,
              SEC = sequence,
              SUP = sequence) |>
    mutate(TOTAL = PRI + SEC + SUP,
           col = rgb(red = PRI, green = SEC, blue = SUP, maxColorValue = 100)) |>
    filter(TOTAL == 100) |>
    ggtern::ggtern(aes(x = PRI, y = SEC, z = SUP,
                       color = I(col))) +
    geom_point(alpha = .alpha) +
    ggtern::theme_ggtern() +
    theme(plot.background = element_rect(fill='transparent', color = NA))
}
