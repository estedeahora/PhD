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
    dplyr::mutate(TOTAL = PRI + SEC + SUP,
                  col = grDevices::rgb(red = PRI, green = SEC, blue = SUP, maxColorValue = 100)) |>
    dplyr::filter(TOTAL == 100) |>
    ggtern::ggtern(ggplot2::aes(x = PRI, y = SEC, z = SUP,
                       color = I(col))) +
    ggplot2::geom_point(alpha = .alpha) +
    ggtern::theme_ggtern() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill='transparent', color = NA))
}
