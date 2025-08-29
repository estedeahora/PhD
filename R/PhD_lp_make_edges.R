PhD_lp_make_edges <- function(cost, k){

  split(cost, seq(nrow(cost))) |>
    purrr::map(\(r) data.frame(j = order(r)[1:k] )) |>
    dplyr::bind_rows(.id = "i") |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.integer)) |>
    as.matrix()
}
