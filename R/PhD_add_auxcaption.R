PhD_add_auxcaption <- function(tab, type = "notes", value){

  at <- attributes(tab)
  content_add <- paste0("\\\\", type, "{", value, "}")

  at$kable_meta$end_tabular <- paste(at$kable_meta$end_tabular,
                                     content_add, sep = "\n")

  content <- tab |>
    stringr::str_replace_all(pattern = "(\\\\end\\{table\\})",
                             replacement = paste(content_add,
                                                 "\\1", sep = "\n") ) |>
    magrittr::set_attributes(at)

  return(content)
}
