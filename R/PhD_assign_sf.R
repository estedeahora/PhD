#' @title Assign centroids to a database based on ID
#'
#' @description This function assigns the centroids of a spatial object to a
#'              database based on a specified ID.
#' @param db_ID A data frame containing the IDs to which centroids will be assigned.
#' @param id The name of the column in `db_ID` that contains the IDs to match with
#'              the spatial object.
#' @param sf_object A spatial object (an sf object) containing the geometries
#'              to be assigned.
#' @param id_sf The name of the column in `sf_object` that contains the IDs to
#'              match with `db_ID`. Defaults to "ID_RADIO".
#' @param geom_column The name of the geometry column in the `sf_object` object.
#'                   Defaults to the sf column attribute of `sf_object`.
#'
#' @return A spatial data frame with the centroids assigned to the specified IDs.
#'
#' @export

PhD_assign_sf <- function(db_ID, id, sf_object,
                                id_sf = "ID_RADIO",
                                geom_column = attr(sf_object, "sf_column") ){

  sf_object <- st_centroid(sf_object)
  by <- id_sf |> set_names(id)

  res <- left_join(db_ID[id], sf_object, by = by) |>
    st_as_sf(sf_column_name = geom_column)

  return(res)
}


