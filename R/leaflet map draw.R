#' @title set up map tiles for sf
#' @name set_sf_map_tile
#' @param map_df map data frame
#' @export
set_sf_map_tile  <- function(map_df) {
  map_df  %>%
    dplyr::left_join(color_map %>% dplyr::select(color_letter, color_code)) %>%
    dplyr::group_by(row, col, color_code)%>%
    tidyr::nest() -> map_df_grp

  map_df_grp$data %>%  purrr::map(function(x) {
    tmp_d <- x %>% dplyr::select(x := x1, y := y1)
    list(dplyr::bind_rows(tmp_d, tmp_d[1, ]) %>% as.matrix()) %>% st_polygon()
  })   -> g #%>% st_sfc()

  sf_TM <- map_df_grp %>% dplyr::select(
    row, col, color_code)

  sf_TM$g <-  g %>% st_sfc()
  sf_TM <- sf_TM %>% st_as_sf()
  return(sf_TM)
}

#' @title set up map lines for sf
#' @name set_sf_map_line
#' @param map_df map data frame
#' @export
set_sf_map_line  <- function(map_df) {
  map_df_lines <- map_df %>%
    dplyr::select(x1, y1, x2, y2, bridge) %>% dplyr::distinct()

  1:nrow(map_df_lines) %>%
    purrr::map(function(x) {
      map_df_lines[x, 1:4] %>% t() %>% as.vector() -> tmp
      matrix(tmp[c(1, 3, 2, 4)], 2) %>% st_linestring()
    }) -> g_line

  sf_TM_line <- map_df_lines
  sf_TM_line$g_line <- g_line %>% st_sfc()
  sf_TM_line <- sf_TM_line %>% st_as_sf()

  sf_TM_line <- sf_TM_line %>%
    dplyr::mutate(
      color = dplyr::case_when(
        bridge == 0 ~ "#8B4500",
        bridge == 1 ~ 'blue',
        bridge == 2 ~ 'green',
        bridge == 3 ~ 'gray',
        bridge == 4 ~ 'red',
        bridge == 5 ~ 'yellow',
        bridge == 6 ~ 'brown',
        bridge == 7 ~ 'black',
        TRUE ~ 'black'
      ),
      wgt = dplyr::case_when(
        !is.na(bridge) & bridge > 0 ~ 3,
        bridge == 0 ~ 1,
        TRUE ~ 2
      ),
      dash_a = dplyr::case_when(
        bridge == 0 ~ 2,
        TRUE ~ 1
      )
    )

  return(sf_TM_line)
}

#' @title set up leaflet options
#' @name getLeafletOptions
#' @param minZoom minimum zoom
#' @param maxZoom maximum zoom
#' @param ... additional parameters from leaflet
#' @export
getLeafletOptions <- function(minZoom, maxZoom, ...) {
  leaflet::leafletOptions(
    crs = leaflet::leafletCRS("L.CRS.Simple"),
    minZoom = minZoom, maxZoom = maxZoom,
    dragging = FALSE, zoomControl = FALSE,
    tap = FALSE,
    attributionControl = FALSE , ...)
}
