suppressPackageStartupMessages(library(tilegramsR))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))

data("color_map")

map_str <- "G,K,I,U,Y,S,K,S,Y,R,K,B,Y;
B,U,I,B,G,R,I,I,I,I,I,U;
S,G,R,I,I,U,I,K,S,U,Y,I,S;
I,I,I,S,I,I,G,R,B,G,R,I;
R,S,Y,I,B,R,I,U,Y,S,U,I,K;
K,U,I,G,Y,G,I,S,B,G,I,S;
Y,B,I,K,S,K,B,I,U,K,I,G,R;
G,I,U,R,U,Y,R,I,I,I,R,B;
K,I,I,G,B,S,B,I,G,Y,K,U,Y;"

map_df <- create_map_table(map_str) %>% map_coord_setup()

map_df  %>%
  dplyr::left_join(color_map %>% dplyr::select(color_letter, color_code)) %>%
  dplyr::group_by(row, col, color_code)%>%
  tidyr::nest() -> map_df_grp

map_df_lines <- map_df %>%
  dplyr::select(x1, y1, x2, y2, bridge) %>% dplyr::distinct()

1:nrow(map_df_lines) %>%
  purrr::map(function(x) {
      map_df_lines[x, 1:4] %>% t() %>% as.vector() -> tmp
      matrix(tmp[c(1, 3, 2, 4)], 2) %>% st_linestring()
  }) -> g_line

map_df_grp$data %>%  purrr::map(function(x) {
  tmp_d <- x %>% dplyr::select(x := x1, y := y1)
  list(dplyr::bind_rows(tmp_d, tmp_d[1, ]) %>% as.matrix()) %>% st_polygon()
})   -> g #%>% st_sfc()

sf_TM <- map_df_grp %>% dplyr::select(
               row, col, color_code)

sf_TM$g <-  g %>% st_sfc()
sf_TM <- sf_TM %>% st_as_sf()

sf_TM_line <- map_df_lines
sf_TM_line$g_line <- g_line %>% st_sfc()
sf_TM_line <- sf_TM_line %>% st_as_sf()

pal_fill <- colorFactor(
  palette = c('blue', 'green', 'gray', 'red', 'yellow', 'brown', 'black', 'white'),
  domain = map_df_grp$color_code %>% as.factor() %>% levels()
)


library(colormap)

# tilegrams are not geo-rerefenced so we need to use
# L.CRS.Simple projecion.
getLeafletOptions <- function(minZoom, maxZoom, ...) {
  leafletOptions(
    crs = leafletCRS("L.CRS.Simple"),
    minZoom = minZoom, maxZoom = maxZoom,
    dragging = FALSE, zoomControl = FALSE,
    tap = FALSE,
    attributionControl = FALSE , ...)
}
#
# # helper function to get colors based on a factor variable
# getFactorPal <- function(f) {
#   colorFactor(colormap::colormap(
#     colormap = colormap::colormaps$hsv,
#     nshades = length(f)), f)
# }

leaflet(
  #sf_NPR1to1,
  sf_TM,
  options= getLeafletOptions(-0.25, -0.25)) %>%
  addPolygons(
    weight=2, color='white',
    fillOpacity = 0.6, opacity = 1, fillColor= ~pal_fill(color_code),
    highlightOptions = highlightOptions(weight = 4))%>%
  addPolylines(weight = 2, color = 'black', data = sf_TM_line, dashArray = 2) %>%
  setMapWidgetStyle()

# %>%
#   addLabelOnlyMarkers(
#     data=sf_NPR1to1.centers,
#     label = ~as.character(state),
#     labelOptions = labelOptions(
#       noHide = 'T', textOnly = T,
#       offset=c(-4,-10), textsize = '12px'))

library('leaflet')

# Fake data
df <- data.frame(lng = c(-5, -10, -15, -20, 25),
                 lat = c(8, 12, 33, 4, 18),
                 size = c(200000, 100000, 800000, 250000, 350000),
                 popup = c('A', 'B', 'C', 'D', 'E'),
                 type = c('A', 'B', 'C', 'D', 'E'),
                 stringsAsFactors = FALSE)

# If you want to set your own colors manually:
pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange'),
  domain = df$type
)

leaflet(df) %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~size, popup = ~popup, color = ~pal_fill(type))
