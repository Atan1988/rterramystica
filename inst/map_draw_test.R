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

sf_TM <- set_sf_map_tile(map_df)
sf_TM_line <- set_sf_map_line(map_df)

pal_fill <- colorFactor(
  palette = c('blue', 'green', 'gray', 'red', 'yellow', 'brown', 'black', 'white'),
  domain = c(seq(1, 7, 1), 99) %>% as.character()
)

leaflet(
  #sf_NPR1to1,
  sf_TM,
  options= getLeafletOptions(-0.2, -0.2)) %>%
  addPolygons(
    weight=2, color='white',
    fillOpacity = 0.6, opacity = 1, fillColor= ~pal_fill(color_code),
    highlightOptions = highlightOptions(weight = 4))%>%
  addPolylines(weight = ~wgt, color = ~color, data = sf_TM_line, dashArray = ~dash_a) %>%
  setMapWidgetStyle()

# %>%
#   addLabelOnlyMarkers(
#     data=sf_NPR1to1.centers,
#     label = ~as.character(state),
#     labelOptions = labelOptions(
#       noHide = 'T', textOnly = T,
#       offset=c(-4,-10), textsize = '12px'))
