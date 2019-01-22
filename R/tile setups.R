#' @title tile coordinates setup
#' @name tile_coord_setup
#' @param row tile row position
#' @param col tile column position
#' @export
tile_coord_setup  <- function(row, col, row_unit = sqrt(3) / 2 * 100, col_unit = 1 *100) {
  top <- tibble::tibble(x = (col - 0.5) * row_unit, y = (row - 1) * col_unit )
  left2 <- tibble::tibble(x = (col - 1) * row_unit, y = (row - 0.75) * col_unit )
  right2 <- tibble::tibble(x = col * row_unit, y = (row - 0.75) * col_unit )
  left3 <- tibble::tibble(x = (col - 1) * row_unit, y = (row - 0.25) * col_unit )
  right3 <- tibble::tibble(x = col * row_unit, y = (row - 0.25) * col_unit )
  bot <- tibble::tibble(x = (col - 0.5) * row_unit, y = row * col_unit)

  coord_df <- dplyr::bind_rows(
    top, left2, right2, left3, right3, bot
  ) %>% dplyr::mutate(pts = c('top', 'left2', 'right2', 'left3', 'right3', 'bot'))

  if (row %%2 == 0) coord_df <- coord_df %>% dplyr::mutate(x = x + 0.5 * row_unit)
  if (row > 1) coord_df <- coord_df %>% dplyr::mutate(y = y - 0.25 * (row - 1) * col_unit)
  coord_df <- coord_df %>% dplyr::mutate(y = y * -1)

  top <- coord_df[1, ]
  left2 <- coord_df[2, ]
  right2 <- coord_df[3, ]
  left3 <- coord_df[4, ]
  right3 <- coord_df[5, ]
  bot <- coord_df[6, ]

  tibble::tibble(
    lines = seq(1, 6, 1),
    x1 = c(top$x, right2$x, right3$x, bot$x, left3$x, left2$x),
    y1 = c(top$y, right2$y, right3$y, bot$y, left3$y, left2$y),
    x2 = c(right2$x, right3$x, bot$x, left3$x, left2$x, top$x),
    y2 = c(right2$y, right3$y, bot$y, left3$y, left2$y, top$y),
  )
}

#' @title map tiles coordinates setup
#' @name map_coord_setup
#' @param map_df map basic data frame
#' @export
map_coord_setup  <- function(map_df, ...) {
   tmp_df <- map_df %>% dplyr::select(row, col, color_code) %>%
    purrrlyr::by_row(
      ~tile_coord_setup(.$row, .$col)
    ) %>% tidyr::unnest() %>%
     dplyr::mutate_at(
       dplyr::vars(x1, x2, y1, y2), ~round(., 5)
     )

   tmp_df %>% dplyr::mutate(
     bridge = chk_bridge(tmp_df)
   )
}

#' @title check for potential bridge connection
#' @name chk_bridge
#' @param line1 line one
#' @param line2 line two
#' @export
chk_bridge  <- function(map_df) {
   1:nrow(map_df) %>% purrr::map_dbl(
     function(x) {
       line_loc <- dplyr::bind_rows(
         map_df[x, ] %>% dplyr::select(x1, y1, x2, y2),
         map_df[x, ] %>% dplyr::select(x1 := x2, y1 := y2, x2 := x1, y2 := y1)
       ) %>% dplyr::inner_join(
         map_df %>% dplyr::select(x1, y1, x2, y2, color_code),
         by = c('x1', 'x2', 'y1', 'y2')
       ) %>% dplyr::pull(color_code)

       pt_loc1 <- map_df[x, ] %>% dplyr::select(x1, y1) %>% dplyr::inner_join(
         map_df %>% dplyr::select(x1, y1, color_code),
         by = c('x1', 'y1')
       ) %>% dplyr::pull(color_code)

       pt_loc2 <- map_df[x, ] %>% dplyr::select(x1 := x2, y1 := y2) %>% dplyr::inner_join(
         map_df %>% dplyr::select(x1, y1, color_code),
         by = c('x1', 'y1')
       ) %>% dplyr::pull(color_code)

       if (sum(magrittr::equals(line_loc, c(99, 99))) == 2 &
           sum(pt_loc1 != 99) >= 1 & sum(pt_loc2 != 99) >= 1
       ) return(0) else return(NA)
     })
}

#' @title check line equivalence
#' @name line_equal
#' @param line1 line one
#' @param line2 line two
#' @export
line_equal  <- function(line1, line2) {
   chk1 <- equals(line1, line2) %>% sum()
   chk2 <- equals(line1, c(line2[3:4], line2[1:2])) %>% sum()

   if (chk1 == 4 | chk2 == 4) return(TRUE) else return(FALSE)
}

#' @title  build map string
#' @name parse_map_str
#' @param map_str a string to set up tiles
#' @export
parse_map_str <- function(map_str) {
  map_str %>% strsplit(";") %>% .[[1]] %>% purrr::map(~strsplit(gsub("\\\n| ", "", .), ',')[[1]] )
}

#' @title create map table
#' @name create_map_table
#' @param map_str a string to set up tiles
#' @export
create_map_table <- function(map_str) {
  parsed_map_str <- map_str %>% parse_map_str()
  data(color_map)

  df <- 1:length(parsed_map_str) %>%
    purrr::map_df(function(x) {
      tibble::tibble(
        row = x,
        col = 1:length(parsed_map_str[[x]]),
        color_letter = parsed_map_str[[x]]
      )
    }) %>%
    dplyr::left_join(
      color_map
    ) %>%
    dplyr::mutate(
      building = 0
    )

  return(df)
}
