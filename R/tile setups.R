#' @title tile coordinates setup
#' @name tile_coord_setup
#' @param row tile row position
#' @param col tile column position
#' @export
tile_coord_setup  <- function(row, col) {
  top <- tibble::tibble(x = col - 0.5, y = row - 1)
  left2 <- tibble::tibble(x = col - 1, y = row - 0.75)
  right2 <- tibble::tibble(x = col, y = row - 0.75)
  left3 <- tibble::tibble(x = col - 1, y = row - 0.25)
  right3 <- tibble::tibble(x = col, y = row - 0.25)
  bot <- tibble::tibble(x = col - 0.5, y = row)

  coord_df <- dplyr::bind_rows(
    top, left2, right2, left3, right3, bot
  ) %>% dplyr::mutate(pts = c('top', 'left2', 'right2', 'left3', 'right3', 'bot'))

  if (row %%2 == 0) coord_df <- coord_df %>% dplyr::mutate(x = x + 0.5)

  tibble::tibble(
    lines = seq(1, 6, 1),
    x1 = c(top$x, left2$x, right2$x, left3$x, right3$x, bot$x),
    y1 = c(top$y, left2$y, right2$y, left3$y, right3$y, bot$y),
    x2 = c(left2$x, right2$x, left3$x, right3$x, bot$x, top$x),
    y2 = c(left2$y, right2$y, left3$y, right3$y, bot$y, top$y),
  )
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

  1:length(parsed_map_str) %>%
    purrr::map()
}
