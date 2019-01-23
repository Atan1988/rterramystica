color_map <- tibble::tibble(
  color_letter = c("B", "G", "U", "R", "Y", "S", "K", "I"),
  color_name = c('Blue', 'Green', 'Gray', "Red", 'Yellow', 'Brown', 'Black', 'River'),
  color_code = c(1, 2, 3, 4, 5, 6, 7, 99)
)

##list bonus tiles
scoring_tile <- tibble::tibble(
  trading_house = c(1, 1, rep(0, 6)),
  stronghold = c(0, 0, 1, 1, rep(0, 4)),
  sancuary = c(0, 0, 1, 1, rep(0, 4)),
  town = c(rep(0, 4), 1, rep(0, 3)),
  dig = c(rep(0, 5), 1, rep(0, 2)),
  dwelling = c(rep(0, 6), rep(1, 2)),
  bouns_pt = c(rep(3, 2), rep(5, 3), rep(2, 3)),
  cult_color = c('blue', 'white', 'red', 'white', rep('brown', 2), 'blue', 'red'),
  cult_unit = c(rep(4, 2), rep(2, 2), 4, 1, 4, 4),
  bounus_type = c(rep('dig', 2), rep('worker', 2), 'dig', 'gold', 'priest', 'power'),
  bounus_unit = c(rep(1, 7), 4)
)

##
bouns_card <- tibble::tibble(

)
