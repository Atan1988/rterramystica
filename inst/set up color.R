color_map <- tibble::tibble(
  color_letter = c("B", "G", "U", "R", "Y", "S", "K", "I"),
  color_name = c('Blue', 'Green', 'Gray', "Red", 'Yellow', 'Brown', 'Black', 'River'),
  color_code = c(1, 2, 3, 4, 5, 6, 7, 99)
)

##list bonus tiles
scoring_tile <- tibble::tibble(
  scoring_tile_id = seq(1, 8, 1),
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
bounus_card <- tibble::tibble(
  bounus_card_id = seq(1, 9, 1),
  priest = c(1, rep(0, 8)),
  worker = c(0, 1, rep(0, 5), 1, 2),
  coin = c(rep(0, 2), 6, 0, 2, 4, 2, 0, 0),
  power = c(0, 3, 0, 3, rep(0, 5)),
  dig = c(rep(0, 4), 1, rep(0, 4)),
  ship = c(rep(0, 3), 1, rep(0, 5)),
  cult = c(rep(0, 5), 1, rep(0, 3)),
  dwelling_bonus = c(rep(0, 6), 1, rep(0, 2)),
  trading_house_bonus = c(rep(0, 7), 2, 0),
  stronghold_bonus = c(rep(0, 8), 4),
  sancuary_bonus = c(rep(0, 8), 4)
)

###facor_tile
favor_tiles <- tibble::tibble(
  favor_tiles_id = seq(1, 12, 1),
  cult_unit = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
  cult_color = c(rep('red', 3), rep('blue', 3), rep('brown', 3), rep('white', 3)),
  coin = c(3, rep(0, 11)),
  town6 = c(0, 1, rep(0, 10)),
  trading_house_bonus = c(rep(0, 3), 3, rep(0, 8)),
  cult = c(rep(0, 4), 1, rep(0, 7)),
  dwelling_bonus = c(rep(0, 6), 2, rep(0, 5)),
  power = c(rep(0, 7), 1, rep(0, 2), 4, 0),
  worker = c(rep(0, 7), 1, rep(0, 4)),
  trading_house_turn_bonus = c(rep(0, 9), 1, rep(0, 2)),
  number = 3
)

###
power_actions <-tibble::tibble(
   power_actions_id = seq(1, 6, 1),
   power_unit = c(rep(3, 2), rep(4, 3), 6),
   bridge = c(1, rep(0, 5)),
   priest = c(0, 1, rep(0, 4)),
   worker = c(rep(0, 2), 2, rep(0, 3)),
   coin = c(rep(0, 3), 7, rep(0, 2)),
   dig = c(rep(0, 4), 1, 2)
)
