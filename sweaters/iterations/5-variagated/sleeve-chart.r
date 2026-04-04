library('dplyr')
library('ggplot2')

# measured after IO already started the sweater 
# didn't actually swatch this big
cm_per_100_rows = 28.5
cm_per_100_st = 39.0

stAtUnderarm = 106

sleeveCmFromUnderarm = 45 # cm

stFromUnderarm = ceiling(
  sleeveLengthFromUnderarm / (cm_per_100_rows / 100)
)
# sleeve row counts where the decrease rate changes
pivotPoints = tibble(
  rowStart = c(1, 10, 60),
  decreaseRate =  c(1, 1/2, 1/4)
)

tibble(row = seq(1, stFromUnderarm)) %>%
  inner_join(
    pivotPoints,
    join_by(closest(row >= rowStart))
  ) %>%
  mutate(
    # total decreases executed so far
    decreases = floor(cumsum(decreaseRate)),
    # decreases per side - odd decreases go left, even right
    decreasesLeft = if_else( decreases%%2==1, (decreases-1)/2 + 1, decreases/2 ),
    decreasesRight = decreases - decreasesLeft,
    # total stitches around
    st = stAtUnderarm - decreases,
    # left from centreline
    stLeft = stAtUnderarm/2 - decreasesLeft,
    # right from centreline
    stRight = stAtUnderarm/2 - decreasesRight
  ) %>%
  ggplot( aes( x=row ) ) +
  geom_step(aes(y=-stRight)) +
  geom_step(aes(y=stLeft)) +
  xlab('Rows') +
  ylab('Stitches') +
  coord_fixed( ratio = cm_per_100_st / cm_per_100_rows ) +
  scale_x_continuous(minor_breaks = seq(0,stFromUnderarm,2)) +
  scale_y_continuous(minor_breaks = seq(-80,80,2))
