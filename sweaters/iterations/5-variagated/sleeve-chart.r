library('dplyr')
library('ggplot2')

# measured after IO already started the sweater 
# didn't actually swatch this big
cm_per_100_rows = 28.5
cm_per_100_st = 39.0

stAtUnderarm = 135

sleeveCmFromUnderarm = 55 # cm

cuffRows = 20 # no decreases here

stFromUnderarm = ceiling(
  sleeveCmFromUnderarm / (cm_per_100_rows / 100)
)
# sleeve row counts where the decrease rate changes
pivotPoints = tibble(
  rowStart =      c(1, 16,  55,  100, stFromUnderarm-cuffRows),
  decreaseRate =  c(1, 1/2, 1/3, 1/4, 0)
)

rows = tibble(row = seq(1, stFromUnderarm)) %>%
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
  )

rows %>%
  ggplot( aes( x=row ) ) +
  geom_step(aes(y=-stRight)) +
  geom_step(aes(y=stLeft)) +
  geom_vline(data=pivotPoints, aes(xintercept=rowStart), colour='pink' ) +
  geom_vline( aes(xintercept=101), colour='orange') + # current row highlight
  xlab('Rows') +
  ylab('Stitches') +
  coord_fixed( ratio = cm_per_100_st / cm_per_100_rows ) +
  scale_x_continuous(
    breaks = seq(0,stFromUnderarm,10),
    minor_breaks = seq(0,stFromUnderarm,2)
  ) +
  scale_y_continuous(
    breaks = seq(-80,80,10),
    minor_breaks = seq(-80,80,2)
  ) +
  labs(title='Sweeater sleeve decrease guide')

rows %>%
  summarize(
    st_at_wrist = min(st),
    cm_at_wrist = min(st) * cm_per_100_st / 100
  )

rows %>% print(n=200)
