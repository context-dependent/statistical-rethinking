library(tidyverse)

d <- mpg

d |>
    select(cyl, displ, hwy) |>
    filter(cyl == 4) |>
    arrange(desc(hwy)) |>
    head(10)

d |>
    select(cyl, displ, hwy) |>
    filter(cyl == 4) |>
    arrange(desc(hwy)) |>
    head(10)
