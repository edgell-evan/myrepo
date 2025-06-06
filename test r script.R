library(tidyverse)
library(cowplot)
library(palmerpenguins)


ggplot(
  penguins,
  aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
geom_point(
  aes(
    color = species,
    shape = species
  )
) +
facet_wrap(~island) +
theme_cowplot()


