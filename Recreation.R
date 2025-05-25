library(tidyverse)
library(cowplot)

bodyCompData <- read.csv("./testing_write_to_csv_for_deer_mice.csv")

ggplot(
  bodyCompData,
  aes(native_alt, fat_percent, fill=sex)
  ) +
  geom_jitter() +
  facet_wrap(~generation) +
  labs(
    x = element_blank(),
    y = "Body Fat (%)",
    breaks = seq(0,70,10)
    ) +
  theme_cowplot()
