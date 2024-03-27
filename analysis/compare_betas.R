library(ggplot2)
library(dplyr)
library(cowplot)

dx <- seq(-0.5, 1, by = 0.01)
logrr <- log(1 + dx)

df <- tibble(
  dx = dx,
  logrr = logrr)

gg1 <- df |> 
  ggplot(aes(dx, logrr)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  theme_classic() + 
  labs(x = expression(paste(Delta, "x/x")),
       y = expression(paste("log((x+", Delta, "x)/x)")))

gg2 <- df |> 
  ggplot(aes(dx, logrr)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  theme_classic() + 
  labs(x = expression(paste(Delta, "x/x")),
       y = expression(paste("log((x+", Delta, "x)/x)"))) +
  xlim(-0.2, 0.2)

plot_grid(gg1, gg2, ncol = 2)
       