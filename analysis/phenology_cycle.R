# Load library
library(ggplot2)

# Parameters
SOS <- 80
EOS <- 220
MAT <- 100
SEN <- 200

NDVI_min <- 0.2
NDVI_max <- 0.8

t <- 1:365

# Functions
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

softplus <- function(x) {
  log1p(exp(x))
}

# Compute components
g1 <- softplus(MAT - SOS)
g2 <- softplus(EOS - SEN)

# Compute f(t)
f_t <- NDVI_min + (NDVI_max - NDVI_min) * (
  sigmoid(-2 * ((2 * SOS + g1 - 2 * t) / g1)) -
  sigmoid(-2 * ((2 * SEN + g2 - 2 * t) / g2))
)

# Create data frame
df <- data.frame(
  t = t,
  f_t = f_t
)

# Plot with ggplot2
ggplot(df, aes(x = t, y = f_t)) +
  geom_line(size = 1.2, color = "darkgreen") +
  labs(
    title = "NDVI Seasonal Curve",
    x = "Day of Year (t)",
    y = "f(t)"
  ) +
  theme_minimal() + 
  geom_vline(xintercept = c(SOS, MAT, SEN, EOS),
             linetype = "dashed", alpha = 0.5) + 
  annotate("rect", xmin = SOS, xmax = EOS,
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green")


