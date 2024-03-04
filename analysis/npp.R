library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

## Data ------------------
# get data from geco server: data/archive/npp_peng_NA
# here placed in a mirrored local directory ~/data/archive/npp_peng_NA

df <- read_csv("~/data/archive/npp_peng_NA/data/NPP_Nmin_dataset_with_predictors.csv")
df2 <- read_csv("~/data/CNuptake_MS/data/NPP_Yunke/NPP_dataset.csv")

## BPE -------------------
# visdat::vis_miss(
#   df |> 
#     select(lon, lat, site, GPP, TNPP_1)
#   )

df_plot1 <- df |> 
  filter(GPP > TNPP_1, GPP > 0) |> 
  mutate(cue = TNPP_1 / GPP)

gg1 <- ggplot(aes(GPP, TNPP_1), data = df_plot1) +
  geom_point(alpha = 0.4) +
  labs(x = expression(paste("GPP (gC m"^-2, "yr"^-1, ")")),
       y = expression(paste("BP (gC m"^-2, "yr"^-1, ")"))) +
  theme_classic()

gg2 <- ggplot(aes(cue, after_stat(count)), data = df_plot1) +
  geom_histogram(fill = "grey70", color = "black") +
  geom_vline(xintercept = mean(df_plot1$cue), color = "red") +
  labs(x = "BPE", y = "Count") +
  theme_classic() +
  annotate("text", 0.55, 27, label = paste("mean:", format(mean(df_plot1$cue), digits = 2)), color = "red")

plot_grid(gg1, gg2, labels = c("a", "b"))

ggsave(here::here("book/images/npp_bpe.png"), width = 6, height = 3)

## Allocation ------------------
# visdat::vis_miss(
#   df |>
#     select(lon, lat, site, TNPP_1, NPP.wood, NPP.coarse)
#   )

# # NPP.stem is contained in NPP.wood. For many data points, a constant relation is probably assumed
# df |> 
#   ggplot(aes(NPP.wood, NPP.stem)) + 
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
#   theme_classic()

# BNPP_1 = NPP.fine + NPP.coarse

df_plot2 <- df |> 
  # filter(NPP.foliage < 1500,  # remove one outlier
  #        NPP.foliage < TNPP_1,
  #        NPP.fine < TNPP_1,
  #        (NPP.wood + NPP.coarse) < TNPP_1
  #        ) |>  
  mutate(a_leaf = NPP.foliage / TNPP_1,
         a_wood = (NPP.wood + NPP.coarse) / TNPP_1,
         a_root = (NPP.fine) / TNPP_1,
         a_above = ANPP_2 / TNPP_1,
         a_below = BNPP_1 / TNPP_1)

df_summ <- df_plot2 |> 
  group_by(pft) |> 
  summarise(a_leaf = mean(a_leaf, na.rm = TRUE),
            a_wood = mean(a_wood, na.rm = TRUE),
            a_root = mean(a_root, na.rm = TRUE),
            a_above = mean(a_above, na.rm = TRUE),
            a_below = mean(a_below, na.rm = TRUE))

df_summ[2,"a_wood"] <- 0
df_summ[2,"a_root"] <- 1 - df_summ[2,"a_leaf"]

write_rds(df_summ, file = here::here("data/df_allocation.rds"))

### Leaf allocation -----------------
gg1 <- df_plot2 |> 
  ggplot(aes(TNPP_1, NPP.foliage)) +
  geom_point(alpha = 0.3) +
  khroma::scale_color_okabeito(name = "") +
  labs(x = expression(paste("BP (gC m"^-2, "yr"^-1, ")")),
       y = expression(paste("Leaf BP (gC m"^-2, "yr"^-1, ")"))) +
  theme_classic() +
  ylim(0, 1200) # not showing one outlier

gg2 <- ggplot(aes(a_leaf, after_stat(count)), data = df_plot2) +
  geom_histogram(fill = "grey70", color = "black") +
  khroma::scale_color_okabeito(name = "") +
  geom_vline(xintercept = mean(df_plot2$a_leaf, na.rm = TRUE), color = "red") +
  labs(x = "Fractional leaf allocation", y = "Count") +
  theme_classic() +
  annotate("text",
           0.33, 
           50, 
           label = paste("mean:", format(mean(df_plot2$a_leaf, na.rm = TRUE), digits = 2)), 
           color = "red",
           hjust = 0) +
  xlim(0, 1)

gg2b <- ggplot(data = df_plot2) +
  geom_density(aes(a_leaf, color = pft)) +
  khroma::scale_color_okabeito(name = "") +
  # geom_vline(xintercept = df_summ[1, "a_leaf"] |> pull(), color = "black", linetype = "dashed") +
  # geom_vline(xintercept = df_summ[2, "a_leaf"] |> pull(), color = "#E69F00", linetype = "dashed") +
  labs(x = "Fractional leaf allocation", y = "") +
  theme_classic() +
  xlim(0, 1)

plot_grid(gg1, gg2, labels = c("a", "b"))

ggsave(here::here("book/images/npp_a_leaf.png"), width = 6, height = 3)

### Wood allocation -----------------
gg3 <- df_plot2 |> 
  ggplot(aes(TNPP_1, (NPP.wood + NPP.coarse))) +
  geom_point(alpha = 0.3) +
  labs(x = expression(paste("BP (gC m"^-2, "yr"^-1, ")")),
       y = expression(paste("Wood BP (gC m"^-2, "yr"^-1, ")"))) +
  theme_classic()

gg4 <- ggplot(aes(a_wood, after_stat(count)), data = df_plot2) +
  # geom_density() +
  geom_histogram(fill = "grey70", color = "black") +
  geom_vline(xintercept = mean(df_plot2$a_wood, na.rm = TRUE), color = "red") +
  labs(x = "Fractional wood allocation", y = "Count") +
  theme_classic() +
  annotate("text",
           0.47,
           20,
           label = paste("mean:", format(mean(df_plot2$a_wood, na.rm = TRUE), digits = 2)),
           color = "red",
           hjust = 0) +
  xlim(0, 1)

plot_grid(gg3, gg4, labels = c("a", "b"))

ggsave(here::here("book/images/npp_a_wood.png"), width = 6, height = 3)

### Root allocation -----------------
gg5 <- df_plot2 |> 
  ggplot(aes(TNPP_1, NPP.fine)) +
  geom_point(alpha = 0.3) +
  labs(x = expression(paste("BP (gC m"^-2, "yr"^-1, ")")),
       y = expression(paste("Root BP (gC m"^-2, "yr"^-1, ")"))) +
  theme_classic()

gg6 <- ggplot(aes(a_root, after_stat(count)), data = df_plot2) +
  geom_histogram(fill = "grey70", color = "black") +
  geom_vline(xintercept = mean(df_plot2$a_root, na.rm = TRUE), color = "red") +
  labs(x = "Fractional root allocation", y = "Count") +
  theme_classic() +
  annotate("text",
           0.29, 
           21, 
           label = paste("mean:", format(mean(df_plot2$a_root, na.rm = TRUE), digits = 2)), 
           color = "red",
           hjust = 0)

plot_grid(gg5, gg6, labels = c("a", "b"))

ggsave(here::here("book/images/npp_a_root.png"), width = 8, height = 4)

### Combined ------------
plot_grid(gg1, gg2, gg3, gg4, gg5, gg6, 
          ncol = 2,
          labels = c("a", "b", "c", "d", "e", "f"))
ggsave(here::here("book/images/npp_a_ALL.png"), width = 6, height = 7)

### Aboveground allocation -----------------
gg1 <- df_plot2 |> 
  ggplot(aes(TNPP_1, ANPP_2, color = pft)) +
  khroma::scale_color_okabeito(name = "") +
  geom_point(alpha = 0.3) +
  labs(x = expression(paste("BP (gC m"^-2, "yr"^-1, ")")),
       y = expression(paste("Aboveground BP (gC m"^-2, "yr"^-1, ")"))) +
  theme_classic() +
  theme(legend.position = "bottom")

gg2 <- ggplot(aes(a_above, after_stat(count)), data = df_plot2) +
  geom_histogram(fill = "grey70", color = "black") +
  geom_vline(xintercept = mean(df_plot2$a_above, na.rm = TRUE), color = "red") +
  labs(x = "Fractional aboveground allocation", y = "Count") +
  theme_classic() +
  annotate("text",
           0.25, 
           25, 
           label = paste("mean:", format(mean(df_plot2$a_above, na.rm = TRUE), digits = 2)), 
           color = "red",
           hjust = 0) +
  xlim(0, 1)

gg2b <- ggplot(aes(a_above, after_stat(count)), data = df_plot2) +
  geom_density(aes(color = pft)) +
  khroma::scale_color_okabeito(name = "") +
  labs(x = "Fractional aboveground allocation", y = "Count") +
  theme_classic() +
  xlim(0, 1)

plot_grid(gg1, gg2, labels = c("a", "b"))

ggsave(here::here("book/images/npp_a_above.png"), width = 6, height = 3)

### Belowground allocation -----------------
gg3 <- df_plot2 |> 
  ggplot(aes(TNPP_1, BNPP_1, color = pft)) +
  geom_point(alpha = 0.3) +
  khroma::scale_color_okabeito(name = "") +
  labs(x = expression(paste("BP (gC m"^-2, "yr"^-1, ")")),
       y = expression(paste("Belowground BP (gC m"^-2, "yr"^-1, ")"))) +
  theme_classic() +
  theme(legend.position = "bottom")

gg4 <- ggplot(aes(a_below, after_stat(count)), data = df_plot2) +
  geom_histogram(fill = "grey70", color = "black") +
  geom_vline(xintercept = mean(df_plot2$a_below, na.rm = TRUE), color = "red") +
  labs(x = "Fractional belowground allocation", y = "Count") +
  theme_classic() +
  annotate("text",
           0.37, 
           25, 
           label = paste("mean:", format(mean(df_plot2$a_below, na.rm = TRUE), digits = 2)), 
           color = "red",
           hjust = 0) +
  xlim(0, 1)

plot_grid(gg3, gg4, labels = c("a", "b"))

ggsave(here::here("book/images/npp_a_below.png"), width = 8, height = 4)

### Combined ------------
plot_grid(gg1, gg2, gg3, gg4,
          ncol = 2,
          labels = c("a", "b", "c", "d"))
ggsave(here::here("book/images/npp_a_AB.png"), width = 8, height = 6)


