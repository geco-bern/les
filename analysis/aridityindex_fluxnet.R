library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(cwd)
library(gghighlight)
library(cowplot)

# Get site meta info -----------------
sites <- readr::read_csv("~/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv") |>
  filter(!(sitename %in% c("MX-Tes", "US-KS3"))) |>  # failed sites
  left_join(
    readr::read_csv("~/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv"),
    by = "sitename"
  ) |> 
  filter(!drop_lecorr) |>  # where no full year sequence was found
  filter(nyears_lecorr >= 3)

# Load data -------------
path <- "~/data_2/FluxDataKit/v3.4/zenodo_upload/fluxnet/"  # adjust for your own local use
read_onesite <- function(site, path){
  filename <- list.files(path = path, 
                         pattern = paste0("FLX_", site, "_FLUXDATAKIT_FULLSET_DD"), 
                         full.names = TRUE
  )
  out <- read_csv(filename) |> 
    mutate(sitename = site)
  return(out)
}

# read all daily data for the selected sites
ddf <- purrr::map_dfr(
  sites$sitename,
  ~read_onesite(., path)
)

# Select data sequences ----------------
ddf <- ddf |>
  left_join(
    sites |> 
      select(
        sitename, 
        year_start = year_start_lecorr, 
        year_end = year_end_lecorr),
    by = join_by(sitename)
  ) |> 
  mutate(year = year(TIMESTAMP)) |> 
  filter(year >= year_start & year <= year_end) |> 
  select(-year_start, -year_end, -year)

le_to_et <- function(le, tc, patm){
  1000 * 60 * 60 * 24 * le / (cwd::calc_enthalpy_vap(tc) * cwd::calc_density_h2o(tc, patm))
}

# Get mean annual X
adf <- ddf |> 
  mutate(
    # convert latent heat flux into mass flux in mm day-1
    le_mm = le_to_et(LE_F_MDS, TA_F_MDS, PA_F),
    pet = 60 * 60 * 24 * cwd::pet(NETRAD, TA_F_MDS, PA_F)
  ) |> 
  mutate(year = year(TIMESTAMP)) |> 
  group_by(sitename, year) |> 
  summarise(
    prec = sum(P_F),
    aet = sum(le_mm),
    pet = sum(pet)
  ) |> 
  ungroup() |> 
  group_by(sitename) |> 
  summarise(
    prec = mean(prec, na.rm = TRUE),
    aet = mean(aet, na.rm = TRUE),
    pet = mean(pet, na.rm = TRUE)
  )

# Plot --------------
## AET-PET ----------
gg1 <- adf |> 
  ggplot(
    aes(
      x = pet,
      y = aet
    )
  ) + 
  geom_point(color = "tomato") +
  gghighlight(
    sitename %in% c("DE-Hai", "US-Ton", "FI-Hyy", "US-ICh", "AU-How"), 
    label_key = sitename, 
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "grey40")
  ) +
  geom_label(aes(label = sitename),
             hjust = 1, vjust = 1, fill = "white", colour = "black", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(-30, 2500) +
  labs(
    x = expression(paste("PET (mm yr"^-1, ")")),
    y = expression(paste("AET (mm yr"^-1, ")"))
  )

## Budyko ---------
gg2 <- adf |> 
  ggplot(
    aes(
      x = pet/prec,
      y = aet/prec
    )
  ) + 
  geom_point(color = "tomato") +
  gghighlight(
    sitename %in% c("DE-Hai", "US-Ton", "FI-Hyy", "US-ICh", "AU-How"), 
    label_key = sitename, 
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "grey40")
    ) +
  geom_label(aes(label = sitename),
             hjust = 1, vjust = 1, fill = "white", colour = "black", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 3) +
  ylim(0, 2.5) +
  labs(
    x = expression(paste("PET/", italic(P))),
    y = expression(paste("AET/", italic(P)))
  )

plot_grid(
  gg1, 
  gg2,
  labels = c("a", "b")
)

ggsave(here::here("book/images/budyko_fluxnet.png"), width = 8, height = 3.5)


# additional plot not used in the book
gg3 <- adf |> 
  ggplot(
    aes(
      x = pet/prec,
      y = aet/prec
    )
  ) + 
  geom_point(color = "grey40") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 3) +
  ylim(0, 1.2) +
  labs(
    x = expression(paste("PET/", italic(P))),
    y = expression(paste("AET/", italic(P)))
  )

ggsave(here::here("book/images/budyko_only_fluxnet.pdf"), width = 5, height = 3.5)

