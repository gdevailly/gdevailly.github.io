library(here)
library(sf)
library(tidyverse)
library(gganimate)
library(geojsonsf)

sf <- geojsonsf::geojson_sf("par_commune.geojson")

# ggplot(sf, aes(fill = departement)) +
#     geom_sf()

trajets <- read_csv("par_trajet.csv")
trajets <- distinct(trajets)

better_stats <- map_dfr(
    unique(c(trajets$insee, trajets$travail_insee)),
    function(ville) {
        from <- filter(trajets, insee == ville)
        to <- filter(trajets, travail_insee == ville)
        
        tibble(
            insee = from$insee[1],
            commune = from$commune[1],
            extra = sum(from$`2014_extra_travail_commune`),
            intra = sum(to$`2014_extra_travail_commune`)
        )
        
    }
)
sf <- left_join(sf, better_stats, by = "commune")

dep_occ <- sf %>% 
    group_by(departement) %>% 
    summarise(n_communes = n())
# plot(dep_occ) 

# static --------------
ggplot() +
    geom_sf(data = dep_occ, fill = "white") +
    geom_sf(data = filter(sf, !is.na(intra - extra)), mapping = aes(fill = intra - extra), colour = "grey") +
    # scale_fill_distiller(
    #     palette = "PuOr", direction = +1,
    #     breaks = seq(-10000, +10000, by = 5000),
    #     limits = c(-10000, +10000),
    #     oob = scales::squish
    # ) +
    scale_fill_gradient2(
        low = "#e66101", mid = "white", high = "#5e3c99",
        breaks = seq(-5000, +5000, by = 5000),
        limits = c(-5000, +5000),
        oob = scales::squish
    ) +
    theme_void() + coord_sf(datum = NA) +
    labs(fill = "Personnes", title = "Gains pendulaires de populations en Occitanie")

# gif ----------
compdat <- filter(sf, !is.na(intra - extra))
compdat2 <- compdat
compdat2$intra <- 0
compdat2$extra <- 0
compdat$anim <- 1 
compdat2$anim <- 0
anim_dat <- rbind(compdat, compdat2)

an <- ggplot(data = anim_dat, mapping = aes(fill = intra - extra)) +
    geom_sf(data = dep_occ, fill = "white") +
    geom_sf(colour = "grey") +
    scale_fill_gradient2(
        low = "#e66101", mid = "white", high = "#5e3c99",
        breaks = seq(-5000, +5000, by = 5000),
        limits = c(-5000, +5000),
        oob = scales::squish
    ) +
    theme_void() + coord_sf(datum = NA) +
    labs(fill = "Personnes", title = "Gains pendulaires de populations en Occitanie") +
    transition_states(anim, state_length = 1.5)

anim_save(
    "pendulaire.gif",
    animate(
        an,
        nframes = 20, fps = 10,
        width = 800, height = 500
    )
)

# 
library(cartogram)
require("lwgeom")
morphdat <- mutate(compdat, net = intra - extra + 500) %>%
    cartogram_cont("net", itermax = 5)

dep_occ$test <- runif(13)
cartogram_cont(dep_occ, "n_communes")
cartogram_ncont(dep_occ, "test")


p <- st_cast(dep_occ,  'MULTIPOLYGON')
p <- cartogram_cont(p, "n_communes")
   
tm_shape(p) + tm_polygons("test", style = "jenks", legend.show = FALSE) +
    tm_layout(frame = FALSE)

tm_shape(dep_occ) + tm_polygons("test", style = "jenks", legend.show = FALSE) +
    tm_layout(frame = FALSE)

nc_sp <- sf:::as_Spatial(dep_occ) 
p <- cartogram_cont(nc_sp, "n_communes")
