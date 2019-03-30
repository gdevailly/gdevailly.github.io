library(here)
library(sf)
library(tidyverse)
library(gganimate)
library(geojsonsf)
library(ggrepel)

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
par_commune <- read_csv("par_commune.csv")
prefectures <- filter(
    par_commune,
    commune %in% c("TOULOUSE", "MONTPELLIER", "TARBES", "AUCH", "MONTAUBAN", "CAHORS", "RODEZ", "ALBI",
                   "MENDE", "NIMES", "CARCASSONNE", "FOIX", "PERPIGNAN")
)

ggplot(data = filter(sf, !is.na(intra - extra)), mapping = aes(fill = intra - extra)) +
    geom_sf(colour = "grey") +
    geom_sf(data = dep_occ, fill = NA) +
    annotate(
         "text",
         x = prefectures$latitude, y = prefectures$longitude + 0.1, label = str_to_title(prefectures$commune),
         colour = "darkolivegreen", size = 3
    ) +
    annotate(
        "segment",
        x = prefectures$latitude, xend = prefectures$latitude,
        y = prefectures$longitude + 0.05, yend = prefectures$longitude,
        colour = "darkolivegreen"
    ) +
    scale_fill_gradientn(
        colours = c("blue","white", "red", "black"), 
        breaks = seq(-5000, +10000, by = 5000),
        limits = c(-5000, +10000),
        oob = scales::squish
    ) +
    theme_minimal(base_size = 14) + coord_sf(datum = NA) +
    labs(fill = "Changements\nde population", title = "Gains pendulaires de populations en Occitanie", x = NULL, y = NULL) +
    theme(legend.position = c(0.85, 0.2))


# gif ----------
compdat <- filter(sf, !is.na(intra - extra))
compdat2 <- compdat
compdat2$intra <- 0
compdat2$extra <- 0
compdat$anim <- "Jour"
compdat2$anim <- "Nuit"
anim_dat <- rbind(compdat, compdat2)

an <- ggplot(data = anim_dat, mapping = aes(fill = intra - extra)) +
    geom_sf(colour = "grey") +
    geom_sf(data = dep_occ, fill = NA, colour = "darkgrey") +
    annotate(
        "text",
        x = prefectures$latitude, y = prefectures$longitude + 0.1, label = str_to_title(prefectures$commune),
        colour = "black", size = 5
    ) +
    annotate(
        "segment",
        x = prefectures$latitude, xend = prefectures$latitude,
        y = prefectures$longitude + 0.05, yend = prefectures$longitude,
        colour = "black"
    ) +
    scale_fill_gradientn(
        colours = c("blue","white", "red", "darkred"), 
        breaks = seq(-5000, +10000, by = 5000),
        limits = c(-5000, +10000),
        oob = scales::squish
    ) +
    theme_minimal(base_size = 18) + coord_sf(datum = NA) +
    labs(fill = "Changement de population:", title = "Gains pendulaires de population en Occitanie", x = NULL, y = NULL,
         subtitle = "{closest_state}") +
    theme(legend.position = c(0.88, 0.16)) +
    transition_states(anim, state_length = 1.5)

anim_save(
    "pendulaire.gif",
    animate(
        an,
        nframes = 20, fps = 10,
        width = 840, height = 600
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
