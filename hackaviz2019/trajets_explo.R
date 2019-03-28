library(here)
library(tidyverse)

theme_set(theme_bw(base_size = 16))

trajets <- read_csv("par_trajet.csv")
trajets <- distinct(trajets)

View(trajets)

my_coms <- c("RAMONVILLE-SAINT-AGNE", "TOULOUSE")
filter(trajets, commune %in% my_coms, travail_commune %in% my_coms) %>% View()

# distance vs N
filter(trajets, `2014_extra_travail_commune` != 0) %>%
    filter(distance_auto_km > 0) %>%
    ggplot(aes(x = distance_auto_km, y = `2014_extra_travail_commune` / habitants)) +
    geom_point(alpha = 0.5, color = "orange", size = 1) +
    geom_smooth(color = "green") +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 0.5))

filter()


# idées 
# heatmaps
# network
# carte puslative couleur / taille

commune <- read_csv("par_commune.csv")

edges <- filter(trajets, `2014_extra_travail_commune` >= 1000)
nodes <- filter(commune, insee %in% unique(c(edges$insee, edges$travail_insee)))

out_of_occ <- unique(c(edges$insee, edges$travail_insee))[which(!unique(c(edges$insee, edges$travail_insee)) %in% commune$insee)]
filter(edges, travail_insee %in% out_of_occ)


library(visNetwork)

myColors <- colorRampPalette(c("lightgray", "black"))(4)

vedges <- with(edges, data.frame(
    from = insee,
    to = travail_insee,
    arrows = "to",
    width = `2014_extra_travail_commune`/500,
    color = list(
        color = case_when(
            distance_auto_km < 10 ~ myColors[1],
            distance_auto_km < 20 ~ myColors[2],
            TRUE ~ myColors[3]
        ),
        highlight = "orange",
        hover = "blue"
    ),
    arrowStrikethrough = FALSE
    # label = paste0(`2014_extra_travail_commune`), #, " travailleurs"),
    
))
vnodes <- with(nodes, data.frame(
    id = insee,
    group = departement,
    value = case_when(
        habitants > 100000 ~ 40,
        habitants > 10000 ~ 30,
        TRUE ~ 20
    ),
    title = paste0(commune, "(", departement, ")\n", habitants, " habitants")
    # label = commune # if_else(habitants > 100000, commune, "")
))
vnodes <- rbind(vnodes, data.frame(
    id = 84007,
    group = 84,
    value = 20,
    title = "AVIGNON (hors Occitanie)"
))
save(vedges, vnodes, file = "network_data.RData")
visNetwork(vnodes, vedges)
