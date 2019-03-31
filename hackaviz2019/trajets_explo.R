library(here)
library(tidyverse)
library(visNetwork)
library(ggrepel)
library(gganimate)

theme_set(theme_bw(base_size = 16))

trajets <- read_csv("par_trajet.csv")
trajets <- distinct(trajets)

commune <- read_csv("par_commune.csv")

edges <- filter(trajets, `2014_extra_travail_commune` >= 600)
nodes <- filter(commune, insee %in% unique(c(edges$insee, edges$travail_insee)))

out_of_occ <- unique(c(edges$insee, edges$travail_insee))[which(!unique(c(edges$insee, edges$travail_insee)) %in% commune$insee)]
filter(edges, travail_insee %in% out_of_occ)

# network --------------
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
    title = paste0(commune, "(", departement, ")\n", habitants, " habitants"),
    label = commune # if_else(habitants > 100000, commune, "")
))
vnodes <- rbind(vnodes, data.frame(
    id = c(84007, 13108),
    group = c(999, 999),
    value = c(20, 20),
    title = c("AVIGNON (hors Occitanie)", "TARASCON (hors Occitanie)"),
    label = c("AVIGNON", "TARASCON")
))
save(vedges, vnodes, file = "network_data.RData")
visNetwork(vnodes, vedges)

# plots -----------
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
) %>% mutate(changement = intra - extra) %>%
    filter(extra != 0 | intra !=0)

par_commune <- left_join(better_stats, commune, by = "insee") %>%
    mutate(gain_relatif = changement / habitants) %>%
    mutate(pref_dep = if_else(
        commune.x %in% c("TOULOUSE", "MONTPELLIER", "TARBES", "AUCH", "MONTAUBAN", "CAHORS", "RODEZ", "ALBI",
                       "MENDE", "NIMES", "CARCASSONNE", "FOIX", "PERPIGNAN"),
        "Prefectures", "Autres villes"
    )) %>%
    mutate(pref_dep = factor(pref_dep, level = c("Prefectures", "Autres villes"))) %>%
    mutate(communes_notables = if_else(
        commune.x %in% c("LEGUEVIN", "LABEGE", "BLAGNAC", "CHUSCLAN"),
        str_to_title(commune.x), NA_character_
    ))

save(par_commune, file = "par_commune.RData")

ggplot(par_commune, aes(x = habitants, y = gain_relatif)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
    geom_point(aes(colour = pref_dep)) +
    geom_smooth(colour = "salmon") +
    geom_text_repel(aes(label = communes_notables)) +
    scale_x_log10() +
    scale_y_continuous(labels = scales::percent) +
    annotation_logticks(side = "b") +
    scale_colour_viridis_d(begin = 0.3, end = 0.8) +
    labs(x = "Nombre d'habitants", y = "Gains relatifs", colour = NULL, 
         title = "Changement relatifs de population pendant le jour en fonction de la population des communes") +
    theme(
        legend.position = c(0.88, 0.9),
        legend.background = element_rect(fill = "white", linetype = "solid", colour = "black", size = 0.5)
    )

# par trajets ---------
mes_trajets <- filter(trajets, `2014_extra_travail_commune` != 0)

travel_distance <- map_dfr(
    seq(0,9),
    function(i) {
        dat <- filter(mes_trajets, distance_oiseau_km <= 10*(i + 1) & distance_oiseau_km > 10*i)
        tibble(
            dist = mean(c(10*(i + 1), 10*i)),
            n = sum(dat$`2014_extra_travail_commune`)
        )
    }
) %>%
    bind_rows(
        tibble(
            dist = 105,
            n = sum(filter(mes_trajets, distance_oiseau_km > 10*10)$`2014_extra_travail_commune`)
        )
    )

save(travel_distance, file = "mes_trajets.RData")

ggplot(travel_distance, aes(x = dist, y = n)) +
    geom_ribbon(aes(ymin = 0, ymax = n), fill = "#FF707070", aplha = 0.5) +
    geom_line(colour = "salmon", size = 2) +
    scale_y_log10() +
    scale_x_continuous(
        breaks = c(0, 25, 75, 100),
        labels = c("0", "25", "75", ">100")
    ) +
    annotation_logticks(side = "l") +
    labs(x = "Distance à vol d'oiseau (km)", y = "Nombre de travailleurs",
         title = "Distance parcouru lors des trajets\ndomicile - travail en Occitanie")

# modèle gravitaire ----------
# sélections des colonnes
data_grav  <- select(
    mes_trajets,
    insee, commune,
    travail_insee, travail_commune,
    distance_oiseau_km, flux = `2014_extra_travail_commune`
)
# somme des flux
data_grav <- map_dfr(
    seq_len(nrow(data_grav)),
    function(i) {
        line <- data_grav[i, ]
        communes <- c(line$commune, line$travail_commune) %>%
            sort()
        lines <- filter(data_grav, commune %in% communes & travail_commune %in% communes)
        tibble(
            ville_A = communes[1],
            ville_B = communes[2],
            distance = mean(lines$distance_oiseau_km),
            flux = sum(lines$flux)
        )
    }
) %>% distinct()

# on va chercher les populations dans le tableau commune
data_grav <- left_join(
    data_grav,
    select(commune, commune, habitants) %>%
        rename(pop_A = habitants),
    by = c("ville_A" = "commune")
) %>%
    left_join(
        select(commune, commune, habitants) %>%
            rename(pop_B = habitants),
        by = c("ville_B" = "commune")
    )

# produit des populations
data_grav <- mutate(data_grav, prod_pop = pop_A * pop_B) %>%
    filter(!is.na(prod_pop))

# flux = k * prod_pop * 1/(dist^alpha)
# log(flux) - log(prod_pop) = log(k) - alpha*log(dist))
data_grav <- mutate(data_grav, Y = log(flux) - log(prod_pop), X = log(distance))
    
mlm <- lm(Y ~ 1 + X, data = data_grav)
k <- exp(coefficients(mlm)[1])
alpha <- -coefficients(mlm)[2]

data_grav <- mutate(
    data_grav,
    flux_predit = k * prod_pop * (1/(distance^alpha))
)

save(data_grav, file = "data_grav.Rdata")

p <- ggplot(data_grav, aes(x = flux, y = flux_predit)) +
    geom_density2d() +
    geom_point(aes(text = paste(ville_A, ville_B, sep = " - "))) +
    geom_abline(intercept = 0, slope = 1) +
    scale_x_log10() +
    scale_y_log10() +
    annotation_logticks(side = "bl") +
    annotate("text", x = 8000, y = 1, label = "k=0.00012\nalpha = 1.7\nR² = 0.46", hjust = 0) +
    labs(x = "Flux observé (log10)", y = "Flux prédit (log10)",
         title = "Model gravitaire des déplacements\ndomicile - travail en Occitanie",
         caption = "k=0.00012, alpha = 1.7, R² = 0.46") +
    theme_bw(base_size = 12)
plotly::ggplotly(p, tooltip = "text")
