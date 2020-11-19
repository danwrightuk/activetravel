if (!require(stplanr)) {
  install.packages("stplanr", repos = "http://cran.us.r-project.org")
  require(stplanr)
}

if (!require(dplyr)) {
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
  require(dplyr)
}

if (!require(sf)) {
  install.packages("sf", repos = "http://cran.us.r-project.org")
  require(sf)
}

if (!require(tmap)) {
  install.packages("tmap", repos = "http://cran.us.r-project.org")
  require(tmap)
}

od_all <- pct::get_od()

od_all$Active <- (od_all$bicycle + od_all$foot) /
  od_all$all * 100

centroids_all <- pct::get_centroids_ew() %>% sf::st_transform(4326)

wiltshire <-
  pct::pct_regions %>% filter(region_name == "wiltshire")

centroids_wiltshire <- centroids_all[wiltshire,]

od_wiltshire <- od_all %>%
  filter(geo_code1 %in% centroids_wiltshire$msoa11cd) %>%
  filter(geo_code2 %in% centroids_wiltshire$msoa11cd)

od_wiltshire <- od_all[od_all$geo_code1 %in% centroids_wiltshire$msoa11cd &
                          od_all$geo_code2 %in% centroids_wiltshire$msoa11cd,]

desire_lines_wiltshire <-
  od2line(od_wiltshire, centroids_wiltshire)

min_trips_threshold <- 20
desire_lines_inter <-
  desire_lines_wiltshire %>% filter(geo_code1 != geo_code2)
desire_lines_intra <-
  desire_lines_wiltshire %>% filter(geo_code1 == geo_code2)
desire_lines_top <-
  desire_lines_inter %>% filter(all >= min_trips_threshold)

tmap_mode("view")
desire_lines_top <- desire_lines_top %>% arrange(Active)
tm_basemap(leaflet::providers$Stamen.TonerLite) +
  tm_shape(wiltshire) + tm_borders() +
  tm_shape(desire_lines_top) +
  tm_lines(
    palette = "plasma",
    breaks = c(0, 5, 10, 20, 40, 100),
    lwd = "all",
    scale = 9,
    title.lwd = "Number of trips",
    alpha = 0.5,
    col = "Active",
    title = "Active travel (%)",
    legend.lwd.show = FALSE
  ) +
  tm_scale_bar() +
  tm_layout(legend.bg.alpha = 0.5,
            legend.bg.color = "white")