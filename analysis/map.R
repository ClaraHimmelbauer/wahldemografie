rm(list = ls()); gc()

# packages
packages = c("tidyverse", "sf", "leaflet", "htmltools", "htmlwidgets", "xml2")
sapply(packages, library, character.only = T)

# source functions
files.sources = list.files(path = "R", full.names = T)
sapply(files.sources, source, encoding = "utf-8")

# MAP ---------------------------------------------------------------------------------------------

dat <- read.csv2("data/greatzl_ergebnisse.csv")
shp <- geojsonsf::geojson_sf("https://raw.githubusercontent.com/apa-newsroom/austrian-neighbourhood-maps/main/vienna/eu2024/graetzl_eu2024.geojson")

colnames(shp)[1:3] <- c("name", "bez", "id")
shp <- left_join(shp, dat, by = c("name", "id"))

# leaflet

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    position: fixed !important;
    left: 50px;
    text-align: left;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,1);
    font-weight: bold;
    font-size: 20px;
  }
"))
title <- tags$div(
  tag.map.title, HTML("Nichtwähler:innen und Wechselwähler:innen in Wien"))


leaflet(shp) %>% 
  addProviderTiles(providers$BasemapAT.basemap) %>% 
  
  addPolygons(group = "Wechselwählerinnen",
              color = "black", weight = 2, opacity = 1,
              fillColor = ~colorBin("Blues", ww_index)(ww_index), fillOpacity = 0.7,
              label = ~name) %>% 
  
  addPolygons(group = "Nichtwählerinnen",
              color = "black", weight = 2, opacity = 1,
              fillColor = ~colorBin("Reds", mean_nw)(mean_nw), fillOpacity = 0.7,
              label = ~name) %>% 
  
  addControl(title, position = "topleft", className="map-title") %>% 
  
  addLayersControl(baseGroups = c("Wechselwählerinnen", "Nichtwählerinnen"),
                   options = layersControlOptions(collapsed = F),
                   position = "topleft") %>% 
  
  addControl("Die Karte zeigt die durchschnittliche Häufigkeit <br>
             von Nichtwählerinnen und Wechselwählerinnen bei den <br>
             Urnenstimmen bei den letzten Nationalrats- und <br>
             Europaparlamentswahlen in Wien auf Grätzlebene. <br> <br>
             
             Je dunkler das Grätzl, desto mehr Nichtwählerinnen <br>
             bzw Wechselwählerinnen.",
             position = "topleft")

