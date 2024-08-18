library(sf)

graetzl <- st_read("https://raw.githubusercontent.com/apa-newsroom/austrian-neighbourhood-maps/main/vienna/eu2024/graetzl_eu2024.geojson")
sf::st_write(graetzl, dsn = "data/graezl.geojson", layer = "graezl.geojson")

