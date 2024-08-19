rm(list = ls()); gc()

# packages
packages = c("tidyverse", "sf", "leaflet")
sapply(packages, library, character.only = T)

# source functions
files.sources = list.files(path = "R", full.names = T)
sapply(files.sources, source, encoding = "utf-8")

# Polygone übereinanderlegen ----------------------------------------------------------------------
# für jeden alten Sprengel - mit welchen neuen überlappt er
# und wo gibt es die meiste überlappung --> dieser wird als entsprechender sprengel genommen

# data --------------------------------------------------------------------------------------------´

eu24 <- st_read("data/sprshp", "WAHLSPREU2024OGDPolygon")
eu19 <- st_read("data/sprshp", "WAHLSPREU2019OGDPolygon")
nrw19 <- st_read("data/sprshp", "WAHLSPRNR2019OGDPolygon")
nrw17 <- st_read("data/sprshp", "WAHLSPRNR2017OGDPolygon")

eu24 <- eu24[, 5:7]
eu19 <- eu19[, 3:5]
nrw19 <- nrw19[, 5:7]
nrw17 <- nrw17[, 5:7]

colnames(eu24)[1:2] <- colnames(eu19)[1:2] <- colnames(nrw19)[1:2] <- colnames(nrw17)[1:2] <- c("bez", "spr")

# intersections -----------------------------------------------------------------------------------
# jeder Loop dauert 2-3 Stunden!

out <- c()
for(i in 1:nrow(eu19)){
  
  # sprengel, für den die überlappung gefunden werden soll
  spr <- eu19[i, ]
  
  # sprengel, die überlappt werden
  int <- st_intersection(spr, eu24)
  
  # fläche, die sich überlappt
  area <- st_area(int)
  
  # als prozent, aber das brauch ich hier nicht, weil ich nur 1 entsprechenden sprengel hernehme
  # also 1:1 anstatt 1:n, was natürlich sauberer wäre. aber halt auch aufwändiger
  # parea <- area/sum(area)
  
  # sprengel mit meister überlappung
  max <- (which(area == max(area)))
  
  # sprengelnummer abspeichern
  cor <- int$spr.1[max]
  
  # in outvektor
  out <- c(out, cor)
  
  # print to keep track
  print(i)
}
eu19$spr24 <- out

# check
table(as.numeric(substr(eu19$spr24, 1, 2)) == eu19$bez)

# export
ex <- st_drop_geometry(eu19)
write.csv2(ex, "data/eu19_mapping.csv", row.names = F)

# dasselbe für die nrw2019 --------------------------------

out <- c()
for(i in 1:nrow(nrw19)){
  spr <- nrw19[i, ]
  int <- st_intersection(spr, eu24)
  area <- st_area(int)
  max <- (which(area == max(area)))
  cor <- int$spr.1[max]
  out <- c(out, cor)
  print(i)
}
nrw19$spr24 <- out

ex <- st_drop_geometry(nrw19)
write.csv2(ex, "data/nrw19_mapping.csv", row.names = F)

# nrw 2017 ------------------------------------------------

out <- c()
for(i in 1:nrow(nrw17)){
  spr <- nrw17[i, ]
  int <- st_intersection(spr, eu24)
  area <- st_area(int)
  max <- (which(area == max(area)))
  cor <- int$spr.1[max]
  out <- c(out, cor)
  print(i)
}
nrw17$spr24 <- out

ex <- st_drop_geometry(nrw17)
write.csv2(ex, "data/nrw17_mapping.csv", row.names = F)
