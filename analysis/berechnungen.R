rm(list = ls()); gc()

# packages
packages = c("tidyverse")
sapply(packages, library, character.only = T)

# source functions
files.sources = list.files(path = "R", full.names = T)
sapply(files.sources, source, encoding = "utf-8")

source("analysis/mapping.R", encoding = "utf-8")

#--------------------------------------------------------------------------------------------------

dflist <- list(eu24, eu19, nrw19, nrw17)

# Nichtwaehlerinnen -------------------------------------------------------------------------------

# nichtwaehlerinnen pro sprengel
for(i in 1:length(dflist)){
  dflist[[i]]["nw"] <- dflist[[i]]["wber"] - dflist[[i]]["abg"]
}

# nichtwahelerinnen pro graetzl
nw_pro_graetzl <- function(x){
  y <- x %>%
    group_by(id, name) %>% 
    summarise(wber = sum(wber),
              nw = sum(nw)) %>% 
    mutate(ant_nw = nw / wber) %>% 
    select(id, name, ant_nw)
  return(y)
}
nwlist <- lapply(dflist, nw_pro_graetzl)

# NAs bei graetzeln raus
for(i in 1:length(nwlist)){
  nwlist[[i]] <- nwlist[[i]][!is.na(nwlist[[i]]["id"]), ]
}

# merge
nwdf <- bind_cols(nwlist)[, c(1:3, 6, 9, 12)]
names(nwdf) <- c("id", "name", "nw_eu24", "nw_eu19", "nw_nrw19", "nw_nrw17")

# durchschnittlicher Nichtwaehleranteil
nwdf$mean_nw <- rowSums(nwdf[, 3:6]) / 4

# export
# write.csv2(nwdf, "data/nichtwaehlerinnen_graetzl.csv", row.names = F)

# Wechselwaehlerinnen -----------------------------------------------------------------------------

# eu-wahl -------------------------------------------------

names(eu24) <- paste0(names(eu24), "24")
names(eu19) <- paste0(names(eu19), "19")

# merge für gleiche Reiihenfolge
eu <- left_join(eu24, eu19, by = c("spr2424" = "spr2419"))
eu <- eu[!is.na(eu$vp19), ]
eu <- eu[eu$abg19 != 0, ]

# Wahlergebnisse extrahieren
X24 <- eu[, c("vp24", "sp24", "fp24", "gr24", "ne24", "so24")]
X19 <- eu[, c("vp19", "sp19", "fp19", "gr19", "ne19", "so19")]
which(is.na(X24)); which(is.na(X19))

# prozent berechnen
X24 <- X24 / rowSums(X24)
X19 <- X19 / rowSums(X19)
table(rowSums(X24)); table(rowSums(X19))

# Differenz und Standardabweichung
D <- X24 - X19
D <- sqrt(D^2)

d <- rowSums(D)

# dataframe und aggregieren
dfx <- data.frame(bind_cols(eu$name24, eu$id24, d))
names(dfx) <- c("name", "id", "d")

wweu <- dfx %>% 
  group_by(id, name) %>% 
  summarise(ww_index = mean(d))

# Nationalratswahl --------------------------------------------------------------------------------

names(nrw19) <- paste0(names(nrw19), "19")
names(nrw17) <- paste0(names(nrw17), "17")

# merge für gleiche Reiihenfolge
nrw <- left_join(nrw19, nrw17, by = c("spr2419" = "spr2417"))
nrw <- nrw[!is.na(nrw$vp17) & !is.na(nrw$id17), ]
nrw <- nrw[!is.na(nrw$vp19) & !is.na(nrw$id19), ]
nrw <- nrw[nrw$abg19 != 0, ]

# Wahlergebnisse extrahieren
X19 <- nrw[, c("vp19", "sp19", "fp19", "gr19", "ne19", "so19")]
X17 <- nrw[, c("vp17", "sp17", "fp17", "gr17", "ne17", "so17")]
which(is.na(X19)); which(is.na(X17))

# prozent berechnen
X19 <- X19 / rowSums(X19)
X17 <- X17 / rowSums(X17)
table(rowSums(X19)); table(rowSums(X17))

# Differenz und Standardabweichung
D <- X19 - X17
D <- sqrt(D^2)

d <- rowSums(D)

# dataframe und aggregieren
dfx <- data.frame(bind_cols(nrw$name19, nrw$id19, d))
names(dfx) <- c("name", "id", "d")

wwnrw <- dfx %>% 
  group_by(id, name) %>% 
  summarise(ww_index = mean(d))

# gesamt --------------------------------------------------
ww_index <- (wwnrw$ww_index + wweu$ww_index) / 2

# export ------------------------------------------------------------------------------------------
ex <- nwdf[, c("id", "name", "mean_nw")]
ex$ww_index <- ww_index
write.csv2(ex, "data/greatzl_ergebnisse.csv", row.names = F)
