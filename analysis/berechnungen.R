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
write.csv2(nwdf, "data/nichtwaehlerinnen_graetzl.csv", row.names = F)

# Wechselwaehlerinnen -----------------------------------------------------------------------------

# eu-wahl -------------------------------------------------

names(eu24) <- paste0(names(eu24), "24")
names(eu19) <- paste0(names(eu19), "19")

eu <- left_join(eu24, eu19, by = c("spr2424" = "spr2419"))
