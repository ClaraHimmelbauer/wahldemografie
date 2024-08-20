rm(list = ls()); gc()

# packages
packages = c("tidyverse", "readxl")
sapply(packages, library, character.only = T)

# source functions
files.sources = list.files(path = "R", full.names = T)
sapply(files.sources, source, encoding = "utf-8")

#--------------------------------------------------------------------------------------------------


# data --------------------------------------------------------------------------------------------

eu24 <- readxl::read_xlsx("data/eu24.xlsx")
eu19 <- readxl::read_xlsx("data/eu19.xlsx")
nrw17 <- readxl::read_xlsx("data/nrw17.xlsx")
nrw19 <- readxl::read_xlsx("data/nrw19.xlsx")

eu19_map <- read.csv2("data/eu19_mapping.csv")
nrw19_map <- read.csv2("data/nrw19_mapping.csv")
nrw17_map <- read.csv2("data/nrw17_mapping.csv")

graetzl <- readxl::read_xlsx("data/mapping_Graetzl_sprengel_eu2024.xlsx")

# sprengelnummern eindeutig -----------------------------------------------------------------------

x <- ifelse(eu24$bez < 10, paste0("0", eu24$bez), paste0(eu24$bez))
y <- ifelse(eu24$spr < 10, paste0("00", eu24$spr),
            ifelse(eu24$spr >= 100, paste0(eu24$spr), paste0("0", eu24$spr)))
eu24$spr24 <- paste0(x, y)

x <- ifelse(eu19$bez < 10, paste0("0", eu19$bez), paste0(eu19$bez))
y <- ifelse(eu19$spr < 10, paste0("00", eu19$spr),
            ifelse(eu19$spr >= 100, paste0(eu19$spr), paste0("0", eu19$spr)))
eu19$spr19 <- paste0(x, y)

x <- ifelse(nrw19$bez < 10, paste0("0", nrw19$bez), paste0(nrw19$bez))
y <- ifelse(nrw19$spr < 10, paste0("00", nrw19$spr),
            ifelse(nrw19$spr >= 100, paste0(nrw19$spr), paste0("0", nrw19$spr)))
nrw19$spr19 <- paste0(x, y)

x <- ifelse(nrw17$bez < 10, paste0("0", nrw17$bez), paste0(nrw17$bez))
y <- ifelse(nrw17$spr < 10, paste0("00", nrw17$spr),
            ifelse(nrw17$spr >= 100, paste0(nrw17$spr), paste0("0", nrw17$spr)))
nrw17$spr17 <- paste0(x, y)

eu19_map$spr <- ifelse(eu19_map$spr < 10000, paste0("0", eu19_map$spr), eu19_map$spr)
eu19_map$spr24 <- ifelse(eu19_map$spr24 < 10000, paste0("0", eu19_map$spr24), eu19_map$spr24)

nrw19_map$spr <- ifelse(nrw19_map$spr < 10000, paste0("0", nrw19_map$spr), nrw19_map$spr)
nrw19_map$spr24 <- ifelse(nrw19_map$spr24 < 10000, paste0("0", nrw19_map$spr24), nrw19_map$spr24)

nrw17_map$spr <- ifelse(nrw17_map$spr < 10000, paste0("0", nrw17_map$spr), nrw17_map$spr)
nrw17_map$spr24 <- ifelse(nrw17_map$spr24 < 10000, paste0("0", nrw17_map$spr24), nrw17_map$spr24)

colnames(graetzl) <- c("name", "id", "bez", "spr")
graetzl$spr <- ifelse(graetzl$spr < 10000, paste0("0", graetzl$spr), graetzl$spr)

# wahlergebnisse zu heutigen sprengeln zu graetzln ------------------------------------------------

eu24 <- left_join(eu24, graetzl, by = c("spr24" = "spr", "bez" = "bez"))

eu19 <- left_join(eu19, eu19_map, by = c("spr19" = "spr", "bez" = "bez"))
eu19 <- left_join(eu19, graetzl, by = c("spr24" = "spr", "bez" = "bez"))

nrw19 <- left_join(nrw19, nrw19_map, by = c("spr19" = "spr", "bez" = "bez"))
nrw19 <- left_join(nrw19, graetzl, by = c("spr24" = "spr", "bez" = "bez"))

nrw17 <- left_join(nrw17, nrw17_map, by = c("spr17" = "spr", "bez" = "bez"))
nrw17 <- left_join(nrw17, graetzl, by = c("spr24" = "spr", "bez" = "bez"))
