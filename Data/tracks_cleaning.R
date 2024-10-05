library(spotifyr)
library(dplyr)
library(rvest)
Sys.setenv(SPOTIFY_CLIENT_ID = '5398aa416fac4b148b6538f8f658b1e4')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '9ba10b3142774c4b8170d5b9cfe90491')

access_token <- get_spotify_access_token()

tracks <- read.csv("tracks.csv")

x <- tracks$id_artists
x <- sapply(x, function(s) strsplit(s, "\'")[[1]][2])
tracks$id_artists <- x

x <- tracks$artists
x <- sapply(x, function(s) strsplit(s, "\'")[[1]][2])
tracks$artists <- x

save(tracks, file = "clean_tracks.Rdata")