library(dplyr)

tracks <- read.csv("tracks.csv")

x <- tracks$id_artists
x <- sapply(x, function(s) strsplit(s, "\'")[[1]][2])
tracks$id_artists <- x

x <- tracks$artists
x <- sapply(x, function(s) strsplit(s, "\'")[[1]][2])
tracks$artists <- x

x <- tracks$release_date
x <- sapply(x, function(s) strsplit(s, "-")[[1]][1])
tracks$release_date <- x

tracks <- tracks %>% rename("year" = release_date)
tracks$year <- as.numeric(tracks$year)

tracks <- tracks[-(1:11), ] #Data not accurately available for all the earlier years before 1947
tracks <- tracks %>%
  group_by(year) %>%
  summarise(
    Danceability = mean(danceability),
    Energy = mean(energy),
    Valence = mean(valence),
    Speechiness = mean(speechiness),
    Acousticness = mean(acousticness),
    Instrumentalness = mean(instrumentalness),
    Popularity = mean(popularity) / 100,
    Duration = mean(duration_ms) / 1000,
    Tempo = mean(tempo)
  )

save(tracks, file = "clean_tracks.Rdata")
