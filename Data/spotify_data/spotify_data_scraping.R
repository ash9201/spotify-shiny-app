library(spotifyr)
library(dplyr)

cml <- read.csv("../monthly_listeners/cleaned_monthly_listeners.csv")
cml <- head(cml, 100)
# Set up Spotify API credentials
Sys.setenv(SPOTIFY_CLIENT_ID = '5d41e80c50bd4a9b930636a7ccc0cde3')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '713f28cf65ac4974bf1325eba164a833')
access_token <- get_spotify_access_token()

get_artist_id <- function(artist_name) {
  artist_search <- search_spotify(artist_name, type = 'artist')
  if (nrow(artist_search) == 0) {
    stop("Artist not found.")
  }
  artist_id <- artist_search$id[1]
  return(artist_id)
}

artist_ids <- sapply(cml$Artist, get_artist_id)

cml <- cml %>% mutate(artist_id = artist_ids)

x <- get_related_artists(artist_id)
related <- head(x$name, 3)
paste("Similar Artists are", paste(related[1:2], collapse = ", "), "and", related[3])

get_similar_artists_string <- function(artist_id) {
  x <- get_related_artists(artist_id)  # Get related artists
  related <- head(x$name, 3)  # Get top 3 similar artists
  paste("Similar Artists are", paste(related[1:2], collapse = ", "), "and", related[3])
}

# Apply the function to each artist_id
similar_artists_strings <- sapply(artist_ids, get_similar_artists_string)

cml <- cml %>% mutate(similar = similar_artists_strings)

get_artist_data <- function(artist_id) {
  # Get top tracks for the artist
  top_tracks <- get_artist_top_tracks(artist_id)
  
  # Retrieve track names and IDs
  track_names <- top_tracks$name
  track_ids <- top_tracks$id
  
  # Get track details
  tracks_details <- get_tracks(track_ids)
  dates <- tracks_details$album.release_date
  artist_names <- character(length = nrow(tracks_details))
  
  for(i in 1:nrow(tracks_details)) {
    artist_names[i] <- paste(tracks_details[[1]][[i]]$name, collapse = ", ")
  }
  
  # Get audio features for each track
  audio_features <- lapply(track_ids, get_track_audio_features)
  audio_features_df <- bind_rows(audio_features)
  
  # Combine track names with audio features
  top_tracks_data <- data.frame(
    track_names,
    artist_names,
    Duration = sprintf("%d:%02d", ((audio_features_df$duration_ms) %/% 1e3) %/% 60,
                       ((audio_features_df$duration_ms) %/% 1e3) %% 60),
    dates,
    top_tracks$popularity
  )
  names(top_tracks_data) <- c("Track", "Artists", "Duration", "Release Date", "Popularity")
  
  # Return the combined data as a list
  list(top_tracks_data = top_tracks_data, audio_features_df = audio_features_df)
}

# Iterate over unique artist_ids in your data frame
result <- cml %>%
  distinct(artist_id) %>%
  pull(artist_id) %>%
  map(~ get_artist_data(.))


x <- get_artist(cml$artist_id[1])

x <- sapply(cml$artist_id, get_artist)
artist_genre <- sapply(1:ncol(x), function(i) paste(x[, i]$genres, collapse = ", "))
artist_img <- sapply(1:ncol(x), function(i) x[, i]$images[[1]][1])
cml <- cml %>% mutate(artist_genre)
cml <- cml %>% mutate(artist_img)

save(cml, result, file = "spotify_data.Rdata")
