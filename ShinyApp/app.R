# Load required libraries
library(shiny)
install.packages("spotifyr")
library(spotifyr)
library(ggplot2)
library(dplyr)
library(tools)

# Set up Spotify API credentials
Sys.setenv(SPOTIFY_CLIENT_ID = '4e04cd9971e843539a71ea0839b8bb5c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'ddd2e2f022544363a1d0c1f578677b5b')
access_token <- get_spotify_access_token()

# Function to get artist's top tracks and genre
get_artist_info <- function(artist_name) {
  artist_search <- search_spotify(artist_name, type = 'artist')
  if (nrow(artist_search) == 0) {
    stop("Artist not found.")
  }
  artist_id <- artist_search$id[1]
  artist_genre <- paste(artist_search$genres[[1]], collapse = ", ")
  top_tracks <- get_artist_top_tracks(artist_id)
  return(list(tracks = top_tracks, genre = artist_genre))
}

get_track_features <- function(track_ids) {
  features <- get_track_audio_features(track_ids)
  return(features)
}

# Load data for the second app (Music Features Over Time)
load("../Data/clean_tracks.Rdata")

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

# Define the UI with tabs
ui <- fluidPage(
  titlePanel("Spotify Analysis Dashboard"),
  
  tabsetPanel(
    # First tab for artist comparison app
    tabPanel("Artist Details",
             sidebarLayout(
               sidebarPanel(
                 textInput("artist_name", "Enter Artist Name:", value = "Arijit Singh"),
                 actionButton("search", "Get Artist Info"),
                 selectInput("feature", "Select Audio Feature to Plot:",
                             choices = c("Danceability", "Energy", "Tempo", "Valence",
                                         "Speechiness", "Acousticness", "Instrumentalness")),
               ),
               mainPanel(
                 uiOutput("artist_image"),
                 textOutput("related_artists"),
                 tableOutput("top_tracks"),
                 plotOutput("feature_plot")
               )
             )
    ),
    
    # Second tab for artist comparison with a different logic
    tabPanel("Artist Comparison",
             sidebarLayout(
               sidebarPanel(
                 textInput("artist1", "Enter First Artist Name", value = "Honey Singh"),
                 textInput("artist2", "Enter Second Artist Name", value = "Badshah"),
                 actionButton("goButton", "Compare Artists"),
                 selectInput("feature", "Show comparison for:",
                             choices = c("danceability", 
                                         "energy", 
                                         "valence", 
                                         "acousticness"))
               ),
               mainPanel(
                 h4("Genres of Selected Artists"),
                 textOutput("artist1_genre"),
                 textOutput("artist2_genre"),
                 plotOutput("featureBoxPlot")
               )
             )
    ),
    
    # Third tab for music features over time app
    tabPanel("Music Features Over Time",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("features", "Select Music Features:", 
                                    choices = colnames(tracks[-c(1, 9, 10)]),
                                    selected = c("Danceability", "Popularity")),
                 sliderInput("years", "Select Year Range:", min = 1947, max = 2021, 
                             value = c(1990, 2017), sep = "")
               ),
               mainPanel(
                 plotOutput("featurePlot"),
                 plotOutput("durationPlot"),
                 plotOutput("tempoPlot")
               )
             )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # First app: artist info and top tracks
  artist_id <- eventReactive(input$search, {
    req(input$artist_name)
    artist_search <- search_spotify(input$artist_name, type = 'artist')
    artist_search$id[1]
  })
  
  # Display artist image
  output$artist_image <- renderUI({
    req(artist_id())
    artist_img_url <- get_artist(artist_id())$images[[1]][1]
    tags$img(src = artist_img_url, height = "300px")
  })
  
  output$related_artists <- renderText({
    req(artist_id())
    x <- get_related_artists(artist_id())
    related <- head(x$name, 3)
    paste("Similar Artists are", paste(related[1:2], collapse = ", "), "and", related[3])
  })
  
  output$top_tracks <- renderTable({
    req(artist_id())
    top_tracks <- get_artist_top_tracks(artist_id())
    
    # Retrieve track names and IDs
    track_names <- top_tracks$name
    track_ids <- top_tracks$id
    
    tracks_details <- get_tracks(track_ids)
    dates <- as.Date(tracks_details$album.release_date)
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
      format(as.Date(dates, format = "%Y-%m-%d"), "%d/%m/%Y"),
      top_tracks$popularity
    )
    names(top_tracks_data) <- c("Track", "Artists", "Duration", "Release Date", "Popularity")
    top_tracks_data
  })
  
  output$feature_plot <- renderPlot({
    req(artist_id())
    req(input$feature)  # Make sure a feature is selected
    
    # Retrieve top tracks and audio features
    top_tracks <- get_artist_top_tracks(artist_id())
    track_names <- top_tracks$name
    track_ids <- top_tracks$id
    audio_features <- lapply(track_ids, get_track_audio_features)
    audio_features_df <- bind_rows(audio_features)
    
    # Select the feature for plotting
    feature_data <- audio_features_df[[tolower(input$feature)]]
    
    # Create a bar plot with ggplot2
    ggplot(data.frame(Track = track_names, Feature = feature_data), aes(x = Track, y = Feature)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = paste(input$feature, "of Top Tracks"), x = "Track", y = input$feature) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Second app: artist comparison
  observeEvent(input$goButton, {
    artist1_data <- get_artist_info(input$artist1)
    artist2_data <- get_artist_info(input$artist2)
    
    artist1_tracks <- artist1_data$tracks
    artist2_tracks <- artist2_data$tracks
    
    output$artist1_genre <- renderText({paste("Genres of", input$artist1, ":", artist1_data$genre) })
    output$artist2_genre <- renderText({paste("Genres of", input$artist2, ":", artist2_data$genre) })
    
    artist1_ids <- artist1_tracks$id
    artist2_ids <- artist2_tracks$id
    
    artist1_features <- get_track_features(artist1_ids)
    artist2_features <- get_track_features(artist2_ids)
    
    artist1_features$artist <- input$artist1
    artist2_features$artist <- input$artist2
    
    combined_data <- rbind(artist1_features, artist2_features)

    output$featureBoxPlot <- renderPlot({
      # Dynamically select the chosen feature from input$feature
      ggplot(combined_data, aes(x = artist, y = .data[[input$feature]], fill = artist)) +
        geom_boxplot() +
        labs(title = paste(toTitleCase(input$feature), "comparison"),
             x = "Artist",
             y = toTitleCase(input$feature))
    })
    
  })
  
  # Third app: music features over time
  output$featurePlot <- renderPlot({
    filtered_data <- tracks[tracks$year >= input$years[1] & tracks$year <= input$years[2], ]
    
    if (length(input$features) == 0) return(NULL)
    
    p <- ggplot(filtered_data, aes(x = year)) + labs(x = "Year", y = "Audio Features")
    
    for (feature in input$features) {
      p <- p + geom_line(aes_string(y = feature, color = paste0("'", feature, "'")))
    }
    
    print(p)
  })
  
  output$durationPlot <- renderPlot({
    filtered_data <- tracks[tracks$year >= input$years[1] & tracks$year <= input$years[2], ]
    ggplot(filtered_data, aes(x = year)) +
      geom_line(aes(y = Duration, color = "Duration")) +
      labs(x = "Year", y = "Duration")
  })
  
  output$tempoPlot <- renderPlot({
    filtered_data <- tracks[tracks$year >= input$years[1] & tracks$year <= input$years[2], ]
    ggplot(filtered_data, aes(x = year)) +
      geom_line(aes(y = Tempo, color = "Tempo")) +
      labs(x = "Year", y = "Tempo")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# Run the application 
shinyApp(ui = ui, server = server)
