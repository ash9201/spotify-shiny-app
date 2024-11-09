library(dplyr)
load("clean_tracks.Rdata")
tracks <- tracks[-(1:11), ]
tracks <- tracks %>% group_by(year) %>% summarise("Danceability" = mean(danceability), "Energy" = mean(energy), 
                                                   "Valence" = mean(valence),
                                                  "Speechiness" = mean(speechiness), 
                                                  "Acousticness" = mean(acousticness),
                                                  "Instrumentalness" = mean(instrumentalness),
                                                  "Popularity" = mean(popularity)/100)

library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Spotify Music Features Over Time"),

    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("features", 
                           "Select Music Features:", 
                           choices = colnames(tracks[-1]),
                           selected = c("Danceability", "Popularity")),
        
        sliderInput("years", 
                    "Select Year Range:", 
                    min = 1947, 
                    max = 2021, 
                    value = c(1990, 2017), 
                    sep = "")  # no thousands separator
      ),
      
      mainPanel(
        plotOutput("featurePlot")
      )
    )
)

server <- function(input, output) {
  
  output$featurePlot <- renderPlot({
    # Filter the tracks data based on selected years
    filtered_data <- tracks[tracks$year >= input$years[1] & tracks$year <= input$years[2], ]
    
    # Create an empty plot if no features are selected
    if (length(input$features) == 0) {
      return(NULL)
    }
    
    library(ggplot2)
    
    p <- ggplot(filtered_data, aes(x = year)) +
      labs(x = "Year", y = "Feature Value") +
      theme_minimal()
    
    # Add a line for each selected feature
    if ("Danceability" %in% input$features) {
      p <- p + geom_line(aes(y = Danceability, color = "Danceability"))
    }
    
    if ("Energy" %in% input$features) {
      p <- p + geom_line(aes(y = Energy, color = "Energy"))
    }
    
    if ("Valence" %in% input$features) {
      p <- p + geom_line(aes(y = Valence, color = "Valence"))
    }
    
    if ("Speechiness" %in% input$features) {
      p <- p + geom_line(aes(y = Speechiness, color = "Speechiness"))
    }
    
    if ("Acousticness" %in% input$features) {
      p <- p + geom_line(aes(y = Acousticness, color = "Acousticness"))
    }
    
    if ("Instrumentalness" %in% input$features) {
      p <- p + geom_line(aes(y = Instrumentalness, color = "Instrumentalness"))
    }
    
    if ("Popularity" %in% input$features) {
      p <- p + geom_line(aes(y = Popularity, color = "Popularity"), size = 1.5)
    }
    
    print(p)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
