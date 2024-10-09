library(RSelenium)
library(rvest)
library(data.table)
library(tidyverse)
library(netstat)

# Set up the RSelenium driver
rs_driver_object <- rsDriver(browser = "chrome", chromever = "latest", verbose = F, port = free_port())

remDr <- rs_driver_object$client
remDr$navigate("https://chartmasters.org/most-monthly-listeners-on-spotify/")

# Initialize an empty data.table to store all scraped data
all_data <- data.table()

# Initialize condition and page count for the loop
cond <- TRUE
page_count <- 1  # To track the number of pages

# Scroll to the bottom of the page to ensure content loads
remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
Sys.sleep(15)  # Wait for dynamic content to load

# Loop until all pages are scraped or until we hit 40 pages
while (cond == TRUE) {
  # Get the HTML source of the current page
  data_table_html <- remDr$getPageSource()
  page <- read_html(data_table_html %>% unlist())
  Sys.sleep(0.1)
  # Scrape the table from the page and convert it to a data frame
  df <- html_table(page) %>% .[[1]]
  Sys.sleep(0.1)
  # Remove the 26th row if it exists
  if (nrow(df) >= 26) {
    df <- df[-26, ]  # Delete the 26th row
  }
  
  # Append the new data to all_data
  all_data <- rbindlist(list(all_data, df), use.names = TRUE, fill = TRUE)
  
  # Print a message to track progress (optional)
  print(paste("Scraping page", page_count))
  
  # Stop scraping after 40 pages
  if (page_count >= 40) {
    print("Reached the limit of 40 pages. Stopping.")
    cond <- FALSE
    break
  }
  
  # Wait for some time before navigating to the next page (to avoid overwhelming the server)
  Sys.sleep(0.5)
  
  # Try to click the next button and handle errors
  tryCatch(
    {
      # Click the "Next" button to move to the next page
      Sys.sleep(0.1)
      next_button_found <- FALSE
      retry_count <- 0
      while (!next_button_found && retry_count < 5) {
        tryCatch({
          # Try to find and click the "Next" button
          next_button <- remDr$findElement(using = 'xpath', '//a[contains(@class, "next")]')
          next_button$clickElement()
          next_button_found <- TRUE
        }, error = function(e) {
          # If error occurs, wait and retry
          print("Next button not found. Retrying...")
          Sys.sleep(3)  # Wait before retrying
          retry_count <- retry_count + 1
        })
      }
      
      if (!next_button_found) {
        print("Failed to find the 'Next' button. Stopping.")
        cond <- FALSE
      }
      # Increment page count
      page_count <- page_count + 1
      
      # Wait for the page to load (replace static sleep with waiting for a specific change)
      Sys.sleep(3)  # Adjust this value based on your page load speed
    },
    error = function(e) {
      print("Reached the last page or encountered an error. Script Completed.")
      cond  <<- FALSE
    }
  )
  
  # Break the outer while loop if cond is set to FALSE
  if (cond == FALSE) {
    break
  }
}

# After all pages are scraped, clean and modify the data
# Clean the third column and convert it to integer (handling non-numeric values)
all_data[[3]] <- gsub(",", "", all_data[[3]])  # Remove any commas
all_data[[3]] <- as.integer(all_data[[3]])  # Convert to integer, coerce non-numeric to NA

# Rename the first column to 'Rank'
setnames(all_data, old = names(all_data)[1], new = "Rank")

# View the cleaned data
View(all_data)

# Save the cleaned data to CSV
write_csv(all_data, "cleaned_monthly_listeners.csv")

