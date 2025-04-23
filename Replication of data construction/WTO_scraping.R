library(chromote)
library(rvest)
library(dplyr)
library(purrr) # Ensure purrr is loaded

################################################
# FOR 2000-2002
################################################

# Define the URL for the first page
url <- "https://www.wto.org/english/news_e/news00_e/news00_e.htm" # BE CAREFUL HERE

# Start a new Chromote session
b <- ChromoteSession$new()

# Navigate to the page
b$Page$navigate(url)

# Add a delay to ensure the page is fully loaded
Sys.sleep(5)

# Extract the page source
page_source <- b$Runtime$evaluate('document.documentElement.outerHTML')$result$value

# Parse the HTML
parsed_page <- read_html(page_source)

# Extract rows from the table
rows <- parsed_page %>% html_nodes("tr")

# Initialize empty vectors to store data
timestamps <- c()
titles <- c()
urls <- c()

for (row in rows) {
  # Extract timestamp (first <td>)
  timestamp <- row %>%
    html_node("td:first-child span.paracolourtext") %>%
    html_text(trim = TRUE)
  timestamps <- c(timestamps, timestamp)
  
  # Extract title (second <td> -> first span.paracolourtext)
  title <- row %>%
    html_node("td:nth-child(2) span.paracolourtext") %>%
    html_text(trim = TRUE)
  titles <- c(titles, title)
  
  # Extract URL (second <td> -> first <a>)
  url <- row %>%
    html_node("td:nth-child(2) a") %>%
    html_attr("href")
  
  # Handle relative URLs
  url <- if (!is.na(url) && startsWith(url, "../")) {
    paste0("https://www.wto.org/english/news_e/", gsub("^\\.\\./", "", url))
  } else if (!is.na(url)) {
    paste0("https://www.wto.org/english/news_e/news00_e/", url)
  } else {
    NA
  }
  urls <- c(urls, url)
}

# Combine results into a data frame
scraped_data <- data.frame(
  Title = titles,
  Timestamp = timestamps,
  full_url = urls,
  stringsAsFactors = FALSE
)


scraped_data <- scraped_data[!is.na(scraped_data$Title),]

# View the results
write.table(scraped_data,file="2000.txt", row.names = FALSE, col.names = TRUE, sep = "\t")   # BE CAREFUL HERE
