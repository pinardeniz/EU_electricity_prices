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



#######################################################


# # to create folders for each country
# folders <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015",
# "2016","2017","2018","2019","2020","2021","2022","2023","2024")
# for (folder in folders) {
#   dir.create(folder)
# }

# library(rvest)
# library(xml2)


# WE HAVE THE LINKS FOR EACH YEAR (E.G., 2000.txt), AND NOW WE EXTRACT THE CONTENT OF EACH LINK FOR EACH YEAR 
setwd("C:/.../WTO")

data <- read.table("2024.txt",header = T)
data_clean<- data[!is.na(data$full_url),]

urls <- data_clean$full_url

setwd("C:/.../WTO/2024")


# Empty list to store the scraped content
scraped_content <- list()

# Loop over the list of URLs and scrape content
for (i in 1:length(urls)) {
  # Use tryCatch to handle errors gracefully
  tryCatch({
    # Attempt to read the HTML
    page <- read_html(urls[i], options = "HUGE")
    
    # Extract visible text from paragraphs
    content <- page %>%
      html_elements('p') %>%
      html_text()
    
    # Save the content to a text file
    writeLines(content, paste0("scraped_page_", i, ".txt"))
    
    # Store the content in the list
    scraped_content[[i]] <- content
    
    # Print progress
    cat("Scraped page", i, "of", length(urls), "\n")
  }, error = function(e) {
    # Handle the error for broken URL
    writeLines("", paste0("scraped_page_", i, ".txt")) # Create an empty file
    scraped_content[[i]] <- NULL # Store NULL or empty content in the list
    cat("Error scraping page", i, ":", conditionMessage(e), "\n")
  })
}

