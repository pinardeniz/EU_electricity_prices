install.packages("Rtools")  # If you haven't already


####### for loop now ############

# Load necessary libraries
install.packages("chromote")
install.packages("rvest")
install.packages("dplyr")
install.packages("purrr")

library(chromote)
library(rvest)
library(dplyr)
library(purrr)

# Read the links from your text file
WBspeech_links <- read.table("WBspeech_links.txt", sep="")

# Convert the link column to a vector
link <- as.vector(WBspeech_links$V1)

# Initialize Chromote session
b <- ChromoteSession$new()

# Create an empty vector to store all extracted TXT* links
all_txt_links <- c()

# Loop through each link in the 'link' vector
for (url in link) {
  b$Page$navigate(url)  # Navigate to the current URL
  Sys.sleep(5)  # Wait for the page to load
  
  # Get the page source
  page_source <- b$Runtime$evaluate('document.documentElement.outerHTML')$result$value
  
  # Parse the HTML source and extract the link with 'TXT*' text
  txt_link <- read_html(page_source) %>%
    html_nodes("a.ng-tns-c1-0") %>%  # Select <a> tags with class 'ng-tns-c1-0'
    .[grepl("TXT\\*", html_text(.))] %>%  # Filter links where text contains 'TXT*'
    html_attr("href")  # Extract the 'href' attribute
  
  # Append the extracted link to the all_txt_links vector
  all_txt_links <- c(all_txt_links, txt_link)
}

# Print the final list of all extracted TXT* links
print(all_txt_links)

writeLines(all_txt_links, "WB_final_links.txt")

# now let's download the content of each website

c <- c(1:318)
c <- paste0(c,".txt")

links <- links$V1



library(rvest)

# Assuming 'all_txt_links' is your vector with the 318 links
c <- paste0(1:318, ".txt")  # Create filenames for the text files

# Loop through each link and scrape the content
for (i in seq_along(all_txt_links)) {
  url <- all_txt_links[i]  # Get the current link
  
  # Read the page content
  page_content <- read_html(url) %>%
    html_nodes("p") %>%  # Select content within <p> tags (you can adjust this if needed)
    html_text()  # Extract the text
  
  # Combine the content into a single string, if there are multiple <p> tags
  page_text <- paste(page_content, collapse = "\n")
  
  # Write the content to a text file
  writeLines(page_text, c[i])
}
