###################################################################
######## IMF EXTRACT TEXT FROM APP 24,000 LINKS ###################
###################################################################

# IMF_search_datatrial <- read.delim("C:/Users/user/Google Drive/KONFERANS 2023-2024 CALISMALAR/GUELPH_RESEARCH2023/ENERGY PRICES/METHODOLOGY/IMF_SEARCH_FIRST_10K.txt", sep="")
install.packages("rvest")
install.packages("xml2")


IMF_search_datatrial <- read.delim("C:/Users/user/Google Drive/KONFERANS 2023-2024 CALISMALAR/GUELPH_RESEARCH2023/ENERGY PRICES/METHODOLOGY/IMF_SEARCH_links.txt", sep="")
urls <- IMF_search_datatrial$LINKS


urls <- urls[2608:23882] # 21,275 elements left BECAUSE I SCRAPED THEM BEFORE

# first 2000 from 21,275 elements
# urls <- urls[2001:4000]
# urls <- urls[4001:6000]
# urls <- urls[6001:10000]
# urls <- urls[10001:14631]
# urls <- urls[14632:16877]
# urls <- urls[16878:20278] 
urls <- urls[20279:21275]


library(rvest)
library(xml2)


# Empty list to store the scraped content
scraped_content <- list()

# Loop over the list of URLs and scrape content
for (i in 1:length(urls)) {
  page <- read_html(urls[i], options = "HUGE")
  
  # Extract visible text from paragraphs
  content <- page %>%
    html_elements('p') %>%
    html_text()
  
  # Store the content in the list
  scraped_content[[i]] <- content
  
  # Optional: Print the progress
  cat("Scraped page", i, "of", length(urls), "\n")
}

# Output the scraped content
# print(scraped_content)



for (i in 1:length(scraped_content)) {
  
  # Combine the content into one string (if it's a vector of paragraphs)
  content <- paste(scraped_content[[i]], collapse = "\n")
  
  # Write the content to a text file
  writeLines(content, paste0("scraped_page_", i, ".txt"))
  
  # Optional: Print progress
  cat("Downloaded page", i, "to scraped_page_", i, ".txt\n")
}
