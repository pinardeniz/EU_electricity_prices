# Load required libraries
library(chromote)
library(rvest)

# Initialize Chromote session
b <- ChromoteSession$new()

# Initialize variables
all_links <- c()  # To store all the links
num_pages <- 100   # Number of pages to scrape

# Navigate to the first page
b$Page$navigate("https://www.imf.org/en/news/searchnews")

# Wait for the first page to load
#Sys.sleep(5)  # Adjust the time if needed

# Loop through the first 10 pages
for (i in 1:num_pages) {
  
  # Fetch the HTML content of the current page
  page_html <- b$Runtime$evaluate('document.documentElement.outerHTML')$result$value
  
  # Parse the HTML and extract links for the current page
  html_content <- read_html(page_html)
  hrefs <- html_content %>%
    html_elements("a.CoveoResultLink") %>%
    html_attr("href")
  
  # Store the links
  all_links <- c(all_links, hrefs)
  
  # Print the links for the current page
  cat("Links from page", i, ":\n")
  print(hrefs)
  
  # Click the "Right Arrow" to go to the next page, except for the last page
  if (i < num_pages) {
    b$Runtime$evaluate("
      var element = document.querySelector('[aria-label=\"Right Arrow\"]').parentElement;
      if (element) {
        var rect = element.getBoundingClientRect();
        var mouseEvent = new MouseEvent('click', {
          clientX: rect.left + rect.width / 2,
          clientY: rect.top + rect.height / 2,
          bubbles: true,
          cancelable: true,
          view: window
        });
        element.dispatchEvent(mouseEvent);
      }
    ")
    
    # Wait for the next page to load
    Sys.sleep(5)  # Adjust the time if necessary
  }
}

# Remove duplicates, if any
unique_links <- unique(all_links)

# Print the final list of unique links
cat("Total unique links from the first 10 pages:\n")
print(unique_links)

write.table(unique_links, file="IMF_searchsite_links.xls")



###################################################################
######## IMF EXTRACT TEXT FROM APP 24,000 LINKS ###################
###################################################################


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
