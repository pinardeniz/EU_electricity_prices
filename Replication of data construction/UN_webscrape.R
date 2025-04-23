# HERE WE EXTRACT DATA FROM WEBSCRAPING FOR THE UN

#remove.packages("rvest")
install.packages("rvest", type = "binary")
library(rvest)
library(dplyr) # For %>% operator



# Create a list to store the extracted links from each page
links_list <- vector("list", 202)

# Loop through pages 1 to 201
for (page_num in 1:202) {
  # Construct the URL for the current page
  url <- paste0("https://www.un.org/sg/en/latest/sg/statement?page=", page_num)
  
  # Read the HTML content from the constructed URL
  doc <- read_html(url)
  
  # Extract all <div> elements
  html_product <- doc %>% html_elements("div")
  html_product1 <- html_product %>% html_elements("div")
  html_product2 <- html_product1 %>% html_elements("div")
  html_product3 <- html_product2 %>% html_elements("div")
  html_product4 <- html_product3 %>% html_elements("div")
  html_product5 <- html_product4 %>% html_elements("div")
  html_product6 <- html_product5 %>% html_elements("div")
  html_product7 <- html_product6 %>% html_elements("div")
  div_elements <- html_product7 %>% html_elements("div")

  
  
  
  # Initialize a vector to store links for the current page
  page_links <- vector("list", length(div_elements))
  
  # Extract <a> elements and their href attributes from each <div> element
  for (i in seq_along(div_elements)) {
    # Extract <a> elements within the current <div> element
    a_elements <- div_elements[[i]] %>% html_elements("a")
    
    # Extract href attributes from the <a> elements
    links <- a_elements %>% html_attr("href")
    
    # Store the links in the page_links vector
    page_links[[i]] <- links
  }
  
  # Flatten the list of links and remove any NULL values
  all_links <- unlist(page_links)
  
  # Store the extracted links in the links_list
  links_list[[page_num]] <- all_links
  
  # Optionally, print the page number to track progress
  print(paste("Page", page_num, "processed"))
}



#########################################################################################

# THERE IS THE FIRST PAGE WITH PAGE0, HOWEVER I CANNOT ADD IT TO THE FOR LOOP
# SINCE IT NEEDS TO START FROM AN INTEGER NUMBER. THAT'S WHY I EXTRACT THE FIRST PAGE ALONE HERE

# The first page starts with 0
doc <- read_html("https://www.un.org/sg/en/latest/sg/statement?page=0")



# Extract all <div> elements
html_product <- doc %>% html_elements("div")
html_product1 <- html_product %>% html_elements("div")
html_product2 <- html_product1 %>% html_elements("div")
html_product3 <- html_product2 %>% html_elements("div")
html_product4 <- html_product3 %>% html_elements("div")
html_product5 <- html_product4 %>% html_elements("div")
html_product6 <- html_product5 %>% html_elements("div")
html_product7 <- html_product6 %>% html_elements("div")
div_elements <- html_product7 %>% html_elements("div")



# Extract <a> elements and their href attributes from <div> element

link0 <- div_elements %>% html_elements("a") %>%   html_attr("href")

link1 <- list(link0)

#################################################################################################

# NOW, I COMBINE THE FIRST LIST AND THIS ONE, WITH THE LAST ON TOP OF THE FIRST ONE
combined_list <- c(link1, links_list)

################################################################################


write.table(combined_list, file="UN_latest_statements_links.xls")

#THEN IN EXCEL, I SEPARATED THE COLUMNS AND SAVED AGAIN.
# NOW I WILL BE USING THIS FILE
UN_latest_statements_links <- read.delim("C:/Users/user/Google Drive/KONFERANS 2023-2024 CALISMALAR/GUELPH_RESEARCH2023/ENERGY PRICES/METHODOLOGY/UN_latest_statements_links.xls", header=FALSE)

####################################################################################

# Convert the dataframe to a vector by column-wise concatenation
vector_data <- as.vector(as.matrix(UN_latest_statements_links))

# Convert the vector to a new dataframe with a single column
single_column_df <- data.frame(single_column = vector_data)

write.table(single_column_df,file = "UN_latest_statements_links_final.xls")


#######################################################################################
#  NOW LET'S DOWNLOAD THESE 4041 LINKS
c <- c(1:4041)
c <- paste0(c,".txt")

link <- UNstatement_links$LINKS




#############################################################################################################
# I USED CHATGPT TO WRITE A FUNCTION CALLED "download_content"
# to download and save visible content from a single URL

library(rvest)
library(httr)

# Function to download and save visible content from a single URL
download_content <- function(url, file_name) {
  tryCatch({
    # Read the webpage
    webpage <- read_html(url)
    
    # Extract text from a specific HTML element (modify the selector as needed)
    visible_content <- webpage %>%
      html_nodes(".content") %>%
      html_text()
    
    # Combine all text into a single string
    visible_content_text <- paste(visible_content, collapse = " ")
    
    # Save the extracted content to a text file
    writeLines(visible_content_text, file_name)
    
    cat("Successfully downloaded:", url, "\n")
  }, error = function(e) {
    cat("Failed to download:", url, "\n")
  })
}

###################################################################################
# NOW THE FOR LOOP TO DOWNLOAD THE VISIBLE CONTENT OF THESE 4041 PAGES 
# USING THE FUNCTION "download_content" CREATED ABOVE


for (i in seq_along(link)) {
    download_content(link[i], c[i])
}

