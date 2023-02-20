options(stringsAsFactors = F)

library(webdriver)
require(webdriver)
install_phantomjs()
require(rvest)

pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#----------- get gendered words for geschicktgendern.de

url <- "https://geschicktgendern.de/"


# load URL to phantomJS session
pjs_session$go(url)
# retrieve the rendered source code of the page
rendered_source <- pjs_session$getSource()
# parse the dynamically rendered source code
html_document <- read_html(rendered_source)

text_path <- "//tbody[contains(@class, 'row-hover')]//td"

title_text <- html_document %>%
  html_nodes(xpath = text_path) %>%
  html_text(trim = T)

# remove words that are not gendered
gendered_words1 <- title_text[c(FALSE,TRUE)]
# create dataframe
gendered_words1 <- data.frame(gendered_words1)
# rename column for rbind later
names(gendered_words1)[1] <- "gendered_words"

#--------------- get gendered words for gendern.de

# Send a GET request to the URL
url <- "https://www.gendern.de/"
page <- read_html(url)

# Create an empty data frame to store the table rows
gendered_words2 <- data.frame()

# Loop through each page
while (TRUE) {
  # Extract the table body from the current page
  table <- page %>% html_nodes("table") %>% html_table(fill = TRUE)
  gendered_words2 <- rbind(gendered_words2, table)
  
  # Look for the button that loads the next page
  next_button <- page %>% html_nodes("button:contains('Weiter')")
  
  # Exit the loop if there's no more pages to load
  if (length(next_button) == 0) {
    break
  }
  
  # Extract the attributes of the next page URL and construct the URL
  next_url <- paste0(url, html_attr(next_button, "onclick"))
  next_url <- gsub(".*\\('", "", next_url)
  next_url <- gsub("'.*", "", next_url)
  
  # Send a GET request to the next page URL
  page <- read_html(next_url)
}

# choose only relevant column with gendered words
gendered_words2 <- data.frame(gendered_words2[,2])
# rename column for rbind later
names(gendered_words2)[1] <- "gendered_words"

#----------- combine both dfs and eliminate duplicates

# combine both dfs
gendered_words <- rbind(gendered_words1, gendered_words2) #8454 rows
# remove duplicates
gendered_words <- gendered_words[!duplicated(gendered_words),] #6245 rows
gendered_words <- data.frame(gendered_words)
# order alphabetically
gendered_words <- gendered_words[order(gendered_words$gendered_words),]
gendered_words <- data.frame(gendered_words)


write.csv(gendered_words, "gendered_words.csv")

library(stringr)
library(splitstackshape)

# separate strings at ";"
gendered_words_splitted <- cSplit(gendered_words, "gendered_words", sep = ";", direction = "long")
gendered_words_splitted <- data.frame(gendered_words_splitted)
# remove rows that contain "[" or "]", because they don't contain genderneutral terms
gendered_words_splitted <- gendered_words_splitted[!grepl("\\[|\\]", gendered_words_splitted$gendered_words),]
gendered_words_splitted <- data.frame(gendered_words_splitted)
