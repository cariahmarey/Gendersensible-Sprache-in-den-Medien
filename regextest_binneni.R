#-------------- regex test binnnen I
options(stringsAsFactors = FALSE)
#------------------ load necessary libraries
library(quanteda)
library(stringr)

#------------------ set pattern for recognition of binnen-I like "LehrerInnen"
pattern_binneni = "\\b\\w*[A-HJ-KL-Za-hj-klm-z]*I[A-Za-z]*\\b"
placeholder = "myplaceholder"

#------------------ implementation of the scripts

#---------#Referencecoprora -(refernce corpora does not show _?, so it does not exist)
#----#preprocessing
#read in data
comparison_text2020 <- readLines("deu_news_2020_30K-sentences.txt")
comparison_text2021 <- readLines("deu_news_2021_30K-sentences.txt")
comparison_text2022 <- readLines("deu_news_2022_30K-sentences.txt")
# create corpus
reference_corpus <- c(comparison_text2020,comparison_text2021,comparison_text2022)
reference_corpus <- corpus(reference_corpus)

# Preprocessing of the corpus (tokenization)
reference_tokens <- reference_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  tokens_replace(pattern= pattern_binneni, replacement= placeholder, case_insensitive = FALSE)%>%
  tokens_tolower()

matches <- tokens_select(reference_tokens, "myplaceholder")