options(stringsAsFactors = FALSE)
library(quanteda)

#this script builds two corpora newspaper(taz) and reference (wortschatz leipzig)
#preprocesses(tokenizes) them
#and creates ther DTM's
#also it has an appendix: excurs for the creation of frequency lists

#------------------------------target data: TAZ

#load target data (taz)

taz_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\taz_subset.csv")

# create corpus
taz_corpus <- corpus(taz_subset$body, docnames = taz_subset$X)

# Preprocessing of the corpus (tokensization)
taz_tokens <- taz_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  tokens_tolower()

####!!tokens to lower makes it impoosible to find regexes charcterised by Capital letters, 

# Create DTM 
taz_DTM <- taz_tokens %>%
  dfm()


#----------------------------- Reference Data (wortschatz Leipzig

#read in data
comparison_text2020 <- readLines("deu_news_2020_30K-sentences.txt")
comparison_text2021 <- readLines("deu_news_2021_30K-sentences.txt")
comparison_text2022 <- readLines("deu_news_2022_30K-sentences.txt")

# create corpus

comparison_corpus <- c(comparison_text2020,comparison_text2021,comparison_text2022)
comparison_corpus <- corpus(comparison_corpus)

# Preprocessing of the corpus (tokensization)
comparison_tokens <- comparison_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  tokens_tolower()

#create DTM
comparison_DTM <- dfm(comparison_tokens)





#------------#short excurs: create Frequency lists (example taz)
# sum columns for word counts
freqs <- colSums(taz_DTM)
# get vocabulary vector
words <- colnames(taz_DTM)
# combine words and their frequencies in a data frame
wordlist <- data.frame(words, freqs)
# re-order the wordlist by decreasing frequency
wordIndexes <- order(wordlist[, "freqs"], decreasing = TRUE)
wordlist <- wordlist[wordIndexes, ]
