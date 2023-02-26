options(stringsAsFactors = FALSE)
require(quanteda)
library(dplyr)
library(tidyr)
library(dplyr)

library(ggplot2)

#this script creates a graph for the usage of words of a respective filter and a respective subset over time 
#(here faz_subset and filter2)


faz_subset <- read.csv("faz_subset (3).csv")


# convert date column to date format
faz_subset$date <- as.Date(faz_subset$date)
#create data column month
faz_subset$month <- substr(faz_subset$date, 0, 7)

#normal preprocessing like always
# create corpus
faz_corpus <- corpus(faz_subset$body, docnames = faz_subset$X)
# Preprocessing of the corpus (tokensization)
faz_tokens <- faz_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  #tokens_replace(pattern= "[A-Z][a-z]*(Innen|In)\\b", replacement="myplaceholder",valuetype="regex")%>%
  tokens_tolower()
####!!tokens to lower makes it impoosible to find regexes charcterised by Capital letters, 
# Create DTM 
faz_DTM <- faz_tokens %>%
  dfm()
#filter
fazfilter1<-filter1(faz_DTM)
faz_newtokens <- adjusttokens_filter2(faz_tokens)
fazfilter2 <-filter2(faz_newtokens)
fazfilter3 <-filter3(faz_corpus)





#create a df that shows the frequencies of the words of filter2 per month
DTM_reduced <- as.matrix(fazfilter2)
counts_per_month <- aggregate(DTM_reduced, by = list(month = faz_subset$month),
                              sum)


#reshape the df
df_long <- counts_per_month %>%
  gather(key = "term", value = "freq", -month)

#create barchart
ggplot(df_long, aes(x = month, y = freq, fill = term)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency of Terms by Month", x = "Month", y = "Frequency") +
  scale_x_discrete(breaks = df_long$month[seq(1, nrow(df_long), 2)], expand = c(0,0))



