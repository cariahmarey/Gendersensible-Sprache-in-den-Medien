# load necessary libraries
library(quanteda)
library(stringr)

# in this script: implementation of the scripts "4logLikelihood" and 5_dfmfilter" 
# on the subsets, including preprocessing of the data

#---------- set pattern for recognition of binnen-I like "LehrerInnen" and set placeholder
pattern_binneni = "\\b\\w+I[a-z]+\\b"
#replace every occurrence of a binnen-I with a placeholder string
placeholder = "myplaceholder" 

#---------- reference corpus
# info: reference does does not contain "_" 

#----- preprocessing
#read in reference data
comparison_text2020 <- readLines("deu_news_2020_30K-sentences.txt")
comparison_text2021 <- readLines("deu_news_2021_30K-sentences.txt")
comparison_text2022 <- readLines("deu_news_2022_30K-sentences.txt")

# create corpus
reference_corpus <- c(comparison_text2020,comparison_text2021,comparison_text2022)
reference_corpus <- corpus(reference_corpus)

# tokenization
reference_tokens <- reference_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>% #don't remove punctuation and symbols, remove numbers
  tokens_replace(pattern = pattern_binneni, replacement = placeholder, case_insensitive = FALSE, valuetype = "regex")%>% #replace binnen-I
  tokens_tolower() #lowercase all tokens

# create DTM
reference_DTM <- dfm(reference_tokens)

#----- filter for the differnt dtms (see "5_dfmfilters")
referencefilter1<-filter1(reference_DTM)
reference_newtokens <- adjusttokens_filter2(reference_tokens) #to adjust the tokens for filter 2
referencefilter2 <-filter2(reference_newtokens)
referencefilter3 <-filter3(reference_corpus)

#----- loglikelihood
#preparation: sum up counts for each term
sum_allterms_comparison <-sum(colSums(reference_DTM)) #to calculate d in "4_loglikelihood"
termCountsComparison1<- colSums(referencefilter1)
termCountsComparison2<- colSums(referencefilter2)
termCountsComparison3<-referencefilter3


#---------- FAZ
#----- preprocessing
faz_subset <- read.csv("faz_subset.csv")

# create corpus
faz_corpus <- corpus(faz_subset$body, docnames = faz_subset$X)

# tokenization
faz_tokens <- faz_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  tokens_replace(pattern = pattern_binneni, replacement = placeholder, case_insensitive = FALSE, valuetype = "regex")%>% #replace binnen-I
  tokens_tolower()
 
# create DTM 
faz_DTM <- faz_tokens %>%
  dfm()

#----- filter
fazfilter1<-filter1(faz_DTM)
faz_newtokens <- adjusttokens_filter2(faz_tokens)
fazfilter2 <-filter2(faz_newtokens)
fazfilter3 <-filter3(faz_corpus)

#----- loglikelihood
#preparation
sum_allterms_faz <-sum(colSums(faz_DTM))
termCountsfaz1<- colSums(fazfilter1)
termCountsfaz2<- colSums(fazfilter2)
termCountsfaz3<-fazfilter3

#test
fazll1<-calculateLogLikelihood(termCountsfaz1, termCountsComparison1,sum_allterms_comparison, sum_allterms_faz, minSignificance = 6.63)
fazll2<-calculateLogLikelihood(termCountsfaz2, termCountsComparison2, sum_allterms_comparison, sum_allterms_faz, minSignificance = 6.63)
fazll3<-calculateLogLikelihood(termCountsfaz3, termCountsComparison3, sum_allterms_comparison, sum_allterms_faz, minSignificance = 6.63)


#---------- spiegel
#----- preprocessing
spiegel_subset <- read.csv("spiegel_subset.csv")

# create corpus
spiegel_corpus <- corpus(spiegel_subset$body, docnames = spiegel_subset$X)

# tokenization
spiegel_tokens <- spiegel_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  tokens_replace(pattern = pattern_binneni, replacement = placeholder, case_insensitive = FALSE, valuetype = "regex")%>% #replace binnen-I
  tokens_tolower()

# create DTM 
spiegel_DTM <- spiegel_tokens %>%
  dfm()

#----- filter
spiegelfilter1<-filter1(spiegel_DTM)
spiegel_newtokens <- adjusttokens_filter2(spiegel_tokens)
spiegelfilter2 <-filter2(spiegel_newtokens)
spiegelfilter3 <-filter3(spiegel_corpus)

#----- loglikelihood
# preparation
sum_allterms_spiegel <-sum(colSums(spiegel_DTM))#to calculate c
termCountsspiegel1<- colSums(spiegelfilter1)
termCountsspiegel2<- colSums(spiegelfilter2)
termCountsspiegel3<-spiegelfilter3

# test
spiegelll1<-calculateLogLikelihood(termCountsspiegel1, termCountsComparison1, sum_allterms_comparison, sum_allterms_spiegel, minSignificance = 6.63)
spiegelll2<-calculateLogLikelihood(termCountsspiegel2, termCountsComparison2, sum_allterms_comparison, sum_allterms_spiegel, minSignificance = 6.63)
spiegelll3<-calculateLogLikelihood(termCountsspiegel3, termCountsComparison3, sum_allterms_comparison, sum_allterms_spiegel, minSignificance = 6.63)


#---------- TAZ
#----- preprocessing
taz_subset <- read.csv("taz_subset.csv")

# create corpus
taz_corpus <- corpus(taz_subset$body, docnames = taz_subset$X)

# tokenization
taz_tokens <- taz_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  tokens_replace(pattern = pattern_binneni, replacement = placeholder, case_insensitive = FALSE, valuetype = "regex")%>% #replace binnen-I
  tokens_tolower()

# Create DTM 
taz_DTM <- taz_tokens %>%
  dfm()

#----- filter
tazfilter1<-filter1(taz_DTM)
taz_newtokens <- adjusttokens_filter2(taz_tokens)
tazfilter2 <-filter2(taz_newtokens)
tazfilter3 <-filter3(taz_corpus)

#----- loglikelihood
#preparation
sum_allterms_taz <-sum(colSums(taz_DTM)) # calculate c
termCountstaz1<- colSums(tazfilter1)
termCountstaz2<- colSums(tazfilter2)
termCountstaz3<-tazfilter3

#test (see "4_loglikelihood")
tazll1<-calculateLogLikelihood(termCountstaz1, termCountsComparison1, sum_allterms_comparison, sum_allterms_taz, minSignificance = 6.63)
tazll2<-calculateLogLikelihood(termCountstaz2, termCountsComparison2, sum_allterms_comparison, sum_allterms_taz, minSignificance = 6.63)
tazll3<-calculateLogLikelihood(termCountstaz3, termCountsComparison3, sum_allterms_comparison, sum_allterms_taz, minSignificance = 6.63)


#---------- welt
# preprocessing
welt_subset <- read.csv("welt_subset.csv")

# create corpus
welt_corpus <- corpus(welt_subset$body, docnames = welt_subset$X)

# tokenization
welt_tokens <- welt_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  tokens_replace(pattern = pattern_binneni, replacement = placeholder, case_insensitive = FALSE, valuetype = "regex")%>% #replace binnen-I
  tokens_tolower()

# create DTM 
welt_DTM <- welt_tokens %>%
  dfm()

#----- filter
weltfilter1<-filter1(welt_DTM)
welt_newtokens <- adjusttokens_filter2(welt_tokens)
weltfilter2 <-filter2(welt_newtokens)
weltfilter3 <-filter3(welt_corpus)

#----- loglikelihood
#preparation
sum_allterms_welt <-sum(colSums(welt_DTM)) #to calculate c
termCountswelt1<- colSums(weltfilter1)
termCountswelt2<- colSums(weltfilter2)
termCountswelt3<-weltfilter3

#test
weltll1<-calculateLogLikelihood(termCountswelt1, termCountsComparison1, sum_allterms_comparison, sum_allterms_welt, minSignificance = 6.63)
weltll2<-calculateLogLikelihood(termCountswelt2, termCountsComparison2, sum_allterms_comparison, sum_allterms_welt, minSignificance = 6.63)
weltll3<-calculateLogLikelihood(termCountswelt3, termCountsComparison3, sum_allterms_comparison, sum_allterms_welt, minSignificance = 6.63)