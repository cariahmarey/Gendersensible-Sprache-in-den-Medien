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

#---------- newspapers 

for (i in 1:length(list_subsets)) {
  
  # get subset
  subset <- list_subsets[[i]]
  
  # create corpus
  corpus <- corpus(subset$body, docnames = subset$X)
  
  # tokenization
  tokens <- corpus %>%
    tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
    tokens_replace(pattern = pattern_binneni, replacement = placeholder, case_insensitive = FALSE, valuetype = "regex")%>% #replace binnen-I
    tokens_tolower()
  
  # create DTM 
  DTM <- tokens %>%
    dfm()
  
  #----- filter
  filter_1 <- filter1(DTM)
  newtokens <- adjusttokens_filter2(tokens)
  filter_2 <-filter2(newtokens)
  filter_3 <-filter3(corpus)
  
  #----- log likelihood
  # preparation
  sum_allterms <-sum(colSums(DTM)) #to calculate c
  termCounts1<- colSums(filter_1)
  termCounts2<- colSums(filter_2)
  termCounts3<- filter_3
  
  # test
  ll1<-calculateLogLikelihood(termCounts1, termCountsComparison1, sum_allterms_comparison, sum_allterms, minSignificance = 6.63)
  ll2<-calculateLogLikelihood(termCounts2, termCountsComparison2, sum_allterms_comparison, sum_allterms, minSignificance = 6.63)
  ll3<-calculateLogLikelihood(termCounts3, termCountsComparison3, sum_allterms_comparison, sum_allterms, minSignificance = 6.63)
  
  #----- assign correct names to the objects 
  # FAZ
  if (i == 1) {
    assign(paste0("sum_allterms_faz"), sum_allterms)
    assign(paste0("termCountsfaz1"), termCounts1)
    assign(paste0("termCountsfaz2"), termCounts2)
    assign(paste0("termCountsfaz3"), termCounts3)
    
    assign(paste0("fazll1"), ll1)
    assign(paste0("fazll2"), ll2)
    assign(paste0("fazll3"), ll3)
  }
  
  # Spiegel
  if (i == 2) {
    assign(paste0("sum_allterms_spiegel"), sum_allterms)
    assign(paste0("termCountsspiegel1"), termCounts1)
    assign(paste0("termCountsspiegel2"), termCounts2)
    assign(paste0("termCountsspiegel3"), termCounts3)
    
    assign(paste0("spiegelll1"), ll1)
    assign(paste0("spiegelll2"), ll2)
    assign(paste0("spiegelll3"), ll3)
  }
  
  # taz
  if (i == 3) {
    assign(paste0("sum_allterms_taz"), sum_allterms)
    assign(paste0("termCountstaz1"), termCounts1)
    assign(paste0("termCountstaz2"), termCounts2)
    assign(paste0("termCountstaz3"), termCounts3)
    
    assign(paste0("tazll1"), ll1)
    assign(paste0("tazll2"), ll2)
    assign(paste0("tazll3"), ll3)
  }
  
  # Welt
  else {
    assign(paste0("sum_allterms_welt"), sum_allterms)
    assign(paste0("termCountswelt1"), termCounts1)
    assign(paste0("termCountswelt2"), termCounts2)
    assign(paste0("termCountswelt3"), termCounts3)
    
    assign(paste0("weltll1"), ll1)
    assign(paste0("weltll2"), ll2)
    assign(paste0("weltll3"), ll3)
  }

}

  