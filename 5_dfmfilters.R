options(stringsAsFactors = FALSE)
library(quanteda)
library(stringr)

#Baustellen in diesem Skript: #-Filter 3; RegEx zu Doppelnennung ist noch unsauber, da nicht mit drinn ist, dass der Worststamm bei beiden Wörtern derselbe sein sollte

# this script:
# used to filter the dtm's by different forms of gendered language
# 1. the first part filters a dtm, for a list of words of gendered language created by
# the input of gender.de and richtiggendern
# 2. the second part of the scrpit filters the dtm for a list of reg ex (*innen etc)
# 3. the third part of the script filters for the usage of Doppelnennung of female and male forms
# (Leher und Lehrerinnen)

#--------------------filter 1: filter dtm for list of gendered words:

#function that filters a given DTM through filter 1 and returns the reduced DTM

filter1 <- function (DTM){
  
  #load gender list
  genderworddoc<- read.csv("gendered_words_splitted.csv")
  #create list of words
  genderwordlist<-c(genderworddoc[["gendered_words_splitted"]])
  
  #filter a DTM by the genderwordlist
  dtm_filtered_1<-dfm_select(DTM,
                             pattern = genderwordlist,
                             selection ="keep",padding = FALSE)
  
  return(dtm_filtered_1)
}

#-------------------- filter 1: filter dtm by reg Ex list

# in the tokenization the * symbol is put as a single token, 
#: stays in the middle of the word, 
#() become single tokens
#/ become single tokens
# - stay in the middle of the word
#Capitalletters are lowercased

#--> so we first have to adjust our Tokens in the Corpus:


adjusttokens_filter2 <- function(Tokens) {
  #take care of the : - · _ cases, by splitting tokens:
  symbolsplit <- tokens_split(Tokens, separator = ":",
                              valuetype = c("fixed", "regex"),
                              remove_separator = FALSE)
  
  symbolsplit <- tokens_split(symbolsplit, separator = "-",
                              valuetype = c("fixed", "regex"),
                              remove_separator = FALSE)
  
  symbolsplit <- tokens_split(symbolsplit, separator = "·",
                              valuetype = c("fixed", "regex"),
                              remove_separator = FALSE)
  
  symbolsplit <- tokens_split(symbolsplit, separator = "_",
                              valuetype = c("fixed", "regex"),
                              remove_separator = FALSE)
  
  #take care of the * / () : - · _cases by compunding:
  adjustedtokens<- tokens_compound(symbolsplit, list(c("*", "in"), c("*", "innen"), c("/","in"), 
                                           c("/","innen"), c("(","in",")"),
                                           c("(","innen",")"),c(":", "in"), c(":", "innen"),
                                           c("-", "in"), c("-", "innen"),
                                           c("·", "in"), c("·", "innen"),
                                           c("_", "in"), c("_", "innen")))
  
  #now we only have to consider that the regex entails: _
  #/- schreibweise fällt raus, und wird zu - schreibweise

  return(adjustedtokens)
  
}

filter2 <- function (NewTokens){
  
  #create dtm out of the new tokens
  filter2_dtm <- dfm(NewTokens)
  
  #filter a DTM by the regexlist
  dtm_filtered_2<-dfm_select(filter2_dtm,
                             pattern = c("*_in","*_innen",":_in",":_innen", "(_in_)", "(_innen_)", 
                                         "·_in", "·_innen", "__in", "__innen", "/_in", "/_innen", "-_in", "-_innen", "myplaceholder"),
                             selection ="keep")#myplaceholder identifieziert Binnen-I's

  return(dtm_filtered_2)
}

#-------------------- filter 3: filter dtm by doppelnennung

#function that takes a corpus, applies filter 3, and gives out a NUMBER of matches

filter3 <- function (corpus){
  
  #find all doppelnennung in a corpus
  #doppelnennung <- str_match_all(corpus,"(\\b[A-Z][a-z]* und [A-Z][a-z]*(innen|in)\\b)|(\\b[A-Z][a-z]*(innen|in) und [A-Z][a-z]*\\b)")
  # test:
  doppelnennung <- str_match_all(corpus,"(\\b[A-Z][a-z]*in und [A-Z][a-z]*innen\\b)|(\\b[A-Z][a-z]*innen und [A-Z][a-z]*in\\b)")
  
  
  #no dtm should be created because for the log likelihood, target und reference corpora will 
  # not have many intersections. However, one could use just the sums of reg exes found for both
  #and calculate loklikelyhood with this
  
  #num_doppelnennung <- str_count(corpus,"(\\b[A-Z][a-z]* und [A-Z][a-z]*(innen|in)\\b)|(\\b[A-Z][a-z]*(innen|in) und [A-Z][a-z]*\\b)")
  # test:
  num_doppelnennung <- str_count(corpus,"(\\b[A-Z][a-z]*in und [A-Z][a-z]*innen\\b)|(\\b[A-Z][a-z]*innen und [A-Z][a-z]*in\\b)")
  
  
  filter3sum <-sum(num_doppelnennung)
  
  filter3 <- as.numeric(filter3sum)
  names(filter3) <- "Doppelnennung"
  
  return(filter3)
}

