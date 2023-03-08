# load necessary libraries
library(quanteda)
library(stringr)

# in this script:
# filter the dtm's by different forms of gendered language
# 1. the first part filters a dtm, for a list of words of gendered language created by
# the input of gender.de and richtiggendern.de
# 2. the second part of the scrpit filters the dtm for a list of regex (e.g. "*innen")
# 3. the third part of the script filters for the usage of Doppelnennungen of female and male forms
# (e.g. Lehrer und Lehrerinnen)

#---------- filter 1: filter dtm for list of gendered words:
#function that filters a given DTM through filter 1 and returns the reduced DTM

filter1 <- function (DTM){
  
  #load gender list
  genderworddoc<- read.csv("gendered_words_splitted.csv")
  #create list of words
  genderwordlist <- c(genderworddoc[["gendered_words"]])
  
  #filter a DTM by the genderwordlist
  dtm_filtered_1 <- dfm_select(DTM,
                             pattern = genderwordlist,
                             selection ="keep", padding = FALSE)
  
  return(dtm_filtered_1)
}

#---------- filter 2: filter dtm by reg Ex list

# in the tokenization the * symbol is put as a single token, 
# : stays in the middle of the word, 
# () become single tokens
# / become single tokens
# - stay in the middle of the word
# capitalized letters are lowercased

# so: the tokens in the corpus need to be adjusted first

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
  
  # now we only have to consider that the regex entails: _
  # "/-" becomes "-"

  return(adjustedtokens)
}

filter2 <- function (NewTokens){
  
  #create dtm out of the new tokens
  filter2_dtm <- dfm(NewTokens)
  
  #filter a DTM by the regexlist
  dtm_filtered_2<-dfm_select(filter2_dtm,
                             pattern = c("*_in","*_innen",":_in",":_innen", "(_in_)", 
                                         "(_innen_)", "·_in", "·_innen", "__in", 
                                         "__innen", "/_in", "/_innen", "-_in", "-_innen", 
                                         "myplaceholder"), selection ="keep") #myplaceholder identifies binnen-I

  return(dtm_filtered_2)
}

#---------- filter 3: filter dtm by doppelnennungen

#function that takes a corpus, applies filter 3, and gives out a count of matches

filter3 <- function (corpus){
  
  #find all doppelnennungen in the corpus
  doppelnennung <- str_match_all(corpus,"\\b(([A-Z][a-zäöüß]*)([a-zäöüß]*)?) und (\\2(in|innen))\\b|\\b((([A-Z][a-zäöüß]*)(([a-zäöüß]*)?)))+(in|innen)\\b und \\b\\6([a-z]*)")
  
  # get the number of the doppelnennungen in the corpus
  num_doppelnennung <- str_count(corpus,"\\b(([A-Z][a-zäöüß]*)([a-zäöüß]*)?) und (\\2(in|innen))\\b|\\b((([A-Z][a-zäöüß]*)(([a-zäöüß]*)?)))+(in|innen)\\b und \\b\\6([a-z]*)")
  
  filter3sum <-sum(num_doppelnennung)
  
  filter3 <- as.numeric(filter3sum)
  names(filter3) <- "Doppelnennung"
  
  return(filter3)
}

