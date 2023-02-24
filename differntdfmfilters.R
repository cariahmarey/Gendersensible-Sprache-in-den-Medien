options(stringsAsFactors = FALSE)
library(quanteda)
library(stringr)

#Baustellen in diesem Skript: SchülerInnen wird nicht gefunden, wegen lowercasing the tokens, sowie frauen*
#                             word boundaries bei doppelnennung sind nicht im reg ex, dadurch false positives


#this script is used to filter the dtm's by differnt forms of gendered language
#1. the first part filters a dtm, for a list of words of gendered language created by
#the input of gender.de and richtiggendern
#2. the second part of the scrpit filters the dtm for a list of reg ex (*innen etc)
#3. the third part of the script filters for the usage of Doppelnennung of female and male forms
#(Leher und Lehrerinnen)



#--------------------1: filter dtm for list of gendered words:



#function that filters a given DTM through filter 1 and returns the reduced DTM

filter1 <- function (DTM){
  
  #load gender list
  genderworddoc<- read.csv("gendered_words_splitted.csv")
  #create list of words
  genderwordlist<-c(genderworddoc1[["gendered_words_splitted"]])
  
  #filter a DTM by the genderwordlist
  dtm_filtered_1<-dfm_select(DTM,
                             pattern = genderwordlist,
                             selection ="keep",padding = FALSE)
  
  #create new dtm sums for Loglikelyhoodtest
  
  sum_dtm_filtered_1 <- colSums(dtm_filtered_1)
  
  return(sum_dtm_filtered_1)
}

filter1(taz_DTM)


#------------------------------2: Filter dtm by reg Ex list

# in the tokenization the * symbol is put as a single token, 
#: stays in the middle of the word, 
#() become single tokens
#/ become single tokens
# - stay in the middle of the word
#Capitalletters are lowercased

#--> so we first have to adjust our Tokens in the Corpus:


# Here is a sample Text to text how to do that
#sample <- "hus_in Schulerin und Schuler schüler und leher my name is Schüler·innen Christina. 50 Sometimes Lehrer/in we get some weirdness
#Hello my name is Michael:in, 
#sometimes schmim(innen) we get some Sculer und Lehereinnen weird,and odd, results-- 50 I  want to hund und katin replace the 
# 50s"
#corpus<-corpus(sample)
#toks<-tokens(corpus)


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
  #Offen: take care of the I and Frauen* (lowercasing): "[A-Z][a-z]+(In|Innen)", "[A-Z][a-z]+*
  
  
  return(adjustedtokens)
  
}


filter2 <- function (NewTokens){
  
  #apply these steps to the tokens of the corpurs
 

  #create dtm out of the new tokens
  
  filter2_dtm <- dfm(NewTokens)
  
  
  #filter a DTM by the regexlist
  dtm_filtered_2<-dfm_select(filter2_dtm,
                             pattern = c("*_in","*_innen",":_in",":_innen", "(_in_)", "(_innen_)", 
                                         "·_in", "·_innen", "__in", "__innen", "/_in", "/_innen", "-_in", "-_innen"),
                             selection ="keep")#again, something does not work with the reg exes
  
  #sum up columns for loglikelyhod calculation
  
  sum_dtm_filtered_2 <- colSums(dtm_filtered_2)
  
  return(sum_dtm_filtered_2)
}

#test
filter2(toks)


#------------------------------3: Filter dtm by doppelnennung

#function that takes a corpus, applies filter 3, and gives out a NUMBER of matches

filter3 <- function (corpus){
  
  #find all doppelnennung in a corpus
  doppelnennung <- str_match_all(corpus,"([A-Z][a-z]* und [A-Z][a-z]*(innen|in))|([A-Z][a-z]*(innen|in) und [A-Z][a-z]*)")
  #the regex should have a word boundary to exclude cases like "hallo und diskrimnin", but somehow i do not manage
  
  print(doppelnennung)
  #no dtm should be created because for the log likelihood, target und reference corpora will 
  # not have many intersections. However, one could use just the sums of reg exes found for both
  #and calculate loklikelyhood with this
  
  num_doppelnennung <- str_count(corpus,"([A-Z][a-z]* und [A-Z][a-z]*(innen|in))|([A-Z][a-z]*(innen|in) und [A-Z][a-z]*)")
  filter3sum <-sum(num_doppelnennung)
  
  
  return(filter3sum)
}

#test
filter3(taz_corpus)

#notes

#OR#create 3-grams first
#tokens_3gram <- tokens_ngrams(toks, n =3)
#DTM <- dfm(tokens_3gram)
#Do not Lowercase (?maybe already from the beginning)
#filter 3grams for regex:
#filtered_3grams <- kwic(tokens_3gram, pattern = "[a-z]und*")
#sum_filter_3sum <-(filtered_3grams)


#----------------------------4: filter dtm through a dictionary (does not work from the logic)


#full_filter_dtm <- dictionary(list(filter1=c("gendern*|Gender*"),
                                    #filter2 =c("in"),
                                    #filter3=       ))

#all_filtered_dtm <- dfm_lookup(taz_DTM,full_filter_dtm,nomatch = "unmatched")


