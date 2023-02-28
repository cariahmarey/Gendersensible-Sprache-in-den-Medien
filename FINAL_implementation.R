options(stringsAsFactors = FALSE)
library(quanteda)
library(stringr)


#In diesem Script: implementation of the scripts differentdfmfilter.R 
#und logLikelihood.R auf die Subsets, samt preprocessing der Corpora



#---------#Referencecoprora -(refernce corpora does not show _?, so it does not exist)
#----#preprocessing
#read in data
comparison_text2020 <- readLines("./dataset_niekler/deu_news_2020_30K-sentences.txt")
comparison_text2021 <- readLines("./dataset_niekler/deu_news_2021_30K-sentences.txt")
comparison_text2022 <- readLines("./dataset_niekler/deu_news_2022_30K-sentences.txt")
# create corpus
reference_corpus <- c(comparison_text2020,comparison_text2021,comparison_text2022)
refernce_corpus <- corpus(reference_corpus)
# Preprocessing of the corpus (tokensization)
reference_tokens <- reference_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  #tokens_replace(pattern= "[A-Z][a-z]*(Innen|In)\\b", replacement="myplaceholder",valuetype="regex")%>%#funktioniert noch nicht, weil das replaced auch words mit lowercased i. also auch Schülerinnen z.B
  tokens_tolower()
#create DTM
reference_DTM <- dfm(reference_tokens)
#----#filter for the differnt dtms (see differentdfmfilters.R)
referencefilter1<-filter1(reference_DTM)
reference_newtokens <- adjusttokens_filter2(reference_tokens)#to adjust the tokens for filter 2
referencefilter2 <-filter2(reference_newtokens)
referencefilter3 <-filter3(reference_corpus)
#----#Loglikelihood
#preparation: _sum up counts for each term
sumalterms_comparison <-sum(colSums(reference_DTM))#to calculate d in the Loglikelidood.R
termCountsComparison1<- colSums(referencefilter1)
termCountsComparison2<- colSums(referencefilter2)
termCountsComparison3<-referencefilter3


#----------#TAZ
#----#preprocessing
taz_subset <- read.csv("taz_subset (3).csv")
# create corpus
taz_corpus <- corpus(taz_subset$body, docnames = taz_subset$X)
# Preprocessing of the corpus (tokensization)
taz_tokens <- taz_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  #tokens_replace(pattern= "[A-Z][a-z]*(Innen|In)\\b", replacement="myplaceholder",valuetype="regex")%>%
  tokens_tolower()
####!!tokens to lower makes it impoosible to find regexes charcterised by Capital letters, 
# Create DTM 
taz_DTM <- taz_tokens %>%
  dfm()
#---#filter
tazfilter1<-filter1(taz_DTM)
taz_newtokens <- adjusttokens_filter2(taz_tokens)
tazfilter2 <-filter2(taz_newtokens)
tazfilter3 <-filter3(taz_corpus)
#Loglikelihood
#preparation
sumalterms_taz <-sum(colSums(taz_DTM))#to calculate c
termCountstaz1<- colSums(tazfilter1)
termCountstaz2<- colSums(tazfilter2)
termCountstaz3<-tazfilter3
#test (see Loklikelihood.R)
tazll1<-calculateLogLikelihood(termCountstaz1, termCountsComparison1, sumalterms_comparison, sumalterms_taz, minSignificance = 6.63)
tazll2<-calculateLogLikelihood(termCountstaz2, termCountsComparison2, sumalterms_comparison, sumalterms_taz, minSignificance = 6.63)
tazll3<-calculateLogLikelihood(termCountstaz3, termCountsComparison3, sumalterms_comparison, sumalterms_taz, minSignificance = 6.63)


#----------#FAZ
#preprocessing
faz_subset <- read.csv("faz_subset (4).csv")

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
#Loglikelihood
#preparation
sumalterms_faz <-sum(colSums(faz_DTM))
termCountsfaz1<- colSums(fazfilter1)
termCountsfaz2<- colSums(fazfilter2)
termCountsfaz3<-fazfilter3
#test
fazll1<-calculateLogLikelihood(termCountsfaz1, termCountsComparison1,sumalterms_comparison, sumalterms_faz, minSignificance = 6.63)
fazll2<-calculateLogLikelihood(termCountsfaz2, termCountsComparison2, sumalterms_comparison, sumalterms_faz, minSignificance = 6.63)
fazll3<-calculateLogLikelihood(termCountsfaz3, termCountsComparison3, sumalterms_comparison, sumalterms_faz, minSignificance = 6.63)

#----------#sueddeutsche: immernoch Satzdopplungen überall. Check so: sueddeutsche_corpus[[6]]
#preprocessing
#sueddeutsche_subset <- read.csv("sueddeutsche_subset (3).csv")
# create corpus
#sueddeutsche_corpus <- corpus(sueddeutsche_subset$body, docnames = sueddeutsche_subset$X)

  # Preprocessing of the corpus (tokensization)
#sueddeutsche_tokens <- sueddeutsche_corpus %>%
#  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  #tokens_replace(pattern= "[A-Z][a-z]*(Innen|In)\\b", replacement="myplaceholder",valuetype="regex")%>%
#  tokens_tolower()
####!!tokens to lower makes it impoosible to find regexes charcterised by Capital letters, 
# Create DTM 
#sueddeutsche_DTM <- sueddeutsche_tokens %>%
#  dfm()
#---#filter
#sueddeutschefilter1<-filter1(sueddeutsche_DTM)
#sueddeutsche_newtokens <- adjusttokens_filter2(sueddeutsche_tokens)
#sueddeutschefilter2 <-filter2(sueddeutsche_newtokens)
#sueddeutschefilter3 <-filter3(sueddeutsche_corpus)
#Loglikelihood
#preparation
#sumalterms_sueddeutsche <-sum(colSums(sueddeutsche_DTM))#to calculate c
#termCountssueddeutsche1<- colSums(sueddeutschefilter1)
#termCountssueddeutsche2<- colSums(sueddeutschefilter2)
#termCountssueddeutsche3<-sueddeutschefilter3
#test#
#sueddeutschell1<-calculateLogLikelihood(termCountssueddeutsche1, termCountsComparison1, sumalterms_comparison, sumalterms_sueddeutsche, minSignificance = 6.63)
#sueddeutschell2<-calculateLogLikelihood(termCountssueddeutsche2, termCountsComparison2, sumalterms_comparison, sumalterms_sueddeutsche, minSignificance = 6.63)
#sueddeutschell3<-calculateLogLikelihood(termCountssueddeutsche3, termCountsComparison3, sumalterms_comparison, sumalterms_sueddeutsche, minSignificance = 6.63)


#---------#spiegel
#preprocessing
spiegel_subset <- read.csv("spiegel_subset (3).csv")

# create corpus
spiegel_corpus <- corpus(spiegel_subset$body, docnames = spiegel_subset$X)
# Preprocessing of the corpus (tokensization)
spiegel_tokens <- spiegel_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  #tokens_replace(pattern= "[A-Z][a-z]*(Innen|In)\\b", replacement="myplaceholder",valuetype="regex")%>%
  tokens_tolower()
####!!tokens to lower makes it impoosible to find regexes charcterised by Capital letters, 
# Create DTM 
spiegel_DTM <- spiegel_tokens %>%
  dfm()
#---#filter
spiegelfilter1<-filter1(spiegel_DTM)
spiegel_newtokens <- adjusttokens_filter2(spiegel_tokens)
spiegelfilter2 <-filter2(spiegel_newtokens)
spiegelfilter3 <-filter3(spiegel_corpus)
#Loglikelihood
#preparation
sumalterms_spiegel <-sum(colSums(spiegel_DTM))#to calculate c
termCountsspiegel1<- colSums(spiegelfilter1)
termCountsspiegel2<- colSums(spiegelfilter2)
termCountsspiegel3<-spiegelfilter3
#test#
spiegelll1<-calculateLogLikelihood(termCountsspiegel1, termCountsComparison1, sumalterms_comparison, sumalterms_spiegel, minSignificance = 6.63)
spiegelll2<-calculateLogLikelihood(termCountsspiegel2, termCountsComparison2, sumalterms_comparison, sumalterms_spiegel, minSignificance = 6.63)
spiegelll3<-calculateLogLikelihood(termCountsspiegel3, termCountsComparison3, sumalterms_comparison, sumalterms_spiegel, minSignificance = 6.63)


#---------#welt
#preprocessing
welt_subset <- read.csv("welt_subset (3).csv")

# create corpus
welt_corpus <- corpus(welt_subset$body, docnames = welt_subset$X)
# Preprocessing of the corpus (tokensization)
welt_tokens <- welt_corpus %>%
  tokens(remove_punct = FALSE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  #tokens_replace(pattern= "[A-Z][a-z]*(Innen|In)\\b", replacement="myplaceholder",valuetype="regex")%>%
  tokens_tolower()
####!!tokens to lower makes it impoosible to find regexes charcterised by Capital letters, 
# Create DTM 
welt_DTM <- welt_tokens %>%
  dfm()
#---#filter
weltfilter1<-filter1(welt_DTM)
welt_newtokens <- adjusttokens_filter2(welt_tokens)
weltfilter2 <-filter2(welt_newtokens)
weltfilter3 <-filter3(welt_corpus)
#Loglikelihood
#preparation
sumalterms_welt <-sum(colSums(welt_DTM))#to calculate c
termCountswelt1<- colSums(weltfilter1)
termCountswelt2<- colSums(weltfilter2)
termCountswelt3<-weltfilter3
#test#
weltll1<-calculateLogLikelihood(termCountswelt1, termCountsComparison1, sumalterms_comparison, sumalterms_welt, minSignificance = 6.63)
weltll2<-calculateLogLikelihood(termCountswelt2, termCountsComparison2, sumalterms_comparison, sumalterms_welt, minSignificance = 6.63)
weltll3<-calculateLogLikelihood(termCountswelt3, termCountsComparison3, sumalterms_comparison, sumalterms_welt, minSignificance = 6.63)








