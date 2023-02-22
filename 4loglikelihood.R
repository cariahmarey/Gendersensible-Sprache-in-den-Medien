Sys.setenv(LANG = "en")

# import csvs as dataframes
faz_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\faz_subset.csv", fileEncoding = "UTF-8")
spiegel_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\spiegel_subset.csv", fileEncoding = "UTF-8")
sueddeutsche_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\sueddeutsche_subset.csv", fileEncoding = "UTF-8")
taz_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\taz_subset.csv", fileEncoding = "UTF-8")
welt_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\welt_subset.csv", fileEncoding = "UTF-8")

#--------------DTM for TAZ

options(stringsAsFactors = FALSE)
library(quanteda)
# read the corpus data

taz_corpus <- corpus(taz_subset$body, docnames = taz_subset$X)

# Preprocessing of the corpus
taz_tokens <- taz_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  tokens_tolower()
####tokens to lower makes it impoosible to find 
#regexes charcterised by Capital letters, 
#also reg exes which are n grams are not to be found
#also somehow in this step, the sonderzeichen get lost

# Create DTM
taz_DTM <- taz_tokens %>%
  dfm()

#sum up tokens
term_in_docs_taz <- colSums(taz_DTM)


#-----------------------------DTM for Sueddeutsche (here used as reference corpora)

# read the corpus data

sueddeutsche_corpus <- corpus(sueddeutsche_subset$body, docnames = sueddeutsche_subset$X)

# Preprocessing of the corpus
sueddeutsche_tokens <- sueddeutsche_corpus %>%
  tokens(remove_punct = F, remove_numbers = TRUE, remove_symbols = FALSE) %>%
  tokens_tolower()

# Create DTM
sueddeutsche_DTM <- sueddeutsche_tokens %>%
  dfm()

#sum up tokens
term_in_docs_sueddeutsche <- colSums(sueddeutsche_DTM)

#---------------LogLikelihood function fpr a certain pattern:


calculateLogLikelihood <- function(termCountsTarget, termCountsComparison, pattern, minSignificance = 6.63) {
  
  # Match regular expression pattern against term names
  patternMatch <- grep(pattern, names(termCountsTarget), value = TRUE)
  #this only matches the pattern and not a list
  
  # Only consider terms that match pattern in both corpora
  termsToCompare <- intersect(patternMatch, names(termCountsComparison))
  
  
  a <- termCountsTarget[termsToCompare]
  b <- termCountsComparison[termsToCompare]
  c <- sum(termCountsTarget)
  d <- sum(termCountsComparison)
  Expected1 = c * (a+b) / (c+d)
  Expected2 = d * (a+b) / (c+d)
  t1 <- a * log((a/Expected1) + (a == 0))
  t2 <- b * log((b/Expected2) + (b == 0))
  logLikelihood <- 2 * (t1 + t2)
  
  # compare relative frequencies to indicate over/underuse
  relA <- a / c
  relB <- b / d
  # underused terms are multiplied by -1
  logLikelihood[relA < relB] <- logLikelihood[relA < relB] * -1
  
  logLikelihood[logLikelihood < minSignificance] <- 0
  
  return(logLikelihood)
}


#---------------------test out


termCountsTarget <- term_in_docs_taz
termCountsComparison <- term_in_docs_sueddeutsche

logLikelihood <- calculateLogLikelihood(termCountsTarget, 
                                        termCountsComparison, 
                                        pattern = "[a-z]on", 
                                        minSignificance = 6.63)
print(logLikelihood)