options(stringsAsFactors = FALSE)
library(quanteda)

#In diesem Script: Logikelihood test

#--------Preparation for Likelikelihood (wird im implementation.R gemacht):

#sum up columns for each term in a certain DTM
#define the term counts of the target and comparison:


#----------------------------1. loglikelyhood (general form)

calculateLogLikelihood <- function(termCountsTarget, termCountsComparison, sumalterms_comparison, sumalterms_target, minSignificance = 6.63) {  
  
  uniqueTerms <- setdiff(names(termCountsTarget), names(termCountsComparison))
  
  zeroCounts <- rep(0, length(uniqueTerms))
  names(zeroCounts) <- uniqueTerms
  termCountsComparison <- c(termCountsComparison, zeroCounts)
  
  termsToCompare <- intersect(names(termCountsTarget), names(termCountsComparison))
  
  a <- termCountsTarget[termsToCompare]
  b <- termCountsComparison[termsToCompare]
  c <- sumalterms_target #hier aufpassen dass nicht der gesamt Count der reduzierten, sondern der vollen DTM genutzt wird
  d <- sumalterms_comparison #hier aufpassen dass nicht der gesamt Count der reduzierten, sondern der vollen DTM genutzt wird
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





####---------------ab hier kann gelöscht werden am Ende, das ist nur hilfreich um mal einzelne Terms nachzuprüfen


#----------------#Version 2: Log Likelihood function
#for a certain pattern (can be used to look up individual terms):


calculateLogLikelihood2 <- function(termCountsTarget, termCountsComparison, pattern, minSignificance = 6.63) {
  
  # Match regular expression pattern against term names
  patternMatch <- grep(pattern, names(termCountsTarget), value = TRUE)
  
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
  
  # compare relative frequencies to indicate over/underuse (we have to see whether we need that)
  relA <- a / c
  relB <- b / d
  # underused terms are multiplied by -1
  logLikelihood[relA < relB] <- logLikelihood[relA < relB] * -1
  
  logLikelihood[logLikelihood < minSignificance] <- 0#
  
  return(logLikelihood)
}


#----function implementation
logLikelihood2 <- calculateLogLikelihood2(termCountsTarget, 
                                        termCountsComparison, 
                                        pattern = "\\*", #adjust pattern here (problem remains the *)
                                        minSignificance = 6.63)
print(logLikelihood)
