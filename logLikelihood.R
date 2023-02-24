options(stringsAsFactors = FALSE)
library(quanteda)

#this scrpit entails two versions to calculate the loglikelihood test of terms between 
#two corpora. The first function is used for data exploration as individual terms
#can be easily looked up, by adjusting the pattern. The second function is 
#the general one, which does not extra filter for another pattern
#which will be used to test the filtered DTMs

#--------Preparation

#sum up columns for each term in a certain DTM

#define the term counts of the target and comparison:


#----------------#Version 1: Log Likelihood function
#for a certain pattern (can be used to look up individual terms):


calculateLogLikelihood <- function(termCountsTarget, termCountsComparison, pattern, minSignificance = 6.63) {
  
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
logLikelihood <- calculateLogLikelihood(termCountsTarget, 
                                        termCountsComparison, 
                                        pattern = "\\*", #adjust pattern here (problem remains the *)
                                        minSignificance = 6.63)
print(logLikelihood)


#----------------------------2. loglikelyhood (general form)

calculateLogLikelihood2 <- function(termCountsTarget, termCountsComparison, sumalterms_comparison, sumalterms_target, minSignificance = 6.63) {  
  
  uniqueTerms <- setdiff(names(termCountsTarget), names(termCountsComparison))
  
  zeroCounts <- rep(0, length(uniqueTerms))
  names(zeroCounts) <- uniqueTerms
  termCountsComparison <- c(termCountsComparison, zeroCounts)
  
  termsToCompare <- intersect(names(termCountsTarget), names(termCountsComparison))
  
  a <- termCountsTarget[termsToCompare]
  b <- termCountsComparison[termsToCompare]
  c <- sumalterms_target
  d <- sumalterms_comparison
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
