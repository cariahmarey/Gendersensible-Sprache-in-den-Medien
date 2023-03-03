options(stringsAsFactors = FALSE)
library(quanteda)

#In diesem Script: Logikelihood test

#--------Preparation for Likelikelihood (wird im implementation.R gemacht):

#sum up columns for each term in a certain DTM
#define the term counts of the target and comparison:

#----------------------------1. loglikelihood (general form)

calculateLogLikelihood <- function(termCountsTarget, termCountsComparison, sumalterms_comparison, sumalterms_target, minSignificance = 6.63) {  
  
  uniqueTerms <- setdiff(names(termCountsTarget), names(termCountsComparison))
  
  zeroCounts <- rep(0, length(uniqueTerms))
  names(zeroCounts) <- uniqueTerms
  termCountsComparison <- c(termCountsComparison, zeroCounts)
  
  termsToCompare <- intersect(names(termCountsTarget), names(termCountsComparison))
  
  a <- termCountsTarget[termsToCompare]
  b <- termCountsComparison[termsToCompare]
  c <- sumalterms_target #hier aufpassen dass nicht der gesamte Count der reduzierten, sondern der vollen DTM genutzt wird
  d <- sumalterms_comparison #hier aufpassen dass nicht der gesamte Count der reduzierten, sondern der vollen DTM genutzt wird
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
