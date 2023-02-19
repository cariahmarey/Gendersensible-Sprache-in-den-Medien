options(stringsAsFactors = FALSE)
require(quanteda)
library(dplyr)
library(stringr)

###this script is made to find out what the conetxt of the reg ex is:

# 1. Define the regular expressions to search for in the body
regex_list <- c("\\*in", "\\*innen", ":in", ":innen", "\\(in\\)", "\\(innen\\)", 
                "·in", "·innen", "\\\\_in", "\\\\_innen", "\\/in", "\\/innen", "\\\\/-in", "\\\\/-innen", 
                "[A-Z][a-z]+(In|Innen)")

#2. create two colums in the df, which indicate whther there was a regex found, and which one:
# Use lapply to create a list of logical vectors indicating whether each regex is present in the body column
regex_presence <- lapply(regex_list, function(regex) grepl(regex, sueddeutsche_subset$body))

# Use Reduce to check whether any of the regular expressions are present in each row of the body column
sueddeutsche_subset$regex_present <- Reduce(`|`, regex_presence)

#test which reg ex was found:

sueddeutsche_subset$regex_found <- NA

# Loop through each regex in regex_list
for (regex in regex_list) {
  # Find which rows of taz_subset$body contain the current regex
  regex_presence <- grepl(regex, sueddeutsche_subset$body)
  # Store the current regex in the regex_found column for the rows where the regex is present
  sueddeutsche_subset$regex_found[regex_presence] <- regex
}

#create a corpus from the body column and filter for the sentences which entail gendered language

sueddeutsche_corpus <- corpus(sueddeutsche_subset$body, docnames = sueddeutsche_subset$X)

# split corpus into sentences
sent_sueddeutsche_corpus <- corpus_reshape(sueddeutsche_corpus, to = "sentences")

# get the text of the corpus
sent_sueddeutsche_texts <- texts(sent_sueddeutsche_corpus)

# subset to sentences containing the regular expression
regex_sent_sueddeutsche_texts <- str_subset(sent_sueddeutsche_texts, regex(paste(regex_list, collapse = "|")))

# view the resulting sentences
head(regex_sent_sueddeutsche_texts)

# Write the results to a text file
writeLines(regex_sent_sueddeutsche_texts, "sueddeutsche_regex_results.txt")
