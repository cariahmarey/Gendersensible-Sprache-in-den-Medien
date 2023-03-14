options(stringsAsFactors = FALSE)
require(quanteda)
library(dplyr)
library(stringr)

# in this script the context of the regex is visualized:

#---------- set pattern for recognition of binnen-I like "LehrerInnen" and set placeholder
pattern_binneni = "\\b\\w+I[a-z]+\\b"
# define the placeholder string
placeholder = "myplaceholder"

# replace every occurrence of the regular expression in the subsets with the placeholder
for (i in 1:length(list_subsets)) {
  # get subset
  subset <- list_subsets[[i]]
  # replace the regex with the placeholder string
  subset$body <- gsub(pattern_binneni, placeholder, subset$body)
  
  #----- assign correct names to the objects 
  # FAZ
  if (i == 1) {
    assign(paste0("faz_subset_withplaceholder"), subset)
  }
  
  # Spiegel
  if (i == 2) {
    assign(paste0("spiegel_subset_withplaceholder"), subset)
  }
  
  # taz
  if (i == 3) {
    assign(paste0("taz_subset_withplaceholder"), subset)
  }
  
  # Welt
  else {
    assign(paste0("welt_subset_withplaceholder"), subset)
  }
  
}

# put dataframes into list
list_subsets_withplaceholder <- list(faz_subset_withplaceholder, spiegel_subset_withplaceholder,
                                     taz_subset_withplaceholder, welt_subset_withplaceholder)

#---------- regex for gender characters

# define the regular expressions to search for in the body
regex_list <- "\\b[A-Z][a-zäöüß]*\\b\\*in|\\b[A-Z][a-zäöüß]*\\*innen\\b|\\b[A-Z][a-zäöüß]*:in\\b|\\b[A-Z][a-zäöüß]*:innen\\b|\\b[A-Z][a-zäöüß]*in_\\b|\\b[A-Z][a-zäöüß]*innen_\\b|\\b[A-Z][a-zäöüß]*·in\\b|\\b[A-Z][a-zäöüß]*·innen\\b|\\b[A-Z][a-zäöüß]*_in\\b|\\b[A-Z][a-zäöüß]*__innen\\b|\\b[A-Z][a-zäöüß]*\\/in\\b|\\b[A-Z][a-zäöüß]*\\/innen\\b|\\b[A-Z][a-zäöüß]*-in\\b|\\b[A-Z][a-zäöüß]*-innen\\b|\\bmyplaceholder\\b"

for (i in 1:length(list_subsets_withplaceholder)) {
  
  subset_withplaceholder <- list_subsets_withplaceholder[[i]]
  
  # create two colums in the df, which indicate whether there was a regex found, and which one:
  # Use lapply to create a list of logical vectors indicating whether each regex is present in the body column
  regex_presence <- lapply(regex_list, function(regex) grepl(regex, subset_withplaceholder$body))
  
  # use Reduce to check whether any of the regular expressions are present in each row of the body column
  subset_withplaceholder$regex_present <- Reduce(`|`, regex_presence)
  
  # test which reg ex was found:
  subset_withplaceholder$regex_found <- NA
  
  # loop through each regex in regex_list
  for (regex in regex_list) {
    
    # find which rows of subset_withplaceholder$body contain the current regex
    regex_presence <- grepl(regex, subset_withplaceholder$body)
    
    # store the current regex in the regex_found column for the rows where the regex is present
    subset_withplaceholder$regex_found[regex_presence] <- regex
  }
  
  #create a corpus from the body column and filter for the sentences which entail gendered language
  corpus <- corpus(subset_withplaceholder$body, docnames = subset_withplaceholder$X)
  
  # split corpus into sentences
  corpus_sentences <- corpus_reshape(corpus, to = "sentences")
  
  # get the text of the corpus
  texts_sentences <- texts(corpus_sentences)
  
  # subset to sentences containing the regular expression
  regex_texts_sentences <- str_subset(texts_sentences, regex(paste(regex_list, collapse = "|")))
  
  # save dataframes as xlsx
  # FAZ
  if (i == 1) {
    # Write the results to a text file
    write.xlsx(regex_texts_sentences, "faz_regex_results.xlsx")
  }
  # Spiegel
  if (i == 2) {
    # Write the results to a text file
    write.xlsx(regex_texts_sentences, "spiegel_regex_results.xlsx")
  }
  # taz
  if (i == 3) {
    # Write the results to a text file
    write.xlsx(regex_texts_sentences, "taz_regex_results.xlsx")
  }
  # Welt
  else {
    # Write the results to a text file
    write.xlsx(regex_texts_sentences, "welt_regex_results.xlsx")
  }
  
}

#---------- neutral forms
#load gender list
genderworddoc<- read.csv("gendered_words_splitted.csv")

# create list of words
#genderwordlist <- c(genderworddoc[["gendered_words"]])
genderwordlist_selected <- as.list(as.character(counts_df_ll1$Neutralisierung))

for (i in 1:length(list_subsets_withplaceholder)) {
  
  subset_withplaceholder <- list_subsets_withplaceholder[[i]]
  
  # create two colums in the df, which indicate whether there was a neutralform found, and which one:
  # Use lapply to create a list of logical vectors indicating whether each neutralform is present in the body column
  neutralform_presence <- lapply(genderwordlist_selected, function(word) grepl(word, subset_withplaceholder$body))
  
  # use Reduce to check whether any of the regular expressions are present in each row of the body column
  subset_withplaceholder$neutralform_present <- Reduce(`|`, neutralform_presence)
  
  # test which reg ex was found:
  subset_withplaceholder$neutralform_found <- NA
  
  # loop through each neutralform in genderwordlist
  for (neutralform in genderwordlist_selected) {
    
    # find which rows of subset_withplaceholder$body contain the current neutralform
    neutralform_presence <- grepl(neutralform, subset_withplaceholder$body)
    
    # store the current neutralform in the neutralform_found column for the rows where the neutralform is present
    subset_withplaceholder$neutralform_found[neutralform_presence] <- neutralform
  }
  
  #create a corpus from the body column and filter for the sentences which entail gendered language
  corpus <- corpus(subset_withplaceholder$body, docnames = subset_withplaceholder$X)
  
  # split corpus into sentences
  corpus_sentences <- corpus_reshape(corpus, to = "sentences")
  
  # get the text of the corpus
  texts_sentences <- texts(corpus_sentences)
  
  # subset to sentences containing the regular expression
  neutralform_texts_sentences <- str_subset(texts_sentences, regex(paste(genderwordlist_selected, collapse = "|")))
  
  # save dataframes as xlsx
  # FAZ
  if (i == 1) {
    # Write the results to a text file
    write.xlsx(neutralform_texts_sentences, "faz_neutralforms_results.xlsx")
  }
  # Spiegel
  if (i == 2) {
    # Write the results to a text file
    write.xlsx(neutralform_texts_sentences, "spiegel_neutralforms_results.xlsx")
  }
  # taz
  if (i == 3) {
    # Write the results to a text file
    write.xlsx(neutralform_texts_sentences, "taz_neutralforms_results.xlsx")
  }
  # Welt
  else {
    # Write the results to a text file
    write.xlsx(neutralform_texts_sentences, "welt_neutralforms_results.xlsx")
  }
  
}
