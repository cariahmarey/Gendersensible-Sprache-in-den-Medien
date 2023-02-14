# Load the necessary libraries
library(tidyverse)


#-------------------------spiegel


spiegel_df <- read.csv("spiegel_full.csv")

# Define the search pattern
search_pattern <- "Gender|Gendern|Gendersprache|Gender-Sprache|gendergerechte Sprache|gender-gerechte Sprache|
gendersensible Sprache|gender-sensible Sprache|genderneutrale Sprache|gender-neutrale Sprache|
geschlechtergerechte Sprache|geschlechter-gerechte Sprache|geschlechtersensible Sprache|geschlechter-sensible Sprache|
geschlechterneutrale Sprache|geschlechter-neutrale Sprache|generisches Maskulinum|generisches Femininum|Gender-Stern|
Genderstern|Gender-Sternchen|Gendersternchen|grammatisches Geschlecht|Genderlinguistik|Gender-Linguistik|
gender-inklusive Sprache|gender inklusive Sprache|feministische Sprache"

# Subset the faz data frame to only include rows where the search pattern appears columns
spiegel_gender <- spiegel_df[apply(spiegel_df[, c("title", "url", "keywords","description")], 
                        1, function(x) any(grepl(search_pattern, x))), ]

summary(spiegel_gender)

#create subset without paywall
spiegel_gender_free <- subset(spiegel_gender, paywall == 0)


#------find regex


# Define the regular expressions to search for
regex_list <- c("\\*in", "\\*innen", ":in", ":innen", "\\(in\\)", "\\(innen\\)", 
                "·in", "·innen", "\\\\_in", "\\\\_innen", "\\/in", "\\/innen", "\\\\/-in", "\\\\/-innen", 
                "[A-Z][a-z]+(In|Innen)")


# Use lapply to create a list of logical vectors indicating whether each regex is present in the body column
regex_presence <- lapply(regex_list, function(regex) grepl(regex, spiegel_gender_free$body))

# Use Reduce to check whether any of the regular expressions are present in each row of the body column
spiegel_gender_free$regex_present <- Reduce(`|`, regex_presence)

# Create a table with the counts of TRUE and FALSE in the  column
counts_table <- table(spiegel_gender_free$regex_present)

# Create a bar chart with the counts of TRUE and FALSE in the "contains_regex" column
counts_chart <- ggplot(data.frame(counts_table), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  labs(x = "Contains Regex", y = "Count") +
  scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
  ggtitle("Counts of Regular Expressions in Body Column")

# Show the bar chart
counts_chart



#------------------------------------FAZ Data





# Read in the CSV file
faz_df <- read.csv("faz_full.csv")



# Subset the faz data frame to only include rows where the search pattern appears columns
faz_gender <- faz_df[apply(faz_df[, c("title", "url", "keywords","description")], 
                           1, function(x) any(grepl(search_pattern, x))), ]

summary(faz_gender)

#create subset without paywall
faz_gender_free <- subset(faz_gender, paywall == FALSE)


#-----------------------find regex


# Define the regular expressions to search for

# Use lapply to create a list of logical vectors indicating whether each regex is present in the body column
regex_presence <- lapply(regex_list, function(regex) grepl(regex, faz_gender_free$body))

# Use Reduce to check whether any of the regular expressions are present in each row of the body column
faz_gender_free$regex_present <- Reduce(`|`, regex_presence)

# Create a table with the counts of TRUE and FALSE in the  column
counts_table_faz <- table(faz_gender_free$regex_present)

# Create a bar chart with the counts of TRUE and FALSE in the "contains_regex" column
counts_chart_faz <- ggplot(data.frame(counts_table_faz), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  labs(x = "Contains Regex", y = "Count") +
  scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
  ggtitle("Counts of Regular Expressions in Body Column")

# Show the bar chart
counts_chart_faz


