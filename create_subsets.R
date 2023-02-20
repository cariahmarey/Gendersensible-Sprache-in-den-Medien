# Load the necessary libraries
library(tidyverse)

# Define the search pattern for title, url, description and keywords
search_pattern <- "Gendern|Gendersprache|Gender Sprache|Gender-Sprache|gendergerechte Sprache|gender-gerechte Sprache|
gendersensible Sprache|gender-sensible Sprache|genderneutrale Sprache|gender-neutrale Sprache|
geschlechtergerechte Sprache|geschlechter-gerechte Sprache|geschlechtersensible Sprache|geschlechter-sensible Sprache|
geschlechterneutrale Sprache|geschlechter-neutrale Sprache|generisches Maskulinum|generisches Femininum|Gender-Stern|
Genderstern|Gender-Sternchen|Gendersternchen|grammatisches Geschlecht|Genderlinguistik|Gender-Linguistik|
gender-inklusive Sprache|gender inklusive Sprache|genderinklusive Sprache|geschlechterinklusive Sprache|
geschlechter-inklusive Sprache|geschlechter inlusive Sprache|feministische Sprache|geschlechtsspezifische Sprache|
geschlechterspezifische Sprache|geschlechts-spezifische Sprache|geschlechter-spezifische Sprache|sexistische Sprache|
sexistischer Sprachgebrauch|gendert|genderte|genderten|gegendert|gegenderte|gendernde|gendernd"

#-------------------------import the full datasets
faz_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\faz_full.csv", fileEncoding = "UTF-8")
spiegel_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\spiegel_full.csv")
sueddeutsche_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\sueddeutsche_full.csv")
taz_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\taz_full.csv")
welt_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\welt_full.csv")

# Subset the dataframes to only include rows where the search pattern appears in the columns
faz_subset_gender <- faz_full[apply(faz_full[, c("title", "url", "keywords","description")], 
                                     1, function(x) any(grepl(search_pattern, x, ignore.case = T))), ]
spiegel_subset_gender <- spiegel_full[apply(spiegel_full[, c("title", "url", "keywords","description")], 
                                    1, function(x) any(grepl(search_pattern, x, ignore.case = T))), ]
sueddeutsche_subset_gender <- sueddeutsche_full[apply(sueddeutsche_full[, c("title", "url", "keywords","description")], 
                                    1, function(x) any(grepl(search_pattern, x, ignore.case = T))), ]
taz_subset_gender <- taz_full[apply(taz_full[, c("title", "url", "keywords","description")], 
                                    1, function(x) any(grepl(search_pattern, x, ignore.case = T))), ]
welt_subset_gender <- welt_full[apply(welt_full[, c("title", "url", "keywords","description")], 
                                    1, function(x) any(grepl(search_pattern, x, ignore.case = T))), ]
# export dataframes as csvs
write.csv(faz_subset_gender, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\faz_subset.csv")
write.csv(spiegel_subset_gender, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\spiegel_subset.csv")
write.csv(sueddeutsche_subset_gender, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\sueddeutsche_subset.csv")
write.csv(taz_subset_gender, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\taz_subset.csv")
write.csv(welt_subset_gender, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\welt_subset.csv")



