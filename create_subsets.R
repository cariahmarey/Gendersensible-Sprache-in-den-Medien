# Load the necessary libraries
library(tidyverse)

# Define the search pattern for title, url, description and keywords
# search_pattern <- "Gendern|Gendersprache|Gender Sprache|Gender-Sprache|gendergerechte Sprache|gender-gerechte Sprache|
# gendersensible Sprache|gender-sensible Sprache|genderneutrale Sprache|gender-neutrale Sprache|
# geschlechtergerechte Sprache|geschlechter-gerechte Sprache|geschlechtersensible Sprache|geschlechter-sensible Sprache|
# geschlechterneutrale Sprache|geschlechter-neutrale Sprache|generisches Maskulinum|generisches Femininum|Gender-Stern|
# Genderstern|Gender-Sternchen|Gendersternchen|grammatisches Geschlecht|Genderlinguistik|Gender-Linguistik|
# gender-inklusive Sprache|gender inklusive Sprache|genderinklusive Sprache|geschlechterinklusive Sprache|
# geschlechter-inklusive Sprache|geschlechter inlusive Sprache|feministische Sprache|geschlechtsspezifische Sprache|
# geschlechterspezifische Sprache|geschlechts-spezifische Sprache|geschlechter-spezifische Sprache|sexistische Sprache|
# sexistischer Sprachgebrauch|gendert|genderte|genderten|gegendert|gegenderte|gendernde|gendernd"

# aktuelles search pattern
search_pattern <- "Gendern|Gendersprache|Gender (Sprache|Sprachgebrauch)|Gender-(Sprache|Sprachgebrauch)|
gendergerecht(:?e|er|en|ere) (Sprache|Sprachgebrauch)|gender-gerecht(:?e|er|en|ere) (Sprache|Sprachgebrauch)|
gendersensibl(:?e|er|en|ere) (Sprache|Sprachgebrauch)|gender-sensibl(:?e|er|en|ere) (Sprache|Sprachgebrauch)|
genderneutral(:?e|er|en|ere) (Sprache|Sprachgebrauch)|gender-neutral(:?e|er|en|ere) (Sprache|Sprachgebrauch)|
geschlechtergerecht(:?e|er|en|ere) (Sprache|Sprachgebrauch)|geschlechter-gerecht(:?e|er|en|ere) (Sprache|
Sprachgebrauch)|geschlechtersensible (Sprache|Sprachgebrauch)|geschlechter-sensibl(:?e|er|en|ere) (Sprache|
Sprachgebrauch)|geschlechterneutral(:?e|er|en|ere) (Sprache|Sprachgebrauch)|geschlechter-neutral(:?e|er|en|ere) 
(Sprache|Sprachgebrauch)|generisches Maskulinum|generisches Femininum|Gender-Stern|Genderstern|
Gender-Sternchen|Gendersternchen|grammatisches Geschlecht|Genderlinguistik|Gender-Linguistik|
gender-inklusiv(:?e|er|en|ere) (Sprache|Sprachgebrauch)|gender inklusiv(:?e|er|en|ere) (Sprache|Sprachgebrauch)|
genderinklusiv(:?e|er|en|ere) (Sprache|Sprachgebrauch)|geschlechterinklusive (Sprache|Sprachgebrauch)|
geschlechter-inklusiv(:?e|er|en|ere) (Sprache|Sprachgebrauch)|geschlechter inklusiv(:?e|er|en|ere) (Sprache|Sprachgebrauch)|
feministisch(:?e|er|en|ere) (Sprache|Sprachgebrauch)|geschlechtsspezifisch(:?e|er|en|ere) (Sprache|Sprachgebrauch)|
geschlechterspezifisch(:?e|er|en|ere) (Sprache|Sprachgebrauch)|geschlechts-spezifisch(:?e|er|en|ere) (Sprache|Sprachgebrauch)|
geschlechter-spezifisch(:?e|er|en|ere) (Sprache|Sprachgebrauch)|sexistisch(:?e|er|en|ere) (Sprache|Sprachgebrauch)|
Gegendert(:?e|r|n) (Sprache|Sprachgebrauch)|Gender-Gap|gendert|Gender Gap|Gegendere|gendernd|gegendert|gegenderter|
Binnen-I|Binnen I|Gender:Doppelpunkt|Gender*Stern|Gender_Gap|Gender-Doppelpunkt|Gender Doppelpunkt|
.*(Sprache|Sprachgebrauch)+.*(Gender).*|.*(Gender)+.*(Sprache|Sprachgebrauch).*"

#-------------------------import the full datasets
faz_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\faz_full.csv", fileEncoding = "UTF-8")
spiegel_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\spiegel_full.csv", fileEncoding = "UTF-8")
sueddeutsche_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\sueddeutsche_full.csv", fileEncoding = "UTF-8")
taz_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\taz_full.csv", fileEncoding = "UTF-8")
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

# create subsets without paywall
faz_subset <- subset(faz_subset_gender, paywall == 0)
spiegel_subset <- subset(spiegel_subset_gender, paywall == 0)
sueddeutsche_subset <- subset(sueddeutsche_subset_gender, paywall == 0)
taz_subset <- taz_subset_gender
welt_subset <- subset(welt_subset_gender, paywall == 0)

# put subsets into list
list_subsets <- list(faz_subset, spiegel_subset, sueddeutsche_subset, taz_subset, welt_subset)

# remove rows where date<2020 and body=NA from subsets
for (i in 1:length(list_subsets)) {
  df <- list_subsets[[i]]
  
  # delete rows where date < 2020
  df <- subset(df, as.numeric(substr(date, 1, 4)) >= 2020)
  # delete rows where body=NA
  df <- df[complete.cases(df$body), ]
  
  if (i == 1) {
    assign(paste0("faz_subset"), df)
  }
  if (i == 2) {
    assign(paste0("spiegel_subset"), df)
  }
  if (i == 3) {
    assign(paste0("sueddeutsche_subset"), df)
  }
  if (i == 4) {
    assign(paste0("taz_subset"), df)
  }
  if (i == 5) {
    assign(paste0("welt_subset"), df)
  }
}

# export dataframes as csvs
write.csv(faz_subset_gender, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\faz_subset.csv")
write.csv(spiegel_subset_gender, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\spiegel_subset.csv")
write.csv(sueddeutsche_subset_gender, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\sueddeutsche_subset.csv")
write.csv(taz_subset_gender, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\taz_subset.csv")
write.csv(welt_subset_gender, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\welt_subset.csv")

