# load the necessary libraries
library(tidyverse)

# in this script the subsets of the datasets are generated 

#---------- import the full datasets
faz_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\faz_full.csv", fileEncoding = "UTF-8")
spiegel_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\spiegel_full.csv", fileEncoding = "UTF-8")
taz_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\taz_full.csv", fileEncoding = "UTF-8")
welt_full <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\welt_full.csv")

#---------- define search pattern
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

#---------- filter
# Subset the dataframes to only include rows where the search pattern appears in the columns "title, url, keywords, description"
faz_subset_gender <- faz_full[apply(faz_full[, c("title", "url", "keywords","description")], 
                                     1, function(x) any(grepl(search_pattern, x, ignore.case = T))), ]
spiegel_subset_gender <- spiegel_full[apply(spiegel_full[, c("title", "url", "keywords","description")], 
                                    1, function(x) any(grepl(search_pattern, x, ignore.case = T))), ]
taz_subset_gender <- taz_full[apply(taz_full[, c("title", "url", "keywords","description")], 
                                    1, function(x) any(grepl(search_pattern, x, ignore.case = T))), ]
welt_subset_gender <- welt_full[apply(welt_full[, c("title", "url", "keywords","description")], 
                                    1, function(x) any(grepl(search_pattern, x, ignore.case = T))), ]

# create subsets without paywall (taz has no paywall)
faz_subset <- subset(faz_subset_gender, paywall == 0)
spiegel_subset <- subset(spiegel_subset_gender, paywall == 0)
welt_subset <- subset(welt_subset_gender, paywall == 0)

# delete rows that contain "NA" in the body
faz_subset <- faz_subset[complete.cases(faz_subset$body), ]
spiegel_subset <- spiegel_subset[complete.cases(spiegel_subset$body), ]
taz_subset <- taz_subset[complete.cases(taz_subset$body), ]
welt_subset <- welt_subset[complete.cases(welt_subset$body), ]

# transform unicode characters in FAZ Subset 
### define a lookup table for all special characters and their corresponding characters
unicode_specialcharacters <- c("<U\\+002D>" = "-", "<U\\+0020>" = " ", "<U\\+0021>" = "!", 
                               "<U\\+0022>" = "\"", "<U\\+0023>" = "#", "<U\\+0024>" = "$", 
                               "<U\\+0025>" = "%", "<U\\+0026>" = "&", "<U\\+0027>" = "'", 
                               "<U\\+0028>" = "(", "<U\\+0029>" = ")", "<U\\+002A>" = "*", 
                               "<U\\+002B>" = "+", "<U\\+002C>" = ",", "<U\\+002E>" = ".", 
                               "<U\\+002F>" = "/", "<U\\+003A>" = ":", "<U\\+003B>" = ";", 
                               "<U\\+003C>" = "<", "<U\\+003D>" = "=", "<U\\+003E>" = ">", 
                               "<U\\+003F>" = "?", "<U\\+0040>" = "@", "<U\\+005B>" = "[", 
                               "<U\\+005C>" = "\\", "<U\\+005D>" = "]", "<U\\+005E>" = "^", 
                               "<U\\+005F>" = "_", "<U\\+0060>" = "`", "<U\\+007B>" = "{", 
                               "<U\\+007C>" = "|", "<U\\+007D>" = "}", "<U\\+007E>" = "~", 
                               "<U\\+2019>" = "'", "<U\\+201A>" = ",", "<U\\+201C>" = "\"", 
                               "<U\\+201D>" = "\"", "<U\\+201E>" = "\"", "<U\\+2030>" = "%", 
                               "<U\\+2039>" = "<", "<U\\+203A>" = ">", "<U\\+2013>" = "–",
                               "<U\\+02BC>" = "ʼ", "<U\\+2018>" = "‘", "<U\\+00A7>" = "§",
                               "<U\\+20AC>" = "€")

### manually replace all German unicode characters and special characters from other languages
faz_subset[] <- lapply(faz_subset, function(x) {
  for (key in names(unicode_specialcharacters)) {
    x <- gsub(key, unicode_specialcharacters[[key]], x)
  }
  x <- gsub("<U\\+00E4>", "ä", x)
  x <- gsub("<U\\+00C4>", "Ä", x)
  x <- gsub("<U\\+00F6>", "ö", x)
  x <- gsub("<U\\+00D6>", "Ö", x)
  x <- gsub("<U\\+00FC>", "ü", x)
  x <- gsub("<U\\+00DC>", "Ü", x)
  x <- gsub("<U\\+00DF>", "ß", x)
  x <- gsub("<U\\+00E9>", "é", x)
  x <- gsub("<U\\+00E8>", "è", x)
  x <- gsub("<U\\+00E1>", "á", x)
  x <- gsub("<U\\+00E7>", "ç", x)
  x <- gsub("<U\\+00C5>", "Å", x)
  x <- gsub("<U\\+00F8>", "ø", x)
  x <- gsub("<U\\+00AD>", "-", x)
  x <- gsub("<U\\+03A9>", "Ω", x)
  x <- gsub("<U\\+012B>", "ī", x)
  return(x)
})

# delete rows from Welt Subset which contain the gibberish language (only articles with a paywall contain this gibberish language)
welt_subset <- welt_subset[!grepl("uuu |elu |vnlpe ", welt_subset$body),]

# delete duplicated rows
faz_subset <- faz_subset[!duplicated(faz_subset),]
spiegel_subset <- spiegel_subset[!duplicated(spiegel_subset),]
taz_subset <- taz_subset[!duplicated(taz_subset),]
welt_subset <- welt_subset[!duplicated(welt_subset),]

# put subsets into list
list_subsets <- list(faz_subset, spiegel_subset, taz_subset, welt_subset)

# remove rows where date <2020 or >2022 and body=NA from subsets
for (i in 1:length(list_subsets)) {
  df <- list_subsets[[i]]
  
  # delete rows where date < 2020
  df <- subset(df, as.numeric(substr(date, 1, 4)) %in% 2020:2022)
  # delete rows where body=NA
  df <- df[complete.cases(df$body), ]
  
  if (i == 1) {
    assign(paste0("faz_subset"), df)
  }
  if (i == 2) {
    assign(paste0("spiegel_subset"), df)
  }
  if (i == 3) {
    assign(paste0("taz_subset"), df)
  }
  if (i == 4) {
    assign(paste0("welt_subset"), df)
  }
}

# put subsets into list
list_subsets <- list(faz_subset, spiegel_subset, taz_subset, welt_subset)

# export dataframes as csvs
write.csv(faz_subset, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\faz_subset.csv")
write.csv(spiegel_subset, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\spiegel_subset.csv")
write.csv(taz_subset, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\taz_subset.csv")
write.csv(welt_subset, "C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\welt_subset.csv")

