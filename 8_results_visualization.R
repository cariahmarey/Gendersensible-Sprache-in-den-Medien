# load the necessary libraries
library(ggplot2) 
library(tidyr)
library(wordcloud2) 
library(data.table)
library(xlsx)

# in this script the term frequencies of the filters and the loglikelihood are visualized

#---------- results: filter 1

# before visualizing the loglikelihood, we sort the list (> 0)
# and: qualitative analysis -> look for meaningful words

filtered_list_taz_ll1 <- tazll1[tazll1 > 0] #meaningful: besuchsperson,studierende, moderation, teilnahmeliste, forschende, lesende, beratende, backende, teilnehmende, redeliste, abteilungsleitung
filtered_list_faz_ll1 <- fazll1[fazll1 > 0] #meaningful: studierende, führungskraft, versuchspersonen, schreibende
filtered_list_spiegel_ll1 <- spiegelll1[spiegelll1 > 0] #meaningful: erzählende, teamleitung
filtered_list_welt_ll1 <- weltll1[weltll1 > 0] #meaningful: studierende, teilnehmende, anwesende, lehrkräfte, lernende, vorsitzende, mitglieder, lesende, lehrende, führungskraft, teamleitung, kundschaft, promovierende, redepult, hauptfigur

# filter the vector for these meaningful words
filtered_vec_taz <- filtered_list_taz_ll1[c("besuchsperson", "studierende", "moderation", "teilnahmeliste", "forschende", "lesende", "beratende", "backende", "teilnehmende", "redeliste", "abteilungsleitung")]

filtered_vec_faz <- filtered_list_faz_ll1[c("studierende", "führungskraft", "versuchspersonen", "schreibende")]

filtered_vec_spiegel <- filtered_list_spiegel_ll1[c("erzählende", "teamleitung")]

filtered_vec_welt <- filtered_list_welt_ll1[c("studierende", "teilnehmende","anwesende", "lehrkräfte", "lernende", "vorsitzende", "mitglieder", "lesende", "lehrende", "führungskraft", "teamleitung", "kundschaft", "promovierende", "redepult", "hauptfigur")]


#----- loglikelihood, visualization of filter 1 (table)

data_list_ll1 <- list(filtered_vec_faz, filtered_vec_spiegel, filtered_vec_welt, filtered_vec_taz)
names(data_listll1) <- c("FAZ", "Spiegel", "Welt", "TAZ")

# Get all the unique names across all lists
all_names_ll1 <- unique(unlist(lapply(data_list_ll1, names)))

# Create a data frame with counts for each name in each list
counts_df_ll1 <- data.frame(Neutralisierung = all_names_ll1,
                           sapply(data_list_ll1, function(x) {
                             x[all_names_ll1]
                           }))

# save table as xlsx
write.xlsx(counts_df_ll1, "counts_loglikelihood_filter1.xlsx")


#---------- results: filter 2

#----- first: divide results by total amount of tokens and then multiply to get relative frequency
comparison2<-termCountsComparison2/sum_allterms_comparison*100000
faz2<-termCountsfaz2/sum_allterms_faz*100000
taz2<-termCountstaz2/sum_allterms_taz*100000
spiegel2<-termCountsspiegel2/sum_allterms_spiegel*100000
welt2 <-termCountswelt2/sum_allterms_welt*100000

#----- visualization for filter 2 (barchart)

# create a named list with all the data
data_list2 <- list(comparison2, faz2, spiegel2, welt2, taz2)
names(data_list2) <- c("Vergleichskorpus", "FAZ", "Spiegel", "Welt", "TAZ")

# get all the unique names across all lists
all_names2 <- unique(unlist(lapply(data_list2, names)))

# create a data frame with counts for each name in each list
counts_df2 <- data.frame(Name = all_names2,
                        sapply(data_list2, function(x) {
                          x[all_names2]
                        }))

# Reshape the data frame into long format
counts_df_ll2 <- gather(counts_df2, key = "Zeitung", value = "Count", -Name)

# Create the bar chart
ggplot(counts_df_ll2, aes(x = Name, y = Count, fill = Zeitung)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Verwendung verschiedene Genderformen in deutschen Tageszeitungen",
       x = "Genderform",
       y = "Anzahl/Gesamtmenge*100.000") +
  theme_minimal()

#----- visualization for filter 2 (table)

write.xlsx(setDT(counts_df_ll2), "counts_loglikelihood_filter2.xlsx")


#---------- results: filter 3
#----- first: divide results by total amount of tokens and then multiply to get relative frequency
comparison3<-termCountsComparison3/sum_allterms_comparison*100000
faz3<-termCountsfaz3/sum_allterms_faz*100000
taz3<-termCountstaz3/sum_allterms_taz*100000
spiegel3<-termCountsspiegel3/sum_allterms_spiegel*100000
welt3<-termCountswelt3/sum_allterms_welt*100000

#----- visualization for filter 3 (barchart)
# Create a named list with all the data
data_list3 <- list(comparison3, faz3, spiegel3, welt3, taz3)
names(data_list3) <- c("Vergleichskorpus", "FAZ", "Spiegel", "Welt", "TAZ")

counts_df_ll3 <- data.frame(Zeitung = names(data_list3),
                 Doppelnennung = unlist(data_list3))

ggplot(counts_df_ll3, aes(x = Zeitung, y = Doppelnennung, fill = Zeitung)) +
  geom_bar(stat = "identity") +
  labs(x = "Doppelnennung", y = "Anzahl/Gessamtmenge*100.000") +
  theme_minimal()

#----- loglikelihood filter 3

write.xlsx(setDT(counts_df_ll3), "counts_loglikelihood_filter3.xlsx")


#---------- visualize wordcounts: 
# count which words of the gendered_words_splitted appear with what frequency in the subsets

# qualitative analysis -> look for meaningful words
filtered_termCounttaz1 <- termCountstaz1[c("vorsitzende", "arbeitende", "besuchsperson", 
                                           "fachkraft", "erwerbstätige", "studierende", 
                                           "betroffene", "leserschaft", "pflegekraft", 
                                           "handelnde", "geschäftsleitung", "moderation", 
                                           "teilnahmeliste", "forschende", "angestellte", 
                                           "führungskraft", "testpersonen", "lesende", 
                                           "hauptfigur", "beratende", "mitglieder", 
                                           "mitglied", "backende", "mitarbeitende", 
                                           "teilnehmende", "redeliste","assistenz", 
                                           "abteilungsleitung", "intendanz", "auszubildende", 
                                           "sprechende")] 
filtered_termCountfaz1 <- termCountsfaz1[c("studierende", "mitglied", "mitglieder", 
                                           "vorsitzende", "führungskraft", "zielgruppe", 
                                           "geschäftsführung", "fachkraft", "vorstandsmitglied", 
                                           "mitarbeitende", "teilnehmende", "gesetzgebende", 
                                           "versuchspersonen", "angehörige", "personal", 
                                           "schreibende", "belegschaft", "schulleitung", 
                                           "pflegepersonal", "redepult", "fachkräfte", 
                                           "fachleute", "testpersonen", "moderation", 
                                           "befragte", "eheleute", "pflegende", "schreibende")]
filtered_termCountspiegel1 <- termCountsspiegel1[c("lehrende", "leitung", "moderierende", 
                                                   "studierende", "mitglieder", "teamleitung", 
                                                   "lehrkräfte", "mitglied", "pflegekraft", 
                                                   "beschäftigte", "forschende", "führungskraft",
                                                   "teamleitung")]
filtered_termCountwelt1 <- termCountswelt1[c("studierende", "teilnehmende", "leitung", 
                                             "anwesende", "lehrkräfte", "lernende",  
                                             "studierendenwerk", "mitglieder", "fachkräfte", 
                                             "zielgruppe",  "lesende", "mitarbeitende",  
                                             "stadtoberhaupt", "versuchspersonen", "mitglied", 
                                             "lehrende", "befragte", "elternteil", "führungskraft", 
                                             "vorstand", "leserschaft",  "lehrkraft", "teamleitung", 
                                             "betroffene",  "belegschaft", "redeliste", "personal", 
                                             "beschäftigte", "führung", "fachleute", "teilnahmeliste", 
                                             "pflegekraft", "vorsitz", "arbeitskraft", "promovierende", 
                                             "ehrenamtliche", "feuerwehrleute", "redepult", "moderation", 
                                             "pflegekräfte", "hauptfigur")]

# divide results by total amount of tokens and then multiply to get relative frequency
faz1<-filtered_termCountfaz1/sum_allterms_faz*100000
taz1<-filtered_termCounttaz1/sum_allterms_taz*100000
spiegel1<-filtered_termCountspiegel1/sum_allterms_spiegel*100000
welt1<-filtered_termCountwelt1/sum_allterms_welt*100000

# visualization: wordclouds
df_taz1 <- data.frame(word = names(taz1), count = taz1, row.names = NULL)
wordcloud2(df_taz1, shuffle = F, size = 0.5)

df_faz1 <- data.frame(word = names(faz1), count = faz1, row.names = NULL)
wordcloud2(df_faz1, shuffle = F, size = 0.5)

df_spiegel1 <- data.frame(word = names(spiegel1), count = spiegel1, row.names = NULL)
wordcloud2(df_spiegel1, shuffle = F, size = 0.5)

df_welt1 <- data.frame(word = names(welt1), count = welt1, row.names = NULL)
wordcloud2(df_welt1, shuffle = F, size = 0.5)
