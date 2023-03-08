# load the necessary libraries
library(ggplot2) 
library(tidyr)
library(wordcloud2) 
library(data.table)

# in this script the term frequencies of the filters and the loglikelihood are visualized

#---------- results: filter1

# before visualizing the loglikelihood, we sort the list (> 0)
# and: quick qualitative analysis -> which significant words are meaningful?

filtered_listtaz_ll1 <- tazll1[tazll1 > 0] #meaningful: besuchsperson,studierende, moderation, teilnahmeliste, forschende, lesende, beratende, backende, teilnehmende, redeliste, abteilungsleitung
filtered_listfaz_ll1 <- fazll1[fazll1 > 0] #meaningful: studierende, führungskraft, versuchsperson, schreibende
filtered_listspiegel_ll1 <- spiegelll1[spiegelll1 > 0] #meaningful: redaktion, erzählende, teamleitung
filtered_listwelt_ll1 <- weltll1[weltll1 > 0] #meaningful:studierende, redaktion,teilnehmende,anwesende, lehrkräfte, lernende, mitglieder, lesende, lehrende,befragte, führungskraft, teamleitung, promovierende, redepult, hauptfigur

# filter the vector for these meaningful words
filtered_vec_taz <- filtered_listtaz_ll1[c("besuchsperson", "studierende", "moderation", "teilnahmeliste", "forschende", "lesende", "beratende", "backende", "teilnehmende", "redeliste", "abteilungsleitung")]

filtered_vec_faz <- filtered_listfaz_ll1[c("studierende", "führungskraft", "versuchspersonen", "schreibende")]

filtered_vec_spiegel <- filtered_listspiegel_ll1[c("redaktion", "erzählende", "teamleitung")]

filtered_vec_welt <- filtered_listwelt_ll1[c("studierende", "redaktion","teilnehmende","anwesende", "lehrkräfte", "lernende", "mitglieder", "lesende", "lehrende","befragte", "führungskraft", "teamleitung", "promovierende", "redepult", "hauptfigur")]


#----Loglikelihood, Visualisierung filter 1 (table)

data_listll1 <- list(filtered_vec_faz, filtered_vec_spiegel, filtered_vec_welt, filtered_vec_taz)
names(data_listll1) <- c("FAZ", "Spiegel", "Welt", "TAZ")

# Get all the unique names across all lists
all_namesll1 <- unique(unlist(lapply(data_listll1, names)))

# Create a data frame with counts for each name in each list
counts_dfll1 <- data.frame(Neutralisierung = all_namesll1,
                           sapply(data_listll1, function(x) {
                             x[all_namesll1]
                           }))

print(setDT(counts_dfll1))


#---------------------------- Results: filter2

#als erstes werden die Ergebnisse durch die gesamt Tokens geteilt und dann multipliziert für relative Häufigkeit
comparison2<-termCountsComparison2/sumalterms_comparison*100000
faz2<-termCountsfaz2/sumalterms_faz*100000
taz2<-termCountstaz2/sumalterms_taz*100000
spiegel2<-termCountsspiegel2/sumalterms_spiegel*100000
welt2 <-termCountswelt2/sumalterms_welt*100000

#----- visualization for filter 2 (barchart)

# Create a named list with all the data
data_list2 <- list(comparison2, faz2, spiegel2, welt2, taz2)
names(data_list2) <- c("Vergleichskorpora", "FAZ", "Spiegel", "Welt", "TAZ")

# Get all the unique names across all lists
all_names2 <- unique(unlist(lapply(data_list2, names)))

# Create a data frame with counts for each name in each list
counts_df2 <- data.frame(Name = all_names2,
                        sapply(data_list2, function(x) {
                          x[all_names2]
                        }))

# Reshape the data frame into long format
counts_df_long2 <- gather(counts_df2, key = "Zeitung", value = "Count", -Name)

# Create the bar chart
ggplot(counts_df_long2, aes(x = Name, y = Count, fill = Zeitung)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Nutzung verschiedener Genderzeichen in deutschen Tageszeitungen",
       x = "Genderzeichen",
       y = "Anzahl/Gessamtmenge*100.000") +
  theme_minimal()

#------------------Loglikelihood, Visualisierung filter 2 (table)

data_listll2 <- list(fazll2, spiegelll2, weltll2, tazll2)
names(data_listll2) <- c("FAZ", "Spiegel", "Welt", "TAZ")

# Get all the unique names across all lists
all_namesll2 <- unique(unlist(lapply(data_listll2, names)))

# Create a data frame with counts for each name in each list
counts_dfll2 <- data.frame(Genderzeichen = all_namesll2,
                         sapply(data_listll2, function(x) {
                           x[all_namesll2]
                         }))

print(setDT(counts_dfll2))

#-----------------------visualization Filter3 (Barchart)

comparison3<-termCountsComparison3/sumalterms_comparison*100000
faz3<-termCountsfaz3/sumalterms_faz*100000
taz3<-termCountstaz3/sumalterms_taz*100000
spiegel3<-termCountsspiegel3/sumalterms_spiegel*100000
welt3<-termCountswelt3/sumalterms_welt*100000

# Create a named list with all the data
data_list3 <- list(comparison3, faz3, spiegel3, welt3, taz3)
names(data_list3) <- c("Vergleichskorpus", "FAZ", "Spiegel", "Welt", "TAZ")

df3 <- data.frame(Zeitung = names(data_list3),
                 Doppelnennung = unlist(data_list3))

ggplot(df3, aes(x = Zeitung, y = Doppelnennung, fill = Zeitung)) +
  geom_bar(stat = "identity") +
  labs(x = "Doppelnennung", y = "Anzahl/Gessamtmenge*100.000") +
  theme_minimal()

#-------------------------------Loglikelihood filter 3 (einfach table?)

# Create a named list with all the data
data_listll3 <- list(fazll3, spiegelll3, weltll3, tazll3)
names(data_listll3) <- c("FAZ", "Spiegel", "Welt", "TAZ")

dfll3 <- data.frame(Zeitung = names(data_listll3),
                  Doppelnennung = unlist(data_listll3))

print(setDT(dfll3))


#------------------------------- visualize wordcounts: 
# quick qualitative analysis -> which significant words make sense?

filtered_termCounttaz1 <- termCountstaz1[c("arbeitende", "besuchsperson", "fachkraft", "erwerbstätige", "studierende", "betroffene","leserschaft", "pflegekraft", "zielgruppe", "handelnde", "geschäftsleitung", "moderation", "teilnahmeliste", "forschende","angestellte", "lesende", "hauptfigur", "beratende","mitglieder", "backende","mitarbeitende", "teilnehmende", "redeliste","assistenz", "abteilungsleitung","auszubildende","sprechende")] 

filtered_termCountfaz1 <- termCountsfaz1[c("studierende","mitglied","mitglieder", "führungskraft","zielgruppe","geschäftsführung","fachkraft","vorstandsmitglied","mitarbeitende","teilnehmende", "gesetzgebende", "versuchspersonen","angehörige","personal","schreibende","belegschaft", "schulleitung","pflegepersonal", "redepult", "fachkräfte","fachleute","testpersonen","moderation","befragte","eheleute","pflegende", "schreibende")]

filtered_termCountspiegel1 <- termCountsspiegel1[c("lehrende", "leitung", "moderierende", "studierende", "mitglieder", "teamleitung", "lehrkräfte", "mitglied", "pflegekraft", "beschäftigte", "forschende", "führungskraft","teamleitung")]

filtered_termCountwelt1 <- termCountswelt1[c("studierende", "teilnehmende", "leitung", "anwesende", "lehrkräfte", "lernende",  "studierendenwerk", "mitglieder", "fachkräfte", "zielgruppe",  "lesende", "mitarbeitende",  "stadtoberhaupt", "versuchspersonen", "mitglied", "lehrende", "befragte", "elternteil", "führungskraft", "vorstand", "leserschaft",  "lehrkraft", "teamleitung", "betroffene",  "belegschaft", "redeliste", "personal", "beschäftigte", "führung", "fachleute", "teilnahmeliste", "pflegekraft", "vorsitz", "arbeitskraft", "promovierende", "ehrenamtliche", "feuerwehrleute", "redepult", "moderation", "pflegekräfte", "hauptfigur")]

#dann die relevanten terms der jeweiligen zeitungen realtivieren mit: geteilt durch die gesamttermzahl, *100000
faz1<-filtered_termCountfaz1/sumalterms_faz*100000
taz1<-filtered_termCounttaz1/sumalterms_taz*100000
spiegel1<-filtered_termCountspiegel1/sumalterms_spiegel*100000
welt1<-filtered_termCountwelt1/sumalterms_welt*100000

# jetzt Visualisierung: z.b wordclouds
df_taz1 <- data.frame(word = names(taz1), count = taz1, row.names = NULL)
wordcloud2(df_taz1, shuffle = F, size = 0.2)

df_faz1 <- data.frame(word = names(faz1), count = faz1, row.names = NULL)
wordcloud2(df_faz1, shuffle = F, size = 0.2)

df_spiegel1 <- data.frame(word = names(spiegel1), count = spiegel1, row.names = NULL)
wordcloud2(df_spiegel1, shuffle = F, size = 0.2)

df_welt1 <- data.frame(word = names(welt1), count = welt1, row.names = NULL)
wordcloud2(df_welt1, shuffle = F, size = 0.2)
