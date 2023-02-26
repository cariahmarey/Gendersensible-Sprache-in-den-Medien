install.packages("ggplot2")
library(ggplot2)
library(tidyr)
install.packages("wordcloud2")
library(wordcloud2)

#----------------------------#filter2

comparison2<-c(termCountsComparison2/sumalterms_comparison*100000)
faz2<-c(termCountsfaz2/sumalterms_faz*100000)
taz2<-termCountstaz2/sumalterms_taz*100000
spiegel2<-termCountsspiegel2/sumalterms_spiegel*100000
sueddeutsche2<-termCountssueddeutsche2/sumalterms_sueddeutsche*100000
welt2 <-termCountswelt2/sumalterms_welt*100000

#explore a bit:
#str_match_all(taz_corpus,"[A-Z][a-z]*Innen")




#----------------visualization for filter 2 (barchart)


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
       y = "Anzahl/Gessamtmenge*1000") +
  theme_minimal()

#------------------Loglikelihood, Visualisierung filter 2 (einfach table?)

data_listll2 <- list(fazll2, spiegelll2, weltll2, tazll2)
names(data_listll2) <- c("FAZ", "Spiegel", "Welt", "TAZ")

# Get all the unique names across all lists
all_namesll2 <- unique(unlist(lapply(data_listll2, names)))

# Create a data frame with counts for each name in each list
counts_dfll2 <- data.frame(Genderzeichen = all_namesll2,
                         sapply(data_listll2, function(x) {
                           x[all_namesll2]
                         }))


print(counts_dfll2)


#-----------------------visualization Filter3 (Barchart)



comparison3<-termCountsComparison3/sumalterms_comparison*100000
faz3<-termCountsfaz3/sumalterms_faz*100000
taz3<-termCountstaz3/sumalterms_taz*100000
spiegel3<-termCountsspiegel3/sumalterms_spiegel*100000
sueddeutsche3<-termCountssueddeutsche3/sumalterms_sueddeutsche*100000
welt3<-termCountswelt3/sumalterms_welt*100000





# Create a named list with all the data
data_list3 <- list(comparison3, faz3, spiegel3, welt3, taz3)
names(data_list3) <- c("Vergleichskorpus", "FAZ", "Spiegel", "Welt", "TAZ")

df3 <- data.frame(Zeitung = names(data_list3),
                 Doppelnennung = unlist(data_list3))

ggplot(df3, aes(x = Zeitung, y = Doppelnennung, fill = Zeitung)) +
  geom_bar(stat = "identity") +
  labs(x = "Doppelnennung", y = "Anzahl/Gessamtmenge*1000") +
  theme_minimal()
#-------------------------------Loglikelihood filter 3 (einfach table?)

# Create a named list with all the data
data_listll3 <- list(fazll3, spiegelll3, weltll3, tazll3)
names(data_listll3) <- c("FAZ", "Spiegel", "Welt", "TAZ")

dfll3 <- data.frame(Zeitung = names(data_listll3),
                  Doppelnennung = unlist(data_listll3))

print(dfll3)


#------------------------------filter 1 Visualisieurng: noch offen was wir damit machen wollen


#kurze qualitative analyse: welche signifikanten wörter machen sinn?

filtered_listtaz_ll1 <- tazll1[tazll1 > 0] #für ninnvoll befunden: menschen, besuchsperson,studierende, moderation, teilnahmeliste, forschende, lesende, beratende, backende, teilnehmende, redeliste, abteilungsleitung, intendaz
filtered_listfaz_ll1 <- fazll1[fazll1 > 0] #studierende, führungskraft, person, gesetzgebende, versuchsperson, redepult, schreibende
filtered_listspiegel_ll1 <- spiegelll1[spiegelll1 > 0] #menschen, redaktion, leute, erzählende, teamleitung
filtered_listwelt_ll1 <- weltll1[weltll1 > 0] #vorsitzende, studierende, redaktion, menschen, lernende, mitglieder, teilnehmende, lesende, person, lehrende, führungskraft, teamleitung, kundschaft,promovierende, redepult, hauptfigur
filtered_listsueddeutsche_ll1 <- sueddeutschell1[sueddeutschell1 > 0] #person, mitarbeitende, fachkraft, kunstschaffende, klientel, moderation, beteiligte, vorsitzende, freiwillige, lernende, abgeordnete


#filter den vector für diese wörter um damit eine visulaisierung zu bauen
filtered_vec_taz <- filtered_listtaz_ll1[c("menschen", "besuchsperson", "studierende", "moderation", "teilnahmeliste", "forschende", "lesende", "beratende", "backende", "teilnehmende", "redeliste", "abteilungsleitung", "intendanz")]

filtered_vec_faz <- filtered_listfaz_ll1[c("studierende", "führungskraft", "person", "gesetzgebende", "versuchspersonen", "redepult", "schreibende")]

filtered_vec_spiegel <- filtered_listspiegel_ll1[c("menschen", "redaktion", "leute", "erzählende", "teamleitung")]

filtered_vec_welt <- filtered_listwelt_ll1[c("vorsitzende", "studierende", "redaktion", "menschen", "lernende", "mitglieder", "teilnehmende", "lesende", "person", "lehrende", "führungskraft", "teamleitung", "kundschaft", "promovierende", "redepult", "hauptfigur")]

filtered_vec_sueddeutsche <- filtered_listsueddeutsche_ll1[c("person", "mitarbeitende", "fachkraft", "kunstschaffende", "klientel", "moderation", "beteiligte", "vorsitzende", "freiwillige", "lernende", "abgeordnete")]

# z.b wordclouds

df_taz1 <- data.frame(word = names(filtered_vec_taz), count = filtered_vec_taz, row.names = NULL)
wordcloud2(df_taz1, shuffle = F, size = 0.2)

df_faz1 <- data.frame(word = names(filtered_vec_faz), count = filtered_vec_faz, row.names = NULL)
wordcloud2(df_faz1, shuffle = F, size = 0.2)

df_spiegel1 <- data.frame(word = names(filtered_vec_spiegel), count = filtered_vec_spiegel, row.names = NULL)
wordcloud2(df_spiegel1, shuffle = F, size = 0.2)

df_welt1 <- data.frame(word = names(filtered_vec_welt), count = filtered_vec_welt, row.names = NULL)
wordcloud2(df_welt1, shuffle = F, size = 0.2)

df_sueddeutsche1 <- data.frame(word = names(filtered_vec_sueddeutsche), count = filtered_vec_sueddeutsche, row.names = NULL)
wordcloud2(top50_df, shuffle = F, size = 0.2)

#jetzt noch die wordcounts nach den geählten wörtertn filtern
filtered_termCounttaz1 <- termCountstaz1[c("besuchsperson", "studierende", "moderation", "teilnahmeliste", "forschende", "lesende", "beratende", "backende", "teilnehmende", "redeliste", "abteilungsleitung")] #ohne menschen, ohne Intendanz

filtered_termCountfaz1 <- termCountsfaz1[c("studierende", "führungskraft", "gesetzgebende", "versuchspersonen", "redepult", "schreibende")]#ohne person

filtered_termCountspiegel1 <- termCountsspiegel1[c("erzählende", "teamleitung")]#ohne menschen, ohne leute, ohne redaktion

filtered_termCountwelt1 <- termCountswelt1[c("vorsitzende", "studierende", "lernende", "mitglieder", "teilnehmende", "lesende","lehrende", "führungskraft", "teamleitung", "promovierende", "redepult", "hauptfigur")]#ohne "redaktion", "menschen",  "person", "kundschaft", 

filtered_terCountsueddeutsche1 <- termCountssueddeutsche1[c("mitarbeitende", "fachkraft", "kunstschaffende",  "moderation", "beteiligte", "vorsitzende", "lernende")]#ohne "person", "klientel","freiwillige", , "abgeordnete"

#hier also die signifikanten und relevanten terms der jeweiligen zeitungen, geteilt durch die gesamttermzahl, *1000
faz1<-filtered_termCountfaz1/sumalterms_faz*100000
taz1<-filtered_termCounttaz1/sumalterms_taz*100000
spiegel1<-filtered_termCountspiegel1/sumalterms_spiegel*100000
sueddeustche1<-filtered_terCountsueddeutsche1/sumalterms_sueddeutsche*100000
welt1<-filtered_termCountwelt1/sumalterms_welt*100000


