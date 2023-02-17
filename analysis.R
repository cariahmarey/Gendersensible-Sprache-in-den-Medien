#-------------------------import the subset datasets
faz_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\faz_subset.csv", fileEncoding = "UTF-8")
spiegel_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\spiegel_subset.csv", fileEncoding = "UTF-8")
sueddeutsche_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\sueddeutsche_subset.csv", fileEncoding = "UTF-8")
taz_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\taz_subset.csv", fileEncoding = "UTF-8")
welt_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\welt_subset.csv", fileEncoding = "UTF-8")

#create subsets without paywall
faz_subset <- subset(faz_subset, paywall == 0)
spiegel_subset <- subset(spiegel_subset, paywall == 0)
sueddeutsche_subset <- subset(sueddeutsche_subset, paywall == 0)
welt_subset <- subset(welt_subset, paywall == 0)

# Define the regular expressions to search formthe Body
regex_list <- c("\\*in", "\\*innen", ":in", ":innen", "\\(in\\)", "\\(innen\\)", 
                "·in", "·innen", "\\\\_in", "\\\\_innen", "\\/in", "\\/innen", "\\\\/-in", "\\\\/-innen", 
                "[A-Z][a-z]+(In|Innen)")

