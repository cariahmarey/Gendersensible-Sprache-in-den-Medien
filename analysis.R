library(ggplot2)
library(quanteda)
options(stringsAsFactors = F)

#-------------------------import the subset datasets
faz_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\faz_subset.csv", fileEncoding = "UTF-8")
spiegel_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\spiegel_subset.csv", fileEncoding = "UTF-8")
sueddeutsche_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\sueddeutsche_subset.csv", fileEncoding = "UTF-8")
taz_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\taz_subset.csv", fileEncoding = "UTF-8")
welt_subset <- read.csv("C:\\Users\\mariu\\Documents\\Studium Leipzig\\Master\\Wintersemester 22-23\\Methods & Applications in DH\\Abschlussprojekt\\Datensatz\\Subset\\welt_subset.csv", fileEncoding = "UTF-8")

# create subsets without paywall
faz_subset <- subset(faz_subset, paywall == 0)
spiegel_subset <- subset(spiegel_subset, paywall == 0)
sueddeutsche_subset <- subset(sueddeutsche_subset, paywall == 0)
welt_subset <- subset(welt_subset, paywall == 0)

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

# Define the regular expressions to search for in the body
regex_list <- c("\\*in", "\\*innen", ":in", ":innen", "\\(in\\)", "\\(innen\\)", 
                "·in", "·innen", "\\\\_in", "\\\\_innen", "\\/in", "\\/innen", "\\\\/-in", "\\\\/-innen", 
                "[A-Z][a-z]+(In|Innen)")

#-------------- barcharts

# create barcharts for every subset where we look for the presence of the regexes in the body
for (i in 1:length(list_subsets)) {
  df <- list_subsets[[i]]
  # Use lapply to create a list of logical vectors indicating whether each regex is present in the body column
  regex_presence <- lapply(regex_list, function(regex) grepl(regex, df$body))
  
  # Use Reduce to check whether any of the regular expressions are present in each row of the body column
  df$regex_present <- Reduce(`|`, regex_presence)
  
  # Create a table with the counts of TRUE and FALSE in the  column
  counts_table <- table(df$regex_present)
  
  if (i == 1) {
    # Create a bar chart with the counts of TRUE and FALSE in the "contains_regex" column
    counts_chart <- ggplot(data.frame(counts_table), aes(x = Var1, y = Freq)) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      labs(x = "Contains Regex", y = "Count") +
      scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
      ggtitle("FAZ: Counts of Regular Expressions in Body Column")
    # assign name to the bar chart
    assign(paste0("barchart_faz"), counts_chart)
  }
  
  else if (i == 2) {
    # Create a bar chart with the counts of TRUE and FALSE in the "contains_regex" column
    counts_chart <- ggplot(data.frame(counts_table), aes(x = Var1, y = Freq)) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      labs(x = "Contains Regex", y = "Count") +
      scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
      ggtitle("Spiegel: Counts of Regular Expressions in Body Column")
    # assign name to the bar chart
    assign(paste0("barchart_spiegel"), counts_chart)
  }
  
  else if (i == 3) {
    # Create a bar chart with the counts of TRUE and FALSE in the "contains_regex" column
    counts_chart <- ggplot(data.frame(counts_table), aes(x = Var1, y = Freq)) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      labs(x = "Contains Regex", y = "Count") +
      scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
      ggtitle("Sueddeutsche: Counts of Regular Expressions in Body Column")
    # assign name to the bar chart
    assign(paste0("barchart_sueddeutsche"), counts_chart)
  }
  
  else if (i == 4) {
    # Create a bar chart with the counts of TRUE and FALSE in the "contains_regex" column
    counts_chart <- ggplot(data.frame(counts_table), aes(x = Var1, y = Freq)) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      labs(x = "Contains Regex", y = "Count") +
      scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
      ggtitle("taz: Counts of Regular Expressions in Body Column")
    # assign name to the bar chart
    assign(paste0("barchart_taz"), counts_chart)
  }
  
  else {
    # Create a bar chart with the counts of TRUE and FALSE in the "contains_regex" column
    counts_chart <- ggplot(data.frame(counts_table), aes(x = Var1, y = Freq)) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      labs(x = "Contains Regex", y = "Count") +
      scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
      ggtitle("welt: Counts of Regular Expressions in Body Column")
    # assign name to the bar chart
    assign(paste0("barchart_welt"), counts_chart)
  }
}

#-------------- co-occurrences
# create corpus
welt_corpus <- corpus(welt_subset$body, docnames = welt_subset$X)


# Check if at least one string in the list is in the dataset
if (any(regex_list %in% welt_subset)) {
  
  # Get the preceding and succeeding string
  for (i in seq_along(welt_subset$body)) {
    if (welt_subset$body[i] %in% my_list) {
      if (i > 1) {
        preceding_string <- welt_subset$body[i-1]
      } else {
        preceding_string <- NA
      }
      if (i < length(welt_subset$body)) {
        succeeding_string <- welt_subset$body[i+1]
      } else {
        succeeding_string <- NA
      }
      break
    }
  }
}

welt_subset$body[i]
i

# Print results
print(paste("Found a matching string in the dataset:", welt_subset$body[i]))
print(paste("Preceding string:", preceding_string))
print(paste("Succeeding string:", succeeding_string))
