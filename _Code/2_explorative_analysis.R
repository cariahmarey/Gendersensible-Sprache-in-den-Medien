# load the necessary libraries
library(ggplot2)
library(quanteda)
options(stringsAsFactors = F)

# in this script an explorative analysis is performed to get a vague overview of the data

#---------- import the subset datasets
faz_subset <- read.csv("Subsets\\faz_subset.csv", fileEncoding = "UTF-8")
spiegel_subset <- read.csv("Subsets\\spiegel_subset.csv", fileEncoding = "UTF-8")
taz_subset <- read.csv("Subsets\\taz_subset.csv", fileEncoding = "UTF-8")
welt_subset <- read.csv("Subsets\\welt_subset.csv", fileEncoding = "UTF-8")

#---------- define the regular expressions to search for in the body
regex_list <- c("\\*in", "\\*innen", ":in", ":innen", "\\(in\\)", "\\(innen\\)", 
                "·in", "·innen", "\\\\_in", "\\\\_innen", "\\/in", "\\/innen", "\\\\/-in", "\\\\/-innen", 
                "[A-Z][a-z]+(In|Innen)")

#---------- barcharts
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
    assign(paste0("barchart_taz"), counts_chart)
  }
  
  else {
    # Create a bar chart with the counts of TRUE and FALSE in the "contains_regex" column
    counts_chart <- ggplot(data.frame(counts_table), aes(x = Var1, y = Freq)) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      labs(x = "Contains Regex", y = "Count") +
      scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
      ggtitle("taz: Counts of Regular Expressions in Body Column")
    # assign name to the bar chart
    assign(paste0("barchart_welt"), counts_chart)
  }

}

