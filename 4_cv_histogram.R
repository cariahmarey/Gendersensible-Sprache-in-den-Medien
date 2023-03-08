# load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(xlsx)

# in this script, the number of articles are visualized in a histogram
# with single events regarding gender-language, to see if these events
# are visible through the number of articles
# assumption: an events regarding gender-language happens 
# -> the number of articles increases

#---------- dataframe with single events
# import dataframe with single events from qualitative research
single_events <- read.xlsx("Single Events.xlsx", sheetIndex = 1)

# select only the columns "kurzbeschreibung" und "month"
single_events <- select(single_events, kurzbeschreibung, month)

# remove first two numbers of year for visualization later
single_events$month <- substring(single_events$month, 3)


#---------- number of articles per month
# function to extract and convert dates of subsets to standard format
list_subsets <- lapply(list_subsets, function(x) {
  x$date <- lubridate::ymd(x$date)
  x
})

# apply function to extract and convert dates of subsets to standard format
for (i in seq_along(list_subsets)) {
  # extract the date string using regular expressions and convert it to a Date object
  list_subsets[[i]]$date <- as.Date(sub("T.*", "", list_subsets[[i]]$date), format = "%Y-%m-%d")
  
  list_subsets[[i]]$month <- format(list_subsets[[i]]$date, "%Y-%m")
}

# extract month and count articles per month per newspaper
subsets_months_counts <- lapply(list_subsets, function(x) {
  x %>%
    mutate(month = format(date, "%Y-%m")) %>% # extract month
    count(month) # count articles per month
}) %>% 
  bind_rows(.id = "newspaper") # combine data for all newspapers

# convert month column to date format
subsets_months_counts$month <- as.Date(paste0(subsets_months_counts$month, "-01"), format = "%Y-%m-%d")

# create a sequence of all dates between the minimum and maximum month
all_dates <- seq.Date(from = min(subsets_months_counts$month), to = max(subsets_months_counts$month), by = "month")

# expand the dataframe to include all dates, with n set to 0 for missing dates
subsets_months_counts_expanded <- subsets_months_counts %>%
  complete(month = all_dates) %>%
  replace_na(list(n = 0))

# group the dataframe by month and summarize the counts
subsets_months_counts_combined <- subsets_months_counts_expanded %>%
  group_by(month) %>%
  summarize(n = sum(n)) %>%
  ungroup()

# convert month column back to format useful for visualization
subsets_months_counts_combined$month <- format(subsets_months_counts_combined$month, "%y-%m")

#---------- merge both dataframes
# merge the two dataframes on the "month" column
df_merged_selected <- merge(subsets_months_counts_combined, single_events, by = "month", all.x = TRUE)

# rename the columns to something more meaningful
colnames(df_merged_selected) <- c("month", "count", "kurzbeschreibung")

# convert month column to date format
df_merged_selected$month <- as.Date(paste0("20", df_merged_selected$month, "-01"), format = "%Y-%m-%d")

# create a sequence of all dates between the minimum and maximum month
all_dates <- seq.Date(from = min(df_merged_selected$month), to = max(df_merged_selected$month), by = "month")

# expand the dataframe to include all dates, with counts set to 0 for missing dates
df_expanded <- df_merged_selected %>%
  complete(month = all_dates) %>%
  replace_na(list(count = 0, kurzbeschreibung = ""))

# group the dataframe by month and summarize the counts
df_combined <- df_expanded %>%
  group_by(month) %>%
  summarize(count = sum(count), kurzbeschreibung = first(kurzbeschreibung)) %>%
  ungroup()

# convert month column back to the original format
df_combined$month <- format(df_combined$month, "%y-%m")

# create a new column that indicates whether each row has a kurzbeschreibung value
df_combined$has_kurzbeschreibung <- ifelse(df_combined$kurzbeschreibung != "", TRUE, FALSE)

#------- visualization
# plot the data with ggplot
ggplot(df_combined, aes(x = month, y = count, group = 1)) +
  geom_line(color = "#804674") +
  geom_point(data = df_combined[df_combined$has_kurzbeschreibung,], aes(x = month, y = count), color = "#898AA6", size = 1.5) +
  geom_text(aes(label = kurzbeschreibung), vjust = -1.1, hjust = 1.02, size = 1.8, color = "black") +
  labs(x = "Monat", y = "Anzahl Artikel", title = "Monatliche Artikel und einzelne Events") +
  theme(axis.text.x = element_text(size = 5))

