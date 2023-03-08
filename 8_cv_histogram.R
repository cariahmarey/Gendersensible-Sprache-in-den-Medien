# cross validation with event timeline

# load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(xlsx)

#------- dataframe with single events
# create dataframe with single events from qualitative research
single_events <- read.xlsx("Single Events.xlsx", sheetIndex = 1)

# select only the columns "kurzbeschreibung" und "month"
single_events <- select(single_events, kurzbeschreibung, month)

# remove first two numbers of year for visualization later
single_events$month <- substring(single_events$month, 3)


#------- number of articles per month
# Extract and convert dates of subsets to standard format
list_subsets <- lapply(list_subsets, function(x) {
  x$date <- lubridate::ymd(x$date) # Convert to standard format
  x
})

# put subsets into list
list_subsets <- list(faz_subset, spiegel_subset, taz_subset, welt_subset)

for (i in seq_along(list_subsets)) {
  # Extract the date string using regular expressions and convert it to a Date object
  list_subsets[[i]]$date <- as.Date(sub("T.*", "", list_subsets[[i]]$date), format = "%Y-%m-%d")
  
  list_subsets[[i]]$month <- format(list_subsets[[i]]$date, "%Y-%m")
}

# Extract month and count articles per month per newspaper
subsets_months_counts <- lapply(list_subsets, function(x) {
  x %>%
    mutate(month = format(date, "%Y-%m")) %>% # Extract month
    count(month) # Count articles per month
}) %>% 
  bind_rows(.id = "newspaper") # Combine data for all newspapers

# Convert month column to date format
subsets_months_counts$month <- as.Date(paste0(subsets_months_counts$month, "-01"), format = "%Y-%m-%d")

# Create a sequence of all dates between the minimum and maximum month
all_dates <- seq.Date(from = min(subsets_months_counts$month), to = max(subsets_months_counts$month), by = "month")

# Expand the dataframe to include all dates, with n set to 0 for missing dates
subsets_months_counts_expanded <- subsets_months_counts %>%
  complete(month = all_dates) %>%
  replace_na(list(n = 0))

# Group the dataframe by month and summarize the counts
subsets_months_counts_combined <- subsets_months_counts_expanded %>%
  group_by(month) %>%
  summarize(n = sum(n)) %>%
  ungroup()

# Convert month column back to format useful for visualization
subsets_months_counts_combined$month <- format(subsets_months_counts_combined$month, "%y-%m")

#------- merge both dataframes
# Merge the two dataframes on the "month" column
df_merged_selected <- merge(subsets_months_counts_combined, single_events, by = "month", all.x = TRUE)

# Rename the columns to something more meaningful
colnames(df_merged_selected) <- c("month", "count", "kurzbeschreibung")

# Convert month column to date format
df_merged_selected$month <- as.Date(paste0("20", df_merged_selected$month, "-01"), format = "%Y-%m-%d")

# Create a sequence of all dates between the minimum and maximum month
all_dates <- seq.Date(from = min(df_merged_selected$month), to = max(df_merged_selected$month), by = "month")

# Expand the dataframe to include all dates, with counts set to 0 for missing dates
df_expanded <- df_merged_selected %>%
  complete(month = all_dates) %>%
  replace_na(list(count = 0, kurzbeschreibung = ""))

# Group the dataframe by month and summarize the counts
df_combined <- df_expanded %>%
  group_by(month) %>%
  summarize(count = sum(count), kurzbeschreibung = first(kurzbeschreibung)) %>%
  ungroup()

# Convert month column back to the original format
df_combined$month <- format(df_combined$month, "%y-%m")

# Create a new column that indicates whether each row has a kurzbeschreibung value
df_combined$has_kurzbeschreibung <- ifelse(df_combined$kurzbeschreibung != "", TRUE, FALSE)

#------- visualization
# Plot the data with ggplot
ggplot(df_combined, aes(x = month, y = count, group = 1)) +
  geom_line(color = "#804674") +
  geom_point(data = df_combined[df_combined$has_kurzbeschreibung,], aes(x = month, y = count), color = "#BFD4D9", size = 1.5) +
  geom_text(aes(label = kurzbeschreibung), vjust = -1.1, hjust = -0.015, size = 1.7, color = "black") +
  labs(x = "Month", y = "Count", title = "Monthly Counts and Single Events") +
  theme(legend.title = element_blank())

