# cross validation with event timeline

# y: number of articles
# x: month
# + points with single events from qualitative research

library(dplyr)
library(ggplot2)
library(tidyverse)

# Extract and convert dates to standard format
list_subsets <- lapply(list_subsets, function(x) {
  x$date <- lubridate::ymd(x$date) # Convert to standard format
  x
})

# put subsets into list
list_subsets <- list(faz_subset, spiegel_subset, taz_subset, welt_subset)

for (i in seq_along(list_subsets)) {
  # Extract the date string using regular expressions and convert it to a Date object
  list_subsets[[i]]$date <- as.Date(sub("T.*", "", list_subsets[[i]]$date), format = "%Y-%m-%d")
  
  list_subsets[[i]]$month <- format(list_subsets[[i]]$date, "%m")
}

# Extract month and count articles per month per newspaper
df <- lapply(list_subsets, function(x) {
  x %>%
    mutate(month = format(date, "%m")) %>% # Extract month
    count(month) # Count articles per month
}) %>% 
  bind_rows(.id = "newspaper") # Combine data for all newspapers

# Plot histogram with different curves for each newspaper
ggplot(df, aes(x = month, y = n, colour = newspaper)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(x = "Month", y = "Number of articles", color = "Newspaper")


#--------------- test
# Extract month and count articles per month per newspaper
df <- lapply(list_subsets, function(x) {
  x %>%
    mutate(month = format(date, "%m")) %>% # Extract month
    count(month) %>% # Count articles per month
    complete(month = factor(1:12), fill = list(n = 0)) # Ensure there is a row for every month
}) %>% 
  bind_rows(.id = "newspaper") # Combine data for all newspapers

# Convert month to factor and set levels in correct order
df$month <- factor(df$month, levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))

# Plot histogram with different curves for each newspaper
ggplot(df, aes(x = month, y = n, color = newspaper, group = newspaper)) +
  geom_line() +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(x = "Month", y = "Number of articles", color = "Newspaper")
