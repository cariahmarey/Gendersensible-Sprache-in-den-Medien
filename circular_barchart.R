# Libraries
library(tidyverse)

#this script creates a circular barplot for the used genderlanguage per newspaper for 4 categories
#hier ist die ta genommen, das skript kann mit anderen zeitungen erstezt werden, nur im schritt 1 muss aufgepasst werden
#da sich die reihnfolge der binärer und nicht binärer zeichen ändern kann


# Schritt 1. aufplitten der resultate aus filter 2 in nicht binär und binär:

nicht_binäre_zeichen_taz2 <-(taz2[2]+taz2[3]+taz2[4]+taz2[5]+taz2[6]+taz2[7])
binäre_zeichen_taz2 <-(taz2[1]+taz2[8])


#schritt2: werte zuordnen

id<-c(1,2,3,4)
genderform<-c("Doppelnennung","Neuralisierung", "binäres Zeichen", "nicht-binäres Zeichen")
value <- c(as.numeric(taz3)/10, sum(taz1)/10, as.numeric(binäre_zeichen_taz2)/10, as.numeric(nicht_binäre_zeichen_taz2)/10)

# schritt 3: Create data für verschiedene genderformen
data <- data.frame(
  id= id,
  genderform= genderform,
  value= value
)

# schritt4: ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# schritt 5 Start the plot
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+10, label=genderform, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p