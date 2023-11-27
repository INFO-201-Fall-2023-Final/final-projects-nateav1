#loading libraries
library(dplyr)
library(stringr)

#load in datasets
forbes_richest <- read.csv("Forbes Richest Atheletes (Forbes Richest Athletes 1990-2020) 2.csv")
toughest_sports <- read.csv("toughestsportcorrect.csv")

#cleaning up dataset for merge
forbes_richest[forbes_richest == "NFL"] <- "Football"
forbes_richest[forbes_richest == "American Football"] <- "Football"
forbes_richest[forbes_richest == "NBA"] <- "Basketball"
forbes_richest[forbes_richest == "F1 Motorsports"] <- "Auto Racing"
forbes_richest[forbes_richest == "F1 racing"] <- "Auto Racing"
forbes_richest[forbes_richest == "NASCAR"] <- "Auto Racing"
forbes_richest[forbes_richest == "Auto Racing (Nascar)"] <- "Auto Racing"
forbes_richest[forbes_richest == "motorcycle gp"] <- "Auto Racing"
forbes_richest[forbes_richest == "MMA"] <- "Martial Arts"
forbes_richest[forbes_richest == "Baseball"] <- "Baseball/Softball"
forbes_richest[forbes_richest == "baseball"] <- "Baseball/Softball"
forbes_richest$Sport <- str_to_title(forbes_richest$Sport)

#merge datasets
df <- merge(x = forbes_richest, y = toughest_sports, by = "Sport", all_x = TRUE)

#insert variables below
#age <-
#df$age = age