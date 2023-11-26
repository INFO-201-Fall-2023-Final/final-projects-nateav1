library(dplyr)

forbes_richest <- read.csv("Forbes Richest Atheletes (Forbes Richest Athletes 1990-2020) 2.csv")
toughest_sports <- read.csv("toughestsportcorrect.csv")

df <- merge(x = forbes_richest, y = toughest_sports, by = "Sport", all = TRUE)
age <-
df$age = age