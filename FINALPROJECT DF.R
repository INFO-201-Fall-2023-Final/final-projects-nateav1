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
#merge removes Deion Sanders from the dataset who played both football and baseball, unsure how we want to fix

#variable containing number of unique sports listed in merged dataset
#indicates number of sports in which athletes made high earnings
num_listed_sports <- length(unique(df$Sport))

#variable containing the strings of those unique sports
string_listed_sports <- unique(df$Sport)

#for loop to create variables showing how many times a sport was listed in dataset
for(i in 1:num_listed_sports){
  count_string <- sprintf("%s_count", string_listed_sports[i])
  assign(count_string,sum(str_detect(df$Sport, string_listed_sports[i])))
}

#additional column variables

#tougher sport categorical variable, rates whether the athlete's sport was in the top 50 percent of toughness, boolean value
#tougher sport function
is_tougher_sport <- function(name, year){
  if(df[df$Name == name & df$Year == year, "RANK"] <= 30){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
for(i in 1:nrow(df)){
  df_name <- df[i, "Name"]
  df_year <- df[i, "Year"]
  df[i, "Tougher Sport"] <- is_tougher_sport(df_name, df_year)
}

#difference between "rich" rank and "toughness" rank
for(i in 1:nrow(df)){
  df_year_rank <- df[i, "Current.Rank"]
  df_toughness_rank <- df[i, "RANK"]
  df[i, "Difference in Wealth Rank and Toughness Rank"] <- df_year_rank - df_toughness_rank
}

#percentage of list numerical variable, shows the percentage of how much of the list is made of a particular sport
for(i in 1:nrow(df)){
  df_sport <- df[i, "Sport"]
  df[i, "Sport Percentage of List"] <- round(sum(str_detect(df$Sport, df_sport)) / nrow(df) * 100, 0)
}

#summarize athletes full earnings dataframe
grouped_athletes <- group_by(df, Name)
grouped_athletes <- summarize(grouped_athletes, Full_Earnings_Millions = sum(earnings....million.), Mean_Earnings_Millions = mean(earnings....million.))

#summarize sports full earnings
grouped_sports <- group_by(df, Sport)
grouped_sports <- summarize(grouped_sports, Full_Earnings_Millions = sum(earnings....million.), Mean_Earnings_Millions = mean(earnings....million.))
