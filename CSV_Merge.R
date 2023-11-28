library(dplyr)
library(stringr)
library(testthat)

tree_df <- read.csv("Seattle_Tree_Canopy_2016_2021_RSE_Census_Tracts.csv")
income_df <- read.csv("PER_CAPITA_INCOME_and_AGGREGATE_INCOME_IN_THE_PAST_12_MONTHS_(IN_INFLATION-ADJUSTED_DOLLARS).csv")

df <- left_join(income_df, tree_df, join_by(GEOID == GEOID10), na_matches = "never")

colnames(df)[4] <- "Total Population"
colnames(df)[5] <- "Aggregate Income"
colnames(df)[6] <- "Per Capita Income"

df$maj_TC <- ifelse(df$TC_E_P > 50, TRUE, FALSE)
df$Imperv_TC_Diff <- df$TC_E_P - df$Imperv_P

write.csv(df, "JoinedDF.csv", row.names=FALSE)

summary_df <- group_by(df, ACS_VINTAGE)
summary_df <- summarise(summary_df, avg = mean(TC_E_P, na.rm = TRUE))