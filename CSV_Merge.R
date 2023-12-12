library(dplyr)
library(stringr)
library(testthat)

tree_df <- read.csv("Seattle_Tree_Canopy_2016_2021_RSE_Census_Tracts.csv")
income_df <- read.csv("PER_CAPITA_INCOME_and_AGGREGATE_INCOME_IN_THE_PAST_12_MONTHS_(IN_INFLATION-ADJUSTED_DOLLARS).csv")

df <- left_join(income_df, tree_df, join_by(GEOID == GEOID10), na_matches = "never")

colnames(df)[4] <- "Total_Population"
colnames(df)[5] <- "Aggregate_Income"
colnames(df)[6] <- "Per_Capita_Income"

# Data Cleaning
df <- filter(df, !is.na(df$TreeCanopy_2016_Percent))
df <- filter(df, !is.na(df$TreeCanopy_2021_Percent))
df <- filter(df, !is.na(df$Per_Capita_Income))
df <- filter(df, !is.na(df$GEN_ALIAS))

df$maj_TC <- ifelse(df$TC_E_P > 50, TRUE, FALSE)
df$Imperv_TC_Diff <- df$TC_E_P - df$Imperv_P

# Explicitly create the TreeCanopy_Change column
df <- mutate(df, TreeCanopy_Change = df$TreeCanopy_2021_Percent - df$TreeCanopy_2016_Percent)

# Grouping and Summarizing
grouped_df <- group_by(df, TRACT_LABEL, ACS_VINTAGE)
summarized_df <- summarise(
  grouped_df,
  Total_Population = sum(`Total_Population`, na.rm = TRUE),
  Average_Income = mean(`Per_Capita_Income`, na.rm = TRUE),
  Average_Tree_Canopy_Change = mean(TreeCanopy_Change, na.rm = TRUE),
  Major_Tree_Canopy = sum(maj_TC, na.rm = TRUE)
)


write.csv(df, "JoinedDF.csv", row.names=FALSE)

summary_df <- group_by(df, ACS_VINTAGE)
summary_df <- summarise(summary_df, avg = mean(TC_E_P, na.rm = TRUE))
