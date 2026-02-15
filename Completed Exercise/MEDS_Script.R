# MEDS Hiring Exercise Script - COMPLETED
set.seed(1)
library(tidyverse)
library(stringr)
library(gt)

## PART ONE: Wrangling

# QUESTION 1
part1_data <- read.csv("part1_data.csv")
part1_fips <- read.csv("part1_fips.csv")

# merging with fips dataset
part1_data_merged <- merge(part1_data, part1_fips, by.x = "ReportingCountyName", by.y = "county_name")

# identifying smallest election reporting unit aka precincts
# notice: the smallest electoral units contain numbers next to the township name
# we can use this to our advantage

# vector of popular population parties, used when creating the party variables
popular_parties <- c("DEMOCRAT", "REPUBLICAN", "INDEPENDENT", "LIBERTARIAN")

# cleaned data set
part1_cleaned <- part1_data_merged |>
  select("DataEntryJurisdictionName", "Office", "PoliticalParty", "TotalVotes", "ReportingCountyName", "county_fips", 
         "NameonBallot", "Election") |> # selecting necessary columns from original dataset
  filter(str_detect(DataEntryJurisdictionName, "\\d") == TRUE) |> # identifying the precincts and disregarding the aggregated township sums
  mutate(Office = recode(Office, 
                         "US President & Vice President" = "US PRESIDENT")) |> # notice: all of the office values are for US President, no need to recode multiple times
  rename("office" = Office) |> # renaming using naming convention
  rename("party_detailed" = PoliticalParty) |> # renaming using naming convention
  rename("precinct" = DataEntryJurisdictionName) |> # renaming using naming convention
  mutate(party_detailed = recode(party_detailed, 
                           "Democratic" = "DEMOCRAT",
                           .default = str_to_upper(party_detailed)
                           )) |> # this code is changing the party values for naming convention, capitalizing everything
  mutate(party_simplified = if_else(party_detailed %in% popular_parties, 
                                    party_detailed,
                                    "OTHER")) |> # using the same naming convention for popular parties, otherwise using OTHER. there are no NA values in this dataset
  rename("votes" = TotalVotes) |> # renaming using naming convention
  mutate(votes = as.numeric(votes)) |> # changing the votes variable to a numeric value
  rename("county_name" = ReportingCountyName) |>  # renaming using naming convention
  mutate(jurisdiction_name = county_name) |> # in the state of indiana, the jurisdiction name is the county name - no need to use a different name
  mutate(write_in = str_detect(NameonBallot, "W/I")) |> # counting the instances of the (W/I) string bit in the candidate variable of each observation
  mutate(candidate = str_split(NameonBallot, " & ", simplify = TRUE)[,1]) |> # removing the vice president candidates from the ballot, only keeping the presidential candidate (per codebook instructions)
  mutate(candidate = str_remove(candidate, "\\(W/I\\)")) |> # removing the (W/I) string bit from the presidential candidate string (we have no more of a need for it)
  mutate(candidate = str_to_upper(candidate)) |> # renaming using naming convention
  mutate(year = "2024") |> # every observation in the dataset is the 2024 general elections, no need for additional code in this instance
  mutate(stage = "GEN") |> # see: above
  mutate(special = FALSE) |> # see: above
  mutate(state = "INDIANA") |> # indiana election data
  mutate(date = as.Date("2024-11-05")) |> # see: above
  select(-NameonBallot, -Election) # removing unnecessary elements from the dataframe
  
# re-ordering columns by codebook order
part1_cleaned <- part1_cleaned |>
  select(precinct, office, party_detailed, party_simplified, votes, county_name, county_fips, jurisdiction_name, candidate, year, stage, special, write_in, date)

head(part1_cleaned)

## QUESTION 3

county_totals <- part1_cleaned |>
  group_by(county_name, candidate) |>   
  summarise(total_votes = sum(votes, na.rm = TRUE)) |>
  ungroup()

