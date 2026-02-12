# MEDS Hiring Exercise Script
set.seed(1)
library(tidyverse)
library(stringr)

## PART ONE: Wrangling

# QUESTION 1
part1_data <- read.csv("part1_data.csv")
part1_fips <- read.csv("part1_fips.csv")

# merging with fips dataset
part1_data_merged <- merge(part1_data, part1_fips, by.x = "ReportingCountyName", by.y = "county_name")

# identifying smallest election reporting unit aka precincts
# notice: the smallest electoral units contain numbers next to the township name
# we can use this

popular_parties <- c("DEMOCRAT", "REPUBLICAN", "INDEPENDENT", "LIBERTARIAN")

part1_cleaned <- part1_data_merged |>
  select("DataEntryJurisdictionName", "Office", "PoliticalParty", "TotalVotes", "ReportingCountyName", "county_fips", 
         "NameonBallot", "Election") |>
  filter(str_detect(DataEntryJurisdictionName, "\\d") == TRUE) |>
  mutate(Office = recode(Office, 
                         "US President & Vice President" = "US PRESIDENT")) |>
  rename("office" = Office) |>
  rename("party_detailed" = PoliticalParty) |>
  rename("precinct" = DataEntryJurisdictionName) |>
  mutate(party_detailed = recode(party_detailed,
                           "Democratic" = "DEMOCRAT",
                           "Republican" = "REPUBLICAN",
                           "Independent" = "INDEPENDENT",
                           "Libertarian" = "LIBERTARIAN",
                           .default = str_to_upper(party_detailed)
                           )) |>
  mutate(party_simplified = if_else(party_detailed %in% popular_parties, 
                                    party_detailed,
                                    "OTHER")) |>
  rename("votes" = TotalVotes) |>
  mutate(votes = as.numeric(votes)) |>
  rename("county_name" = ReportingCountyName) |> 
  mutate(jurisdiction_name = county_name) |>
  mutate(write_in = str_detect(NameonBallot, "W/I")) |>
  mutate(candidate = str_split(NameonBallot, " & ", simplify = TRUE)[,1]) |>
  mutate(candidate = str_remove(candidate, "\\(W/I\\)")) |>
  mutate(candidate = str_to_upper(candidate)) |>
  mutate(year = "2024") |>
  mutate(stage = "GEN") |>
  mutate(special = FALSE) |>
  mutate(date = as.Date("2024-11-05")) |>
  select(-NameonBallot, -Election)
  

## QUESTION 3

county_totals <- part1_cleaned |>
  group_by(county_name, candidate) |>   
  summarise(total_votes = sum(votes, na.rm = TRUE)) |>
  ungroup()

