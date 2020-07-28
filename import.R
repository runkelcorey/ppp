########################
#Importing PPP loan data
#Corey Runkel
#2020-07-06
########################

#libraries
library(tidyverse)
library(lubridate)
library(vroom)

#import loan data
##small loans
under_names <- paste("data/", list.files(path = "data/", pattern = "^foia_up_to_150k_", recursive = TRUE), sep = "") #list files smaller than $150k
under <- vroom(file = under_names[1], col_types = c("ncfcfffffcnfcc")) #create initial tibble
for (i in seq_along(under_names[2:57])) { #append the rest
  under <- bind_rows(under, vroom(file = under_names[i], col_types = c("ncfcfffffcnfcc")))
}
##aggregate loans
all_loans <- bind_rows(under, vroom(file = "data/150k plus/foia_150k_plus.csv", col_types = c("ccccfcffffffnccc"))) %>% #quickly import loans larger than $150,000
  mutate(DateApproved = mdy(DateApproved), #convert DateApproved to date format
         NonProfit = ifelse(is.na(NonProfit), FALSE, TRUE), #convert non-profit to type logical
         LoanRange = ifelse(is.na(LoanRange), "", as.character(factor(LoanRange, levels = c("e $150,000-350,000", "d $350,000-1 million", "c $1-2 million", "b $2-5 million", "a $5-10 million"), labels = c("$150k-$350k", "$350k-$1m", "$1m-$2m", "$2m-$5m", "$5m-$10m")))), #create factor from character
         LoanRange2 = ifelse(is.na(LoanAmount), "", as.character(cut(LoanAmount, breaks = c(-Inf, 10000, 50000, 100000, 150000), labels = c("$0-$10k", "$10k-$50k", "$50k-$100k", "$100k-$150k"), right = TRUE)))) %>% #create factor from continuous variable
  unite(col = LoanRange, LoanRange, LoanRange2, sep = "") %>% #unify variables
  separate(Lender, c("Lender", NA), sep = " d/b/a", remove = TRUE) %>% #remove anything after d/b/a in lender name
  mutate(Lender = str_remove_all(Lender, ", National Association")) %>% #remove national association
  mutate(NAICS2DIGIT = substr(NAICSCode, 1, 2), Industry = fct_collapse(substr(NAICSCode, 1, 2), #create sensible NAICS codes
                                                                      "Agriculture" = "11",
                                                                      "Mineral extraction" = "21",
                                                                      "Utilities" = "22",
                                                                      "Construction" = "23",
                                                                      "Manufacturing" = "31",
                                                                      "Manufacturing" = "32",
                                                                      "Manufacturing" = "33",
                                                                      "Wholesale trade" = "42",
                                                                      "Retail" = "44",
                                                                      "Retail" = "45",
                                                                      "Logistics" = "48",
                                                                      "Logistics" = "49",
                                                                      "Information" = "51",
                                                                      "Finance & insurance" = "52",
                                                                      "Real estate" = "53",
                                                                      "Professional services, research, & IT" = "54",
                                                                      "Management" = "55",
                                                                      "Administrative support" = "56",
                                                                      "Education" = "61",
                                                                      "Healthcare" = "62",
                                                                      "Entertainment & recreation" = "71",
                                                                      "Restaurants & hospitality" = "72",
                                                                      "Public administration" = "92",
                                                                      other_level = "Other"))

#state geometries (shifted) and population
states <- get_acs("state", variables = "B01003_001", geometry = TRUE, shift_geo = TRUE) %>% #get ACS population estimates and shifted geometry for each state
  inner_join(distinct(select(rename(fips_codes, NAME = state_name, State = state), NAME, State))) %>% #make joinable with all_loans
  select(State, NAME, estimate) %>%
  rename(StateName = NAME, Population = estimate) %>%
  st_as_sf() #reinstate sf designation (joining reverts it to a tibble)

#nonemployers
nonemp <- vroom("data/nonemp18st.txt") %>%
  filter(nchar(NAICS) == 2, LFO == "-") %>%
  rename(state_code = ST, Nonemployers = ESTAB) %>%
  mutate(NAICS = str_replace_all(NAICS, "00", "--")) %>%
  inner_join(tigris::fips_codes) %>%
  rename(State = state) %>%
  select(State, NAICS, Nonemployers) %>%
  distinct()

#join sector employment to state
temp <- tempfile()
download.file("https://www2.census.gov/programs-surveys/susb/tables/2017/us_state_naics_detailedsizes_2017.txt", destfile = temp) #get NAICS codes and employment figures from Census Bureau
naics <- read_csv(temp) %>%
  filter(ENTRSIZE == "19", STATEDSCR != "United States") %>% #only totals
  select(STATEDSCR, NAICS, FIRM, EMPL_N, PAYR_N) %>% #shrink file size
  rename(StateName = STATEDSCR, Firms = FIRM, Employees = EMPL_N, PayrollSize = PAYR_N) %>%
  mutate(NAICS = substr(NAICS, 1, 2), Industry = fct_collapse(substr(NAICS, 1, 2),
                                                                    "Agriculture" = "11",
                                                                    "Mineral extraction" = "21",
                                                                    "Utilities" = "22",
                                                                    "Construction" = "23",
                                                                    "Manufacturing" = "31",
                                                                    "Manufacturing" = "32",
                                                                    "Manufacturing" = "33",
                                                                    "Wholesale trade" = "42",
                                                                    "Retail" = "44",
                                                                    "Retail" = "45",
                                                                    "Logistics" = "48",
                                                                    "Logistics" = "49",
                                                                    "Information" = "51",
                                                                    "Finance & insurance" = "52",
                                                                    "Real estate" = "53",
                                                                    "Professional services, research, & IT" = "54",
                                                                    "Management" = "55",
                                                                    "Administrative support" = "56",
                                                                    "Education" = "61",
                                                                    "Healthcare" = "62",
                                                                    "Entertainment & recreation" = "71",
                                                                    "Restaurants & hospitality" = "72",
                                                                    "Public administration" = "92",
                                                                    other_level = "Other")) %>% #take 2-digit NAICS code
  inner_join(states) %>%
  left_join(nonemp) %>%
  mutate(Businesses = Firms + ifelse(is.na(Nonemployers), 0, Nonemployers))