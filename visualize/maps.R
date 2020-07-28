##########################
#Visualizing PPP loan data
#Maps
#Corey Runkel
#2020-07-16
##########################

#libraries
library(tidyverse)
library(lubridate)
library(sf)
library(tidycensus)
library(tmap)

#pre-processing: requires all_loans dataset created from import.R script
all_loans <- read_csv("data/all_ppp_loans.csv", col_types = c("nccccfffflnDccfccnf")) %>%
  mutate(LoanRange = factor(LoanRange, levels = c("$0-$10k", "$10k-$50k", "$50k-$100k", "$100k-$150k", "$150k-$350k", "$350k-$1m", "$1m-$2m", "$2m-$5m", "$5m-$10m"), ordered = TRUE)) #re-create factor levels

##animated map of per capita large loan approvals over time: percap-map (perbiz is a simple variant)
p <- all_loans %>%
  group_by(State, DateApproved) %>%
  summarize(Loans = n()) %>% #count loans approved each day within each state
  ungroup() %>%
  group_by(State) %>%
  arrange(DateApproved) %>% #arrange daily counts by date within each state
  mutate(Loans = cumsum(Loans)) %>% #cumulative sum (by virtue of arrange(), this is the cum. sum from the earliest date)
  complete(DateApproved = seq.Date(min(DateApproved), max(DateApproved), by="day")) %>% #interpolate dates that, for some states, do not exist b/c no loans were approved then
  fill(Loans) %>% #use row above's Loans value as new dates' loan values
  ungroup() %>%
  inner_join(select(filter(naics, NAICS == "--"), State, Businesses, Employees, Population, geometry)) %>% #add employment figures, population, spatial geometry
  mutate("Loans per business" = Loans/Businesses) %>%
  st_as_sf() %>% #re-make into spatial data
  tm_shape() +
  tm_borders() +
  tm_fill(col = "Loans per business", palette = "viridis") + #color states by loan figures
  tm_facets(along = "DateApproved", free.coords = FALSE) + #creates many maps
  tmap::tm_credits("Yale Program on Financial Stability\nSource: Small Business Administration | Census Bureau") +
  tm_layout(title = "Cumulative approved loans per small business")
tmap_animation(p, "plots/perbiz-map.gif", restart.delay = 100) #create animation