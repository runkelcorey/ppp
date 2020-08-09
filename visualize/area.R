##########################
#Visualizing PPP loan data
#Area plots
#Corey Runkel
#2020-07-16
##########################

library(tidyverse)
library(lubridate)
library(viridis)

#pre-processing: requires all_loans dataset created from import.R script
all_loans <- read_csv("data/all_ppp_loans.csv", col_types = c("nccccfffflnDccfccnf")) %>%
  mutate(LoanRange = factor(LoanRange, levels = c("$0-$10k", "$10k-$50k", "$50k-$100k", "$100k-$150k", "$150k-$350k", "$350k-$1m", "$1m-$2m", "$2m-$5m", "$5m-$10m"), ordered = TRUE)) #re-create factor levels

#by loan size
##absolute: loan-range-distribution
ggplot(all_loans, aes(DateApproved, fill = LoanRange)) +
  geom_area(stat = "count") + #define y aesthetic as a count of loans
  scale_x_date(date_breaks = "2 weeks", date_labels = "%B %d", expand = expansion(mult = .01)) + #increase frequency of x axis ticks (1 week = 1 year in COVID)
  scale_y_continuous(labels = scales::comma) + #prevent scientific notation
  labs(x = "Approval date", y = "Number of loans", title = "Distribution of 4,931,453 PPP loan approvals", subtitle = "April 3, 2020 - June 30, 2020", caption = "Source: Small Business Association", fill = "Loan range") +

##percentage: loan-range-percentage
all_loans %>%
  group_by(DateApproved, LoanRange) %>%
  summarize(freq = n()) %>% #count the number of loans per day, per loan range
  ungroup() %>%
  group_by(DateApproved) %>%
  mutate(dailyfreq = sum(freq)) %>% #sum within days all the loans in a range
  ungroup() %>%
  ggplot(aes(DateApproved, y = freq/dailyfreq, fill = LoanRange)) + #plot portion of loans per range every day
  geom_area() + #use area plot (default is stacked areas)
  annotate("rect", xmin = as.Date("2020-04-17"), xmax = as.Date("2020-04-26"), ymin = -Inf, ymax = Inf, alpha = .4, fill = "#f9f9f9") +
  annotate("text", x = as.Date("2020-04-24"), y = .66, label = "First round funds exhausted", angle = 90, color = "#4a4a4a") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%B %d", expand = expansion(mult = .01)) + #increase frequency of x axis ticks (1 week = 1 year in COVID)
  scale_y_continuous(labels = scales::percent) + #label axes by percent
  labs(x = "Approval date", y = "Percentage of daily loans", title = "Daily distribution of 4,931,453 PPP loan approvals", subtitle = "April 3, 2020 - June 30, 2020", caption = "Source: Small Business Association", fill = "Loan range")

#cdfi vs. noncdfi lenders: cdfi-race
p <- read_csv("ppp/data/cdfis.csv") %>%
  separate(Lender, c("Lender", NA), sep = " d/b/a", remove = TRUE) %>% #remove anything after d/b/a in lender name
  mutate(Lender = str_remove_all(Lender, ", National Association"), CDFI = "CDFI") %>% #remove national association
  right_join(all_loans, ignore_case = TRUE) %>% #ignore case
  mutate(CDFI = ifelse(!is.na(CDFI), CDFI, "Not CDFI"), RaceEthnicity = fct_collapse(RaceEthnicity, "White" = "White", "Black" = "Black or African American", "Hispanic" = "Hispanic", "Asian" = "Asian", "Unanswered" = "Unanswered", other_level = "Other")) %>%
  filter(RaceEthnicity != "Unanswered") %>%
  group_by(DateApproved, CDFI, RaceEthnicity) %>%
  summarize(Loans = n()) %>%
  group_by(CDFI, RaceEthnicity) %>%
  arrange(DateApproved) %>%
  mutate(Loans = cumsum(Loans)) %>%
  complete(DateApproved = seq.Date(min(DateApproved), max(DateApproved), by="day")) %>% #interpolate dates that, for some states, do not exist b/c no loans were approved then
  fill(Loans) %>% #use row above's Loans value as new dates' loan values
  ungroup() %>%
  ggplot() +
  geom_area(aes(DateApproved, Loans, fill = RaceEthnicity), position = "fill") +
  facet_grid(cols = vars(CDFI)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = wesanderson::wes_palette("Royal2")) +
  labs(x = "Date approved", y = "Cumulative portion of loans", title = "Loans approved by applicant demographics", caption = "Source: Small Business Administration", subtitle = "28% of approved CDFI loans gave race or ethnicity, compared to 10% of non-CDFI loans", fill = "Race or ethnicity")