##########################
#Visualizing PPP loan data
#Corey Runkel
#2020-07-06
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


#animated column chart (didn't really work)
library(gganimate)
##first make static chart
all_loans %>%
  separate(Lender, c("Lender", NA), sep = " d/b/a", remove = TRUE) %>% #remove anything after d/b/a in lender name
  mutate(Lender = str_remove_all(Lender, ", National Association")) %>% #remove national association
  group_by(Lender, DateApproved) %>%
  summarize(LoansProcessed = n()) %>% #count each loan per lender per day
  ungroup() %>%
  group_by(Lender) %>% #so cumulative sum is done within each lender
  arrange(DateApproved) %>% #so cumulative sum is done in chronological order
  mutate(LoansProcessed = cumsum(LoansProcessed)) %>%
  ungroup() %>%
  group_by(DateApproved) %>%
  mutate(LoanPortion = LoansProcessed/sum(LoansProcessed)) %>% #avoid double counting cumulative sums by only summing within each day across lenders
  filter(DateApproved >= "2020-05-15") %>%
  slice_max(order_by = LoansProcessed, n = 10) %>% #keep only top ten lenders
  ungroup() %>%
  ggplot(aes(reorder(Lender, LoanPortion), LoanPortion)) + #must reorder bar graph or it will be effectively random
  geom_col(aes(fill = Lender), show.legend = FALSE) +
  coord_flip() + #order top to bottom is more intuitive (eg. "top ten" not "left ten")
  scale_y_continuous(labels = scales::percent, expand = expansion(.02)) + #convert decimals to percentages
#animation
  transition_states(DateApproved, transition_length = 4, state_length = 1) +
  labs(x = NULL, y = "Portion of PPP loans processed as of {closest_state}", title = "Top 10 lenders by portion of loans processed", subtitle = "Number of loans approved in chart", caption = "Source: Small Business Association") +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.background = element_blank()) #tweaks for readability

#loans-zip-rural
all_loans %>%
  group_by(Zip) %>%
  summarize(Loans = n()) %>%
  ungroup() %>%
  inner_join(rename(select(read_csv("ppp/data/zbp17totals.txt"), zip, est), Zip = zip)) %>%
  left_join(mutate(read_csv("ppp/data/rural-zips.csv", col_types = c("c")), Rural = "Rural")) %>%
  mutate(Rural = ifelse(is.na(Rural), "Not rural", Rural)) %>%
  ggplot(aes(est, Loans)) +
  geom_point(aes(col = Rural), alpha = .7) +
  scale_color_manual(values = c("#5f712d", "#bd5319")) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = "lm", color = "#286dc0") +
  labs(x = "Businesses", title = "Relationship between PPP loans approved and businesses by zipcode", subtitle = "Some loans omitted due to P.O. boxes", caption = "Source: Small Business Administration | Census Bureau")

#jobsretained
all_loans %>%
  filter(LoanAmount > 0, JobsRetained > 0) %>%
  ggplot(aes(LoanAmount, JobsRetained)) +
  geom_point(aes(col = DateApproved), shape = '.') +
  geom_rug() +
  geom_smooth(method = "lm", color = "#286dc0") +
  scale_x_continuous(labels = scales::dollar) +
  scale_color_date(labels = scales::date_format(format = "%B %d")) +
  labs(x = "Loan amount", y = "Jobs retained", col = "Date approved", title = "Relationship between loan amount and jobs retention", caption = "Source: Small Business Administration")

#race-time
all_loans %>%
  mutate(RaceEthnicity = fct_collapse(RaceEthnicity, "White" = "White", "Black" = "Black or African American", "Hispanic" = "Hispanic", "Asian" = "Asian", "Unanswered" = "Unanswered", other_level = "Other")) %>%
  filter(RaceEthnicity != "Unanswered") %>%
  ggplot(aes(DateApproved)) +
  geom_bar(aes(fill = RaceEthnicity)) +
  annotate("rect", xmin = as.Date("2020-04-17"), xmax = as.Date("2020-04-26"), ymin = -Inf, ymax = Inf, alpha = .2) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = wesanderson::wes_palette("Royal2")) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%B %d") + #increase frequency of x axis ticks (1 week = 1 year in COVID)
  labs(x = "Date approved", y = "Loans", title = "Race or ethnicity of loan applicants over time", subtitle = "Note: gap due to exhaustion of initial funds; 89% of applicants did not report race or ethnicity", caption = "Source: Small Business Administration", fill = "Race or ethnicity")