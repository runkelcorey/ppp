##########################
#Visualizing PPP loan data
#Corey Runkel
#2020-07-06
##########################

#libraries
library(tidyverse)
library(lubridate)

#pre-processing: requires all_loans dataset created from import.R script
all_loans <- mutate(all_loans, LoanRange = ifelse(is.na(LoanRange), "", as.character(factor(LoanRange, levels = c("e $150,000-350,000", "d $350,000-1 million", "c $1-2 million", "b $2-5 million", "a $5-10 million"), labels = c("$150k-$350k", "$350k-$1m", "$1m-$2m", "$2m-$5m", "$5m-$10m")))), #create factor from character
                    LoanRange2 = ifelse(is.na(LoanAmount), "", as.character(cut(LoanAmount, breaks = c(-Inf, 10000, 50000, 100000, 150000), labels = c("$0-$10k", "$10k-$50k", "$50k-$100k", "$100k-$150k"), right = TRUE)))) %>% #create factor from continuous variable
  unite(col = LoanRange, LoanRange, LoanRange2, sep = "") %>% #unify variables
  mutate(LoanRange = factor(LoanRange, levels = c("$0-$10k", "$10k-$50k", "$50k-$100k", "$100k-$150k", "$150k-$350k", "$350k-$1m", "$1m-$2m", "$2m-$5m", "$5m-$10m"), ordered = TRUE)) #re-create factor levels

#temporal distribution of loan approvals
##absolute
ggplot(all_loans, aes(DateApproved, fill = LoanRange)) +
  geom_area(stat = "count") + #define y aesthetic as a count of loans
  scale_x_date(date_breaks = "2 weeks", date_labels = "%B %d", expand = expansion(mult = .01)) + #increase frequency of x axis ticks (1 week = 1 year in COVID)
  scale_y_continuous(labels = scales::comma) + #prevent scientific notation
  labs(x = "Approval date", y = "Number of loans", title = "Distribution of 4,931,453 PPP loan approvals", subtitle = "April 3, 2020 - June 30, 2020", caption = "Source: Small Business Association", fill = "Loan range")
##percentage
all_loans %>%
  group_by(DateApproved, LoanRange) %>%
  summarize(freq = n()) %>% #count the number of loans per day, per loan range
  ungroup() %>%
  group_by(DateApproved) %>%
  mutate(dailyfreq = sum(freq)) %>% #sum within days all the loans in a range
  ungroup() %>%
  ggplot(aes(DateApproved, y = freq/dailyfreq, fill = LoanRange)) + #plot portion of loans per range every day
  geom_area() + #use area plot (default is stacked areas)
  scale_x_date(date_breaks = "2 weeks", date_labels = "%B %d", expand = expansion(mult = .01)) + #increase frequency of x axis ticks (1 week = 1 year in COVID)
  scale_y_continuous(labels = scales::percent) + #label axes by percent
  labs(x = "Approval date", y = "Percentage of daily loans", title = "Daily distribution of 4,931,453 PPP loan approvals", subtitle = "April 3, 2020 - June 30, 2020", caption = "Source: Small Business Association", fill = "Loan range")

#size distribution of loan approvals below $150k
all_loans %>%
  filter(LoanAmount > 0) %>% #remove NAs and that one loan that was -$2 million
  ggplot() +
  geom_density(aes(LoanAmount), color = "#286dc0") + #density along loan amount
  scale_x_continuous(labels = scales::comma) + #prevent scientific notation
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Size of loan approved ($)", y = "Frequency", title = "Distribution of PPP loan approvals below $150,000", subtitle = "Data covers 4,270,162 loans approved between April 3 and June 30, 2020", caption = "Source: Small Business Administration")