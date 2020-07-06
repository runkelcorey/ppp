##########################
#Visualizing PPP loan data
#Corey Runkel
#2020-07-06
##########################

#libraries
library(tidyverse)
library(lubridate)
library(sf)

#pre-processing
all_loans <- mutate(all_loans, LoanRange = ifelse(is.na(LoanRange), "", as.character(factor(LoanRange, levels = c("e $150,000-350,000", "d $350,000-1 million", "c $1-2 million", "b $2-5 million", "a $5-10 million"), labels = c("$150k-$350k", "$350k-$1m", "$1m-$2m", "$2m-$5m", "$5m-$10m")))), #create factor from character
                    LoanRange2 = ifelse(is.na(LoanAmount), "", as.character(cut(LoanAmount, breaks = c(-Inf, 10000, 50000, 100000, 150000), labels = c("$0-$10k", "$10k-$50k", "$50k-$100k", "$100k-$150k"), right = TRUE)))) %>% #create factor from continuous variable
  unite(col = LoanRange, LoanRange, LoanRange2, sep = "") %>% #unify variables
  mutate(LoanRange = factor(LoanRange, levels = c("$0-$10k", "$10k-$50k", "$50k-$100k", "$100k-$150k", "$150k-$350k", "$350k-$1m", "$1m-$2m", "$2m-$5m", "$5m-$10m"), ordered = TRUE)) #re-create factor levels

#plot of loan approvals
ggplot(all_loans, aes(DateApproved, fill = LoanRange)) +
  geom_area(stat = "count") + #define y aesthetic as a count of loans
  scale_x_date(date_breaks = "2 weeks", date_labels = "%B %d", expand = expansion(mult = .01)) + #increase frequency of x axis ticks (1 week = 1 year in COVID)
  scale_y_continuous(labels = scales::comma) + #prevent scientific notation
  labs(x = "Approval date", y = "Number of loans", title = "Cumulative distribution of 4,931,453 PPP loan approvals", subtitle = "April 3, 2020 - June 30, 2020", caption = "Source: Small Business Association", fill = "Loan range")
