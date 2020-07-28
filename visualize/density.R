##########################
#Visualizing PPP loan data
#Density plots
#Corey Runkel
#2020-07-16
##########################

library(tidyverse)
library(lubridate)
library(viridis)

#pre-processing: requires all_loans dataset created from import.R script
all_loans <- read_csv("data/all_ppp_loans.csv", col_types = c("nccccfffflnDccfccnf")) %>%
  mutate(LoanRange = factor(LoanRange, levels = c("$0-$10k", "$10k-$50k", "$50k-$100k", "$100k-$150k", "$150k-$350k", "$350k-$1m", "$1m-$2m", "$2m-$5m", "$5m-$10m"), ordered = TRUE)) #re-create factor levels

#by loan amount: business-type
all_loans %>%
  filter(LoanAmount > 0, !is.na(BusinessType)) %>% #remove NAs and that one loan that was -$2 million
  mutate(FirmSize = fct_collapse(BusinessType, "Freelancers" = c("Self-Employed Individuals", "Independent Contractors"), "Sole Proprietors" = "Sole Proprietorship",  other_level = "Multi-person")) %>%
  ggplot() +
  geom_density(aes(LoanAmount, fill = FirmSize, color = FirmSize), alpha = .4, bw = 500) + #density along loan amount
  scale_x_continuous(labels = scales::dollar, expand = expansion(.01)) + #make borders smaller
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#bd5319", "#63aaff", "#5f712d"), name = "Firm structure") + #Yale colors
  scale_color_manual(values = c("#bd5319", "#63aaff", "#5f712d"), name = "Firm structure") +
  annotate("text", x = 20833, y = .000139, label = "$20,800", size = 4) + #add label for mode
  labs(x = "Size of loan approved (intervals of $500)", y = "Frequency", title = "Distribution of PPP loan approvals below $150,000 by business structure", subtitle = "Data covers 4,270,162 loans approved between April 3 and June 30, 2020", caption = "Source: Small Business Administration")

#by lender: lenders-loans
all_loans %>%
  group_by(Lender, LoanRange) %>%
  summarize(LoansProcessed = n()) %>%
  ggplot(aes(LoansProcessed, fill = LoanRange)) +
  geom_histogram(binwidth = 100) +
  coord_cartesian(xlim = c(0, 1000)) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis_d() +
  labs(x = "Loans processed", y = "Number of lenders", title = "Distribution of lenders by loan volume and size", subtitle = "236 unique lenders that processed 1,000-300,000 loans per range were omitted for readability", caption = "Source: Small Business Administration", fill = "Loan range")

#jobs retained X loan amount (too large?)
all_loans %>%
  filter(LoanAmount > 0, JobsRetained > 0) %>%
  ggplot() +
  geom_density_2d_filled(aes(LoanAmount, JobsRetained), alpha = .3) +
  labs(x = "Loan Amount", y = "Jobs Retained", title = "Jobs retained by PPP loan size", subtitle = "Data only available for loans below $150,000", caption = "Source: Small Business Administration")

#jobs retained by industry: jobs-retained-industry
all_loans %>%
  filter(JobsRetained > 0, JobsRetained < 50, Industry %in% c("Professional services, research, & IT", "Restaurants & hospitality", "Healthcare", "Retail")) %>%
  ggplot() +
  geom_density(aes(x = JobsRetained, fill = Industry), alpha = .6, bw = 1, outline.type = "full") +
  scale_fill_manual(values = wesanderson::wes_palette("Chevalier1"), labels = c("Healthcare", "Professional services, research, & IT", "Retail", "Hotels & restaurants")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of jobs retained by select industries", subtitle = "Horizontal axis truncated to improve readability: 96% of reporting firms retained fewer than 50", x = "Jobs retained", y = "Portion of loans approved", caption = "Source: Small Business Administration")