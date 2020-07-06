########################
#Importing PPP loan data
#Corey Runkel
#2020-07-06
########################

#libraries
library(tidyverse)
library(lubridate)
library(vroom)

#import small loans
under_names <- paste("data/", list.files(path = "data/", pattern = "^foia_up_to_150k_", recursive = TRUE), sep = "") #list files smaller than $150k
under <- vroom(file = under_names[1], col_types = c("ncfcfffffcnfcc")) #create initial tibble
for (i in seq_along(under_names[2:57])) { #append the rest
  under <- bind_rows(under, vroom(file = under_names[i], col_types = c("ncfcfffffcnfcc")))
}

#aggregate loans on non-size characteristics
all_loans <- bind_rows(under, vroom(file = "data/150k plus/foia_150k_plus.csv", col_types = c("ccccfcffffffnccc"))) %>% #quickly import loans larger than $150,000
  mutate(DateApproved = mdy(DateApproved)) #convert DateApproved to date format