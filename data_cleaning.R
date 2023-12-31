library(readr)
library(tidyverse)
library(stringr)

########################

#Import data
Biden <- read_csv("Final/Biden_support_rate_raw.csv")

#Store the start month, in case the survey starts and ends in same month
start_month <- str_split(Biden$Time, " ", simplify = TRUE)[,1]

#Clean the Time column
Biden_time <- 
  
  #Split Time column into start and end
  str_split(Biden$Time, "-", simplify = TRUE) %>% as.tibble() %>%
  rename(start = V1, end = V2) %>%
  
  #Paste month before the end date, if it starts and ends in same month
  cbind(start_month) %>%
  mutate(end = ifelse(grepl("^[0-9]+$", end), paste(start_month, end), end)) %>%
  
  #Turn into stardard date format
  mutate(
    start_date = as.Date(paste(start, "2023"), format = "%b. %d %Y"),
    end_date = as.Date(paste(end, "2023"), format = "%b. %d %Y"),
  ) %>%
  select(-start_month, -start, -end) #drop raw data


Biden_clean <- Biden %>% 
  
  #Separate the sample size and survey type
  separate(`Sample Size`, into = c("n", "type"), 
           sep = "(?=[A-Za-z])", remove = TRUE) %>%
  
  #Rename
  rename(ybar = `Approval Rate (unadjusted)`,
         s = `Variance(estimated by p(1-p))`) %>%
  
  #Add labels for survey types, change column types
  mutate(
         type_label = str_replace_all(type, c("L" = "Likely Voters", 
                                              "A" = "All Adults", 
                                              "R" = "Registered Voters")),
         ybar = as.numeric(sub("%", "", ybar)) / 100,
         s = as.numeric(sub("%", "", s)) / 100,
         n = as.numeric(sub(",", "", n))
         ) %>%

  #Merge data with pre-prepared time data frame, select variables
  cbind(Biden_time) %>%
  select(start_date, end_date, ybar, s, n, type, type_label) %>%
  suppressWarnings()
  

#Store the cleaned data
write.csv(Biden_clean, "Final/Biden_support_rate_clean.csv")


##################TRUMP Poll########
Trump <- read_csv("president_approval_polls_historical.csv")
Biden <- read_csv("approval_polllist.csv")

Cols_Biden <- colnames(Biden)
Cols_Trump <- colnames(Trump)

Cols_Biden[!(Cols_Biden %in% Cols_Trump)]

Trump_clean <- Trump %>%
  mutate(polltype = NA,
         subgroup = NA,
         raw_sample_size = NA,
         weight = NA,
         approve_adjusted = NA,
         disapprove_adjusted = NA,
         timestamp = NA) %>%
  rename(approve = yes,
         disapprove = no) %>%
  select(politician, polltype, subgroup, start_date, end_date, 
         pollster_rating_name, sample_size, raw_sample_size, population, 
         weight, approve, disapprove, approve_adjusted, disapprove_adjusted, 
         tracking, url, poll_id, question_id, created_at, timestamp, 
         pollster_rating_id, subpopulation)

colnames(Trump_clean) == colnames(poll)

write.csv(Trump_clean, file = "approval_polllist_Trump.csv")
