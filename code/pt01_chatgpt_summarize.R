# Title: Chat GPT Summarize 
# Author: Justin Kringler
# Date: 12/31/2023

# Description: Read raw data and have chat gpt summarize the 
# permit descriptions and assign them to human made topics.

# Notes: Need to insert your own API key. 

# Future: Another round of OpenAi modeling. After the summarizations
# are complete, ask the model: "based on this summarization: INSERT,
# which topic does it mostly belong to: LIST TOPICS. Would obviously
# need to iterate and prompt engineer/do more rounds to get better results.

##### Libraries
library(tidyverse) # Mainly for Dplyr
library(chatgpt) # Reset Chat logs
library(openai) # OpenAi Chat Models
library(quanteda) # token assessment


##### Environment ----

# Github raw data
permits_part1 <- "https://github.com/JustinKringler1/stpaul_housing_permit_app/raw/main/data/Building_Permits_1.RDS"
permits_part2 <- "https://github.com/JustinKringler1/stpaul_housing_permit_app/raw/main/data/Building_Permits_2.RDS"

# Setting API key for open ai
# Sys.setenv(OPENAI_API_KEY="INSERT API KEY HERE")

##### Data Prep ----

# Read Raw Datas
raw_data01 <- readRDS(url(permits_part1))
raw_data02 <- readRDS(url(permits_part2))

# Combine Dataframes
raw_data <- raw_data01 %>%
  rbind(raw_data02)

# Filter for finalized single family dwellings (our area of focus) and
# capping how long the description can be to reduce tokens.
chat_data <- raw_data %>% 
  filter(STATUS == "Finaled",
         SUB_TYPE == "Single Family Dwelling") %>%
  mutate(FOLDERDESCRIPTION = gsub("The following.*", "", FOLDERDESCRIPTION)) %>%
  mutate(permit_description = str_trunc(FOLDERDESCRIPTION,200),
         token_count = ntoken(permit_description))%>%
  filter(permit_description != "")


##### OpenAI Text Summarization ----

# Trying to see if the summarization list already existts in my 
# github, otherwise need to regenerate them.
test <-try(readRDS("C:/Users/justi/OneDrive/Desktop/mn_data/list.RDS"))

# Testing to see if the summarized list matches the same length as
# our dataframe. If it doesn't were assuming we need to generate
# new summarizations.
chat_data_test <- try(nrow(data.frame(test)) == 33960)

# So if the previous test failed:
if(chat_data_test == FALSE) {
  
# Generate empty list to store summarizations
chat_list <- c() 

# Initialize start of while loop
i <- 1

# While Loop will run til it has summarized every row.
while (i <= nrow(chat_data)) {
  
  # We are doing a try catch when summarizing the text by our
  # openAI model because if it errors out (it ocassionaly does),
  # we want it to reattempt that row. So if it errors, we subtract
  # 1 from 'i' so that it has to do it again in the loop. If no error
  # we save the summarization and continue to next. It is important
  # we reset chata log otherwise they itterate off the previous chat
  # and our token ammount skyrockets (costing you more money). This
  # might not hold true for the instruct model but it does for the generic
  # chatgpt model.

  tryCatch(
    {
      chat_list[i]<-openai::create_completion(
        model = "gpt-3.5-turbo-instruct",
        prompt = paste0("Based on the following home permit description, give a generic summarization in UNDER 5 words and keep it short, do not include numbers, keep it generic: ",chat_data$permit_description[i])
      )$choices$text
      
    },
    error = function(cond) {
      message("error")
      i <- i -1
    },
    finally = {
      message("done")
      i <- i +1
      reset_chat_session()
    }
  )
}
}


##### Chat GPT Summarized Dataframe -----

# The order of the casewhen matters. For example,
# we search for egress before windows because most "egress"
# topics will also contain "window". This logic also applies to
# "shingle" and "roof". 

# NOTE: This list is pretty rough and informal, I understand
# this might not be the most perfect way to group these, but
# for the purpose of this app we are doing it this way for 
# simplicity.

final_chat_data  <- chat_data %>%
  # Assigning the openAI summarized list to a column
  mutate(chat_summary = list) %>% 
  # Removing blanks
  filter(chat_summary != "") %>%
  # Removing random quotation marks
  mutate(chat_summary = gsub('\\"',"",chat_summary)) %>%
  # Case Whens to generate topics.
  mutate(categories = case_when(str_detect(chat_summary,
                                           "(?i)bath") ~ "Bathroom",
                                str_detect(chat_summary,
                                           "(?i)egress") ~ "Egress Window",
                                str_detect(chat_summary,
                                           "(?i)shingle") ~ "Shingles",
                                str_detect(chat_summary,
                                           "(?i)window") ~ "Windows",
                                str_detect(chat_summary,
                                           "(?i)door") ~ "Door",
                                str_detect(chat_summary,
                                           "(?i)roof") ~ "Roof",
                                str_detect(chat_summary,
                                           "(?i)drain") ~ "Drainage",
                                str_detect(chat_summary,
                                           "(?i)deck") ~ "Deck",
                                str_detect(chat_summary,
                                           "(?i)solar") ~ "Solar",
                                str_detect(chat_summary,
                                           "(?i)kitchen") ~ "Kitchen",
                                .default = "Other"))

