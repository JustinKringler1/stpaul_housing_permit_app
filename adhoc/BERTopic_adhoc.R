# Title: BERTopic Adhoc
# Author: Justin Kringler
# Linkedin: https://www.linkedin.com/in/justin-kringler/
# Date: 12/26/2023

# Description: See if we can use the summarized data from chatgpt
# to create better topics than manually grouping by str_detect

# Warning: The results were not strong enough. Not enough context to
# derive accurate topics. Major topics would overlap, not enough distinction.
#
# Tried: 
# 1) Semi-Supervisted I.E Passing in 'categories'
# 2) Unsupervised I.E Just 'docs'
# 3) adjusting Topic size
# 4) HDBScan Default values
# 5) UMAP Default values
# 6) Chatgpt summarized text and clean unsummarized text

# Future Itterations:
# 1) Adjust clustering parameters
# 2) Try differerent clustering methods
# 3) Adhust dimensionality reduction parameters
# 4) Try different Dimensionality Reduction methods
# 5) Itterate more with different openai models and prompts


##### Libraries ----

library(tidyverse) # Mainly for Dplyr
pak::pak("rstudio/reticulate") # need this version so that hdbscan works
library(reticulate) # BERTopics
library(data.table) # for eval


##### Environment ----

# Installing Python 
install_python(version = "3.9:latest", list = FALSE, force = FALSE)

# create a new environmentand install bertopic to it
virtualenv_create("bert_environ")
virtualenv_install("bert_environ", "bertopic")

# importing bertopic
bertopic <- import("bertopic")


##### BERTopic ----

# Sourcing Chat Data for BERTopic
source("code/pt01_chatgpt_summarize.R")

# Converting to Python Data
python_data <- r_to_py(final_chat_data)

# Converting chat data and catergories to generate embeddings
docs = python_data[['chat_summary']]
categories = python_data[['categories']]

# Initializing BERTopic, choosing 20 topic max 
# because deafult gives awful results
topic_model = bertopic$BERTopic(nr_topics = 20)

# Fitting BERTopic
bert_fit = topic_model$fit_transform(docs, y=categories)

# Gathering info
bert_dict = topic_model$get_topic_info()


##### Evaluating Results ----

# Adding BERTs topics back to df
final_bert_data <-final_chat_data %>%
  mutate(topic = bert_fit[[1]],
         prob = bert_fit[[2]],
         topic_name = bert_dict$Name[topic+2]) %>%
  filter(FOLDERDESCRIPTION != "")

# Play Around to evaluate
topic = 0 # adjust this to change topic in next chunk

# Sample 15 random permits for specified topic
data.table(final_bert_data %>%
  filter(topic == topic) %>%
  select(chat_summary, prob) %>%
  sample_n(15))


