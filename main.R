#### main ###

### open libaries ###

library(haven)
library(tidyr)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(psych)
library(GPArotation)
library(lm.beta)
library(mctest)
library(plotly)
library(dplyr)
library(stringr)
library(ingmarkdowntemplates)


### Functions ###

source("functions.R")

### Open data ###


raw_data <- read.csv("200122_ING_Global_Data_Set_Q4Y14-Q4Y19_CSV.csv", header = T)
# Add in X1 which is a row count
raw_data <- raw_data %>% 
  dplyr::mutate(
    X1 = 1:n()
  )

ID_char <- c("X1", "quarter_measurement", "country", "age", "gender", "income", "Weight", "weight_nps", "love_ING", "love_Google")

base_data <- raw_data %>% 
  select(ID_char)

# Wanted categories
categories <- c("unaided", "relationship", "aided", "fami", "opinion", "consideration", "preference", "nps",
                "image1", "image2", "image3", "image4", "image5", "image6", "image7", "image13", "image16", 
                "image19", "image20", "reptrak1", "reptrak2", "reptrak3", "reptrak4", "trust4", "proxi", "desirability", "price_perc", "client", "main_bank", "empower_1", "empower_2", "empower_3", "empower_4", "love_respected", "love_meaningful", 
                "love_irresistible", "love_irreplaceable", "consideration_p1", "consideration_p2", "consideration_p3", "consideration_p4", "preference_p1", "preference_p2", 
                "preference_p3", "preference_p4", "relationship_p1", "relationship_p2", "relationship_p3", "relationship_p4", "consideration_p1", "consideration_p2", "consideration_p3", 
                "consideration_p4", "product_awareness_p1", "product_awareness_p2", "product_awareness_p3", 
                "product_awareness_p4", "banktype", "product_usage_p1", "product_usage_p2", "product_usage_p3", "product_usage_p4",
                "product_usage_p5", "product_usage_p6", "product_usage_p7", "product_usage_p8", "product_usage_p9", "ad_awareness", "message_recall")

# Confirm that categories has distinct arguments
categories <- unique(categories)

# cat_to_change <- c("product_usage_p1", "product_usage_p2", "product_usage_p3","product_usage_p4",
#                    "product_usage_p5", "product_usage_p6", "product_usage_p7", "product_usage_p8", "product_usage_p9")

# Markdown renders
rmarkdown::render("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/qreport.Rmd", output_file = "kpi.html")

