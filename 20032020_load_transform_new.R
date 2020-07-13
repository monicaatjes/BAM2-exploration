# BEFORE RUNNING CHANGE WORKING DIRECTORY TO SOURCE FILE LOCATION
# Dependencies
library(tidyverse)
source("add_questionnaire_responds_category.R")
source("changed_messed_up_variable_names.R")
## Load and transform BAM data Q1

# Throw away columns specifically for ML
raw_data2 <- raw_data3 %>%
  dplyr::select(-contains("SPSS"))

# Add in X1 which is a row count
raw_data1 <- raw_data2 %>% 
  dplyr::mutate(
    X1 = 1:dplyr::n()
  )

ID_char1 <- c("X1", "quarter_measurement", "country", "age", "age_groups", "gender", 
              "weight", "Love_ING", "Love_Google", "freedom_segment", "corona_statement_1", "corona_statement_2")

base_data1 <- raw_data1 %>% 
  dplyr::select(ID_char1)

# Wanted categories
categories <- c("unaided", "toma", "aided", "fami", "consideration", "preference", "relationship", "desirability",
                "price_perception", "easy", "smart", "meets_needs", "nps", "reptrak1", "reptrak2", "reptrak3", 
                "reptrak4", "reptrak5", "reptrak6", "reptrak7", "reptrak8", "reptrak9", "reptrak10", "reptrak11",  
                "trust4", "image20", "empower_1", "empower_2", "empower_3", "empower_4",
                "digital_experience", "app_usage", "client", "ad_awareness", "main_bank", "product_usage_p1", 
                "product_usage_p2", "product_usage_p3", "product_usage_p4", "product_usage_p5", "product_usage_p6", 
                "product_usage_p7", "product_usage_p8", "product_usage_p9", "freedom_statement_2",
                "freedom_statement_3", "interest", "interest_action", "corona_support")
                
              
# Confirm that categories has distinct arguments
categories <- unique(categories)

#### ADDING QUESTIONNAIRE RESPONSES  ####
# Clean up rawdata
raw_data1 <- raw_data1 %>% 
  changed_messed_up_variable_names("product_usage")
# Add all the cats to our long data frame

for (x in categories) {
  
  if (x == categories[1]){
    result <- raw_data1 %>%
      dplyr::select(X1, quarter_measurement, country, 
                    age, age_groups, gender, weight, Love_ING, 
                    freedom_segment, Love_Google, corona_statement_1, corona_statement_2) %>%
      add_questionaire_responds_category(raw_data1, x) 
  }
  else{
    print(x)
    result <- add_questionaire_responds_category(
      base_data1 = result, 
      raw_data1 = raw_data1, 
      cat = x
    )
  }
}

##### tidy up
# gather further to get all the scores in one column to reshape the data 
temp <- result %>% 
  select(-X1) %>% 
  gather("type", "score", -quarter_measurement, -country, -age, -age_groups, -gender, -weight, -Love_ING, 
         -Love_Google, -freedom_segment, -b_value)

## Create the variables

### aided awareness
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 0 ~ "Zero",
      score == 1 ~ "One",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    aided = score_one / score_total
  ) %>% 
  dplyr::filter(type == "aided_value")

temp4$score_one <- NULL
temp4$score_zero <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

### unaided value 
temp3 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1, 2)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 2 ~ "Two",
      score == 0 ~ "null",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_two = sum(weight[score_category == "Two"]),
    score_nine_nine = sum(weight[score_category == "null"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    unaided = (score_one + score_two) / score_total
  ) %>% 
  dplyr::filter(type == "unaided_value")

temp3$score_one <- NULL
temp3$score_zero <- NULL
temp3$score_total <- NULL
temp3$score_two <- NULL
temp3$score_nine_nine <- NULL
temp3$type <- NULL

# Connect the first two transformed varibles together and create file data1 that contains all variables for Q1 2020
data1 <- full_join(temp4, temp3, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### toma 
temp33 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1, 2)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 2 ~ "Two",
      score == 0 ~ "null",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_two = sum(weight[score_category == "Two"]),
    score_nine_nine = sum(weight[score_category == "null"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    toma = score_one / score_total
  ) %>% 
  dplyr::filter(type == "toma_value")

temp33$score_one <- NULL
temp33$score_zero <- NULL
temp33$score_total <- NULL
temp33$score_two <- NULL
temp33$score_nine_nine <- NULL
temp33$type <- NULL

data1 <- full_join(data1, temp33, by=c("b_value", "country", "quarter_measurement"))
rm(temp33)

### relationship
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 0 ~ "Zero",
      score == 1 ~ "One",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    relationship = score_one / score_total
  ) %>% 
  dplyr::filter(type == "relationship_value")

temp4$score_one <- NULL
temp4$score_zero <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### fami
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 0 ~ "Zero",
      score == 1 ~ "One",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    fami = score_one / score_total
  ) %>% 
  dplyr::filter(type == "fami_value")

temp4$score_one <- NULL
temp4$score_zero <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### consideration
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 0 ~ "Zero",
      score == 1 ~ "One",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    consideration = score_one / score_total
  ) %>% 
  dplyr::filter(type == "consideration_value")

temp4$score_one <- NULL
temp4$score_zero <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### preference
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 0 ~ "Zero",
      score == 1 ~ "One",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    preference = score_one / score_total
  ) %>% 
  dplyr::filter(type == "preference_value")

temp4$score_one <- NULL
temp4$score_zero <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

## desirability
desirability_overview <- result %>%
  dplyr::select(country, quarter_measurement, b_value, desirability_value, weight) %>%
  dplyr::filter(!is.na(desirability_value)) %>%
  dplyr::group_by(country, quarter_measurement, b_value) %>% 
  dplyr::summarise(
    desirability = mean(desirability_value * weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, desirability_overview, by=c("country", "quarter_measurement", "b_value"))
rm(desirability_overview) 

## easy
easy_overview <- result %>%
  dplyr::select(country, quarter_measurement, b_value, easy_value, weight) %>%
  dplyr::filter(!is.na(easy_value)) %>%
  dplyr::group_by(country, quarter_measurement, b_value) %>% 
  dplyr::summarise(
    easy_mean = mean(easy_value * weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, easy_overview, by=c("country", "quarter_measurement", "b_value"))
rm(easy_overview) 

easy_client <- result %>%
  dplyr::select(country, quarter_measurement, b_value, easy_value, weight, client_value) %>%
  dplyr::filter(!is.na(easy_value)) %>%
  dplyr::filter(client_value==1) %>%
  dplyr::group_by(country, quarter_measurement, b_value) %>% 
  dplyr::summarise(
    easy_client = mean(easy_value * weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, easy_client, by=c("country", "quarter_measurement", "b_value"))
rm(easy_client) 

## meets my needs
meets_needs_client <- result %>%
  dplyr::select(country, quarter_measurement, b_value, meets_needs_value, weight, client_value) %>%
  dplyr::filter(!is.na(meets_needs_value)) %>%
  dplyr::filter(client_value==1) %>%
  dplyr::group_by(country, quarter_measurement, b_value) %>% 
  dplyr::summarise(
    meets_needs_client = mean(meets_needs_value * weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, meets_needs_client, by=c("country", "quarter_measurement", "b_value"))
rm(meets_needs_client) 

## smart
smart_client <- result %>%
  dplyr::select(country, quarter_measurement, b_value, smart_value, weight, client_value) %>%
  dplyr::filter(!is.na(smart_value)) %>%
  dplyr::filter(client_value==1) %>%
  dplyr::group_by(country, quarter_measurement, b_value) %>% 
  dplyr::summarise(
    smart_client = mean(smart_value * weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, smart_client, by=c("country", "quarter_measurement", "b_value"))
rm(smart_client) 

### image20
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 2 ~ "Two",
      score == 3 ~ "Three",
      score == 4 ~ "Four",
      score == 5 ~ "Five",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_two = sum(weight[score_category == "Two"]),
    score_three = sum(weight[score_category == "Three"]),
    score_four = sum(weight[score_category == "Four"]),
    score_five = sum(weight[score_category == "Five"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image20 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image20_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

## freedom statement
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    freedom_statement = score_one / score_total
  ) %>% 
  dplyr::filter(type == "freedom_statement_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

## digital experience
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    digital_experience= score_one / score_total
  ) %>% 
  dplyr::filter(type == "digital_experience_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### client
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    client = score_one / score_total
  ) %>% 
  dplyr::filter(type == "client_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### main bank
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    main_bank = score_one / score_total
  ) %>% 
  dplyr::filter(type == "main_bank_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### trust4
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 0 ~ "Zero",
      score == 1 ~ "One",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    trust4 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "trust4_value")

temp4$score_one <- NULL
temp4$score_zero <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_usage_p1
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_usage_p1 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_usage_p1_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_usage_p2
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_usage_p2 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_usage_p2_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_usage_p3
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_usage_p3= score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_usage_p3_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_usage_p4
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_usage_p4= score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_usage_p4_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_usage_p5
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_usage_p5= score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_usage_p5_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_usage_p6
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_usage_p6= score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_usage_p6_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_usage_p7
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_usage_p7= score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_usage_p7_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_usage_p8
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_usage_p8= score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_usage_p8_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_usage_p9
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_usage_p9= score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_usage_p9_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### ad_awareness
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_zero = sum(weight[score_category == "Zero"]),
    score_total = sum(weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    ad_awareness= score_one / score_total
  ) %>% 
  dplyr::filter(type == "ad_awareness_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data1 <- full_join(data1, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### Reptrak statements
tempREP <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak1 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak1_value")

tempREP$type <- NULL

data1 <- full_join(data1, tempREP, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP)

tempREP2 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak2 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak2_value")

tempREP2$type <- NULL

data1 <- full_join(data1, tempREP2, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP2)

tempREP3 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak3 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak3_value")

tempREP3$type <- NULL

data1 <- full_join(data1, tempREP3, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP3)

tempREP4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak4 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak4_value")

tempREP4$type <- NULL

data1 <- full_join(data1, tempREP4, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP4)

data1<- data1 %>%
  dplyr::mutate(
    pulse = (rep_trak1 + rep_trak2 + rep_trak3 + rep_trak4) /4
  )

## rest van de drivers
tempREP5 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak5 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak5_value")

tempREP5$type <- NULL

data1 <- full_join(data1, tempREP5, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP5)

tempREP6 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak6 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak6_value")

tempREP6$type <- NULL

data1 <- full_join(data1, tempREP6, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP6)

tempREP7 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak7 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak7_value")

tempREP7$type <- NULL

data1 <- full_join(data1, tempREP7, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP7)

tempREP8 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak8 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak8_value")

tempREP8$type <- NULL

data1 <- full_join(data1, tempREP8, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP8)

tempREP9 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak9 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak9_value")

tempREP9$type <- NULL

data1 <- full_join(data1, tempREP9, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP9)

tempREP10 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak10 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak10_value")

tempREP10$type <- NULL

data1 <- full_join(data1, tempREP10, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP10)

tempREP11 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    rep_trak11 =mean(score)/7 *100
  ) %>% 
  dplyr::filter(type == "reptrak11_value")

tempREP11$type <- NULL

data1 <- full_join(data1, tempREP11, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP11)

## Reptrak for clients & non clients
### Reptrak statements
tempREP110 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak1_value, client_value) %>%
  dplyr::filter(!is.na(reptrak1_value) & client_value==1) %>%
  dplyr::filter(reptrak1_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak1_client = reptrak1_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak1_client =(mean(reptrak1_client)/7 *100)/ mean(weight)
  )

data1 <- full_join(data1, tempREP110, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP110)

tempREP22 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak2_value, client_value) %>%
  dplyr::filter(!is.na(reptrak2_value) & client_value==1) %>%
  dplyr::filter(reptrak2_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak2_client = reptrak2_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak2_client =(mean(reptrak2_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP22, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP2)

tempREP23 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak3_value, client_value) %>%
  dplyr::filter(!is.na(reptrak3_value) & client_value==1) %>%
  dplyr::filter(reptrak3_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak3_client = reptrak3_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak3_client =(mean(reptrak3_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP23, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP3)

tempREP24 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak4_value, client_value) %>%
  dplyr::filter(!is.na(reptrak4_value) & client_value==1) %>%
  dplyr::filter(reptrak4_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak4_client = reptrak4_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak4_client =(mean(reptrak4_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP24, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP24)

data1<- data1 %>%
  dplyr::mutate(
    pulse_clients = (reptrak1_client + reptrak2_client + reptrak3_client + reptrak4_client) /4
  )

## rest van de drivers
tempREP25 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak5_value, client_value) %>%
  dplyr::filter(!is.na(reptrak5_value) & client_value==1) %>%
  dplyr::filter(reptrak5_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak5_client = reptrak5_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak5_client =(mean(reptrak5_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP25, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP5)

tempREP26 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak6_value, client_value) %>%
  dplyr::filter(!is.na(reptrak6_value) & client_value==1) %>%
  dplyr::filter(reptrak6_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak6_client = reptrak6_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak6_client =(mean(reptrak6_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP26, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP26)

tempREP27 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak7_value, client_value) %>%
  dplyr::filter(!is.na(reptrak7_value) & client_value==1) %>%
  dplyr::filter(reptrak7_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak7_client = reptrak7_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak7_client =(mean(reptrak7_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP27, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP27)

tempREP28 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak8_value, client_value) %>%
  dplyr::filter(!is.na(reptrak8_value) & client_value==1) %>%
  dplyr::filter(reptrak8_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak8_client = reptrak8_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak8_client =(mean(reptrak8_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP28, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP28)

tempREP29 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak9_value, client_value) %>%
  dplyr::filter(!is.na(reptrak9_value) & client_value==1) %>%
  dplyr::filter(reptrak9_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak9_client = reptrak9_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak9_client =(mean(reptrak9_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP29, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP29)

tempREP210 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak10_value, client_value) %>%
  dplyr::filter(!is.na(reptrak10_value) & client_value==1) %>%
  dplyr::filter(reptrak10_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak10_client = reptrak10_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak10_client =(mean(reptrak10_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP210, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP210)

tempREP211 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak11_value, client_value) %>%
  dplyr::filter(!is.na(reptrak11_value) & client_value==1) %>%
  dplyr::filter(reptrak11_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak11_client = reptrak11_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak11_client =(mean(reptrak11_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP211, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP211)

### NON CLIENTS REPTRAK
### Reptrak statements
tempREP110 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak1_value, client_value) %>%
  dplyr::filter(!is.na(reptrak1_value) & client_value==0) %>%
  dplyr::filter(reptrak1_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak1_non_client = reptrak1_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak1_non_client =(mean(reptrak1_non_client)/7 *100)/ mean(weight)
  )

data1 <- full_join(data1, tempREP110, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP110)

tempREP22 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak2_value, client_value) %>%
  dplyr::filter(!is.na(reptrak2_value) & client_value==0) %>%
  dplyr::filter(reptrak2_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak2_non_client = reptrak2_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak2_non_client =(mean(reptrak2_non_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP22, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP2)

tempREP23 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak3_value, client_value) %>%
  dplyr::filter(!is.na(reptrak3_value) & client_value==0) %>%
  dplyr::filter(reptrak3_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak3_non_client = reptrak3_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak3_non_client =(mean(reptrak3_non_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP23, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP3)

tempREP24 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak4_value, client_value) %>%
  dplyr::filter(!is.na(reptrak4_value) & client_value==0) %>%
  dplyr::filter(reptrak4_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak4_non_client = reptrak4_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak4_non_client =(mean(reptrak4_non_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP24, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP24)

data1<- data1 %>%
  dplyr::mutate(
    pulse_non_clients = (reptrak1_non_client + reptrak2_non_client + reptrak3_non_client + reptrak4_non_client) /4
  )

## rest van de drivers
tempREP25 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak5_value, client_value) %>%
  dplyr::filter(!is.na(reptrak5_value) & client_value==0) %>%
  dplyr::filter(reptrak5_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak5_non_client = reptrak5_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak5_non_client =(mean(reptrak5_non_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP25, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP5)

tempREP26 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak6_value, client_value) %>%
  dplyr::filter(!is.na(reptrak6_value) & client_value==0) %>%
  dplyr::filter(reptrak6_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak6_non_client = reptrak6_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak6_non_client =(mean(reptrak6_non_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP26, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP26)

tempREP27 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak7_value, client_value) %>%
  dplyr::filter(!is.na(reptrak7_value) & client_value==0) %>%
  dplyr::filter(reptrak7_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak7_non_client = reptrak7_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak7_non_client =(mean(reptrak7_non_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP27, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP27)

tempREP28 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak8_value, client_value) %>%
  dplyr::filter(!is.na(reptrak8_value) & client_value==0) %>%
  dplyr::filter(reptrak8_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak8_non_client = reptrak8_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak8_non_client =(mean(reptrak8_non_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP28, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP28)

tempREP29 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak9_value, client_value) %>%
  dplyr::filter(!is.na(reptrak9_value) & client_value==0) %>%
  dplyr::filter(reptrak9_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak9_non_client = reptrak9_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak9_non_client =(mean(reptrak9_non_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP29, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP29)

tempREP210 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak10_value, client_value) %>%
  dplyr::filter(!is.na(reptrak10_value) & client_value==0) %>%
  dplyr::filter(reptrak10_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak10_non_client = reptrak10_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak10_non_client =(mean(reptrak10_non_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP210, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP210)

tempREP211 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak11_value, client_value) %>%
  dplyr::filter(!is.na(reptrak11_value) & client_value==0) %>%
  dplyr::filter(reptrak11_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak11_non_client = reptrak11_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    reptrak11_non_client =(mean(reptrak11_non_client)/7 *100) / mean(weight)
  )

data1 <- full_join(data1, tempREP211, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP211)

### Love overview 
love_overview <- result %>%
 dplyr::filter(!is.na(Love_ING)) %>%
 dplyr::filter(Love_ING %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) %>% 
  dplyr::group_by(country, quarter_measurement) %>% 
  dplyr::summarise(
    love_mean_ING = mean(Love_ING * weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, love_overview, by=c("country", "quarter_measurement"))
rm(love_overview) 

love_overview_client <- result %>%
  dplyr::select(country, quarter_measurement, b_value, client_value, weight, Love_ING) %>%
  dplyr::filter(b_value==1 & client_value==1) %>%
  dplyr::group_by(country, quarter_measurement) %>% 
  dplyr::summarise(
    love_mean_ING_client = mean(Love_ING * weight) /mean(weight),
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, love_overview_client, by=c("country", "quarter_measurement"))
rm(love_overview_client) 

google_overview <- result %>%
  dplyr::filter(!is.na(Love_Google)) %>%
  dplyr::filter(Love_Google %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) %>% 
  dplyr::group_by(country, quarter_measurement) %>% 
  dplyr::summarise(
    love_mean_Google = mean(Love_Google * weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, google_overview, by=c("country", "quarter_measurement"))
rm(google_overview) 

price_perc <- result %>%
  dplyr::select(country, quarter_measurement, b_value, price_perception_value, weight) %>%
  dplyr::filter(!is.na(price_perception_value)) %>%
  dplyr::group_by(country, quarter_measurement, b_value) %>% 
  dplyr::summarise(
    price_mean = mean(price_perception_value * weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, price_perc, by=c("country", "quarter_measurement", "b_value"))
rm(price_perc) 

### NPS
NPS_trial <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, nps_value, client_value) %>%
  dplyr::filter(!is.na(nps_value) & client_value==1) %>% 
  dplyr::mutate(nps_cat = case_when(
    nps_value >= 9 ~ "promotors",
    nps_value < 9 & nps_value > 6 ~ "neutrals",
    nps_value  <= 6 ~ "detractors",
    TRUE ~ "NA_real_")) %>%
  dplyr::group_by(country, quarter_measurement, b_value, nps_cat) %>%
  dplyr::tally(wt=weight) %>%
  dplyr::mutate(
    percentage = n /sum(n) *100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, quarter_measurement, b_value, nps_cat, percentage) %>%
  tidyr::spread(nps_cat, percentage) %>%
  dplyr::mutate(
    NPS_non_rolling =round(promotors, digits =2)- round(detractors, digits =2)
  ) 

data1 <- full_join(data1, NPS_trial, by=c("country", "quarter_measurement", "b_value"))
rm(NPS_trial) 

### Corona additional questions
covid_1 <- temp %>% 
 # dplyr::select(weight, country, quarter_measurement, b_value, corona_statement_1) %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 2 ~ "Two",
      score == 3 ~ "Three",
      score == 4 ~ "Four",
      score == 5 ~ "Five",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_two = sum(weight[score_category == "Two"]),
    score_three = sum(weight[score_category == "Three"]),
    score_four = sum(weight[score_category == "Four"]),
    score_five = sum(weight[score_category == "Five"]),
    score_total = sum(weight) 
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    covid_time_finances= round(((score_one + score_two) / score_total) *100, digits=2),
    covid_no_time_finances = round(((score_three + score_four + score_five) / score_total) *100, digits=2)
 ) %>% 
  dplyr::filter(type == "corona_statement_1")


covid_1$score_one <- NULL
covid_1$score_two <- NULL
covid_1$score_four <- NULL
covid_1$score_three <- NULL
covid_1$score_five <- NULL
covid_1$score_total <- NULL
covid_1$type <- NULL

data1 <- full_join(data1, covid_1, by=c("b_value", "country", "quarter_measurement"))
rm(covid_1)
  
covid_2 <- temp %>% 
  # dplyr::select(weight, country, quarter_measurement, b_value, corona_statement_1) %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 2 ~ "Two",
      score == 3 ~ "Three",
      score == 4 ~ "Four",
      score == 5 ~ "Five",
      score == 6 ~ "Six",
      score == 7 ~ "Seven",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_two = sum(weight[score_category == "Two"]),
    score_three = sum(weight[score_category == "Three"]),
    score_four = sum(weight[score_category == "Four"]),
    score_five = sum(weight[score_category == "Five"]),
    score_six = sum(weight[score_category == "Six"]),
    score_seven = sum(weight[score_category == "Seven"]),
    score_total = sum(weight) 
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    covid_income_drop= round((score_one / score_total) * 100, digits=2),
    covid_online = round((score_two / score_total) * 100, digits=2),
    covid_invest = round((score_three / score_total) *100, digits=2),
    covid_business_impact = round((score_four / score_total) * 100, digits=2),
    covid_more_time = round((score_five / score_total) * 100, digits=2),
    covid_bills = round((score_six / score_total) * 100, digits=2),
    covid_open = round((score_seven / score_total) * 100, digits=2),
  ) %>% 
  dplyr::filter(type == "corona_statement_2")

covid_2$score_one <- NULL
covid_2$score_two <- NULL
covid_2$score_four <- NULL
covid_2$score_three <- NULL
covid_2$score_five <- NULL
covid_2$score_six <- NULL
covid_2$score_seven <- NULL
covid_2$score_total <- NULL
covid_2$type <- NULL

data1 <- full_join(data1, covid_2, by=c("b_value", "country", "quarter_measurement"))
rm(covid_2)  

corona_support <- temp %>% 
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 2 ~ "Two",
      score == 3 ~ "Three",
      score == 4 ~ "Four",
      score == 5 ~ "Five",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_two = sum(weight[score_category == "Two"]),
    score_three = sum(weight[score_category == "Three"]),
    score_four = sum(weight[score_category == "Four"]),
    score_five = sum(weight[score_category == "Five"]),
    score_total = sum(weight) 
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    corona_support = round(((score_four + score_five) / score_total) *100, digits=2),
  ) %>% 
  dplyr::filter(type == "corona_support_value")

corona_support$score_one <- NULL
corona_support$score_two <- NULL
corona_support$score_four <- NULL
corona_support$score_three <- NULL
corona_support$score_five <- NULL
corona_support$score_total <- NULL
corona_support$type <- NULL

data1 <- full_join(data1, corona_support, by=c("b_value", "country", "quarter_measurement"))
rm(corona_support)  

# corona_support_clients
#corona_support_clients <- result %>%
#  dplyr::select(b_value, quarter_measurement, country, weight, corona_support_value, client_value) %>%
#  dplyr::filter(!is.na(corona_support_value) & client_value==1) %>%
#  dplyr::mutate(client_support = case_when(
#    corona_support_value==1 ~ 0,
#    corona_support_value==2 ~ 0,
#    corona_support_value==3 ~ 0,
#    corona_support_value==4 ~ 1,
#    corona_support_value==5 ~ 1,
#    TRUE ~ NA_real_
#  )) %>%
#  dplyr::group_by(country, quarter_measurement, b_value, client_support) %>%
#  dplyr::tally(wt=weight) %>%
#  dplyr::mutate(
#    client_support_corona = n /sum(n) *100
#  ) %>%
#  dplyr::filter(client_support==1)

# corona_support_clients$client_support <- NULL
# corona_support_clients$n <- NULL

# data1 <- full_join(data1, corona_support_clients, by=c("b_value", "country", "quarter_measurement"))

# ING clients thinking/ more about finances
corona_time_spend_clients <- result %>%
  dplyr::select(b_value, quarter_measurement, country, weight, corona_statement_1, client_value) %>%
  dplyr::filter(!is.na(corona_statement_1) & client_value==1 & b_value==1) %>%
  dplyr::mutate(
    corona_statement_1_cat = case_when(
      corona_statement_1 == 1 ~ "One",
      corona_statement_1 == 2 ~ "Two",
      corona_statement_1 == 3 ~ "Three",
      corona_statement_1 == 4 ~ "Four",
      corona_statement_1 == 5 ~ "Five",
      TRUE ~ "No Score"
    )) %>%
  dplyr::group_by(country, quarter_measurement, b_value, corona_statement_1_cat) %>%
  dplyr::tally(wt=weight) %>%
  tidyr::spread(corona_statement_1_cat, n) 
  corona_time_spend_clients[is.na(corona_time_spend_clients)] <- 0
  corona_time_spend_clients <- corona_time_spend_clients %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(
      no_impact = Four + Five + Three,
      impact = One + Two
    ) %>%
    dplyr::mutate(
      ING_clients_no_impact = no_impact / (impact + no_impact) *100,
      ING_clients_impact = impact / (impact + no_impact) *100
    )
  
corona_time_spend_clients$Five <- NULL
corona_time_spend_clients$Four <- NULL
corona_time_spend_clients$Three <- NULL
corona_time_spend_clients$Two <- NULL
corona_time_spend_clients$One <- NULL
corona_time_spend_clients$no_impact <- NULL
corona_time_spend_clients$impact <- NULL

  
data1 <- full_join(data1, corona_time_spend_clients, by=c("b_value", "country", "quarter_measurement"))

# corona_busy_clients
corona_busy_clients <- result %>%
  dplyr::select(b_value, quarter_measurement, country, weight, corona_statement_2, client_value) %>%
  dplyr::filter(!is.na(corona_statement_2) & client_value==1 & b_value==1) %>%
  dplyr::mutate(ING_clients_busy = case_when(
    corona_statement_2 == 1 ~ "covid_income_drop_clients_ING",
    corona_statement_2 == 2 ~ "covid_online_clients_ING",
    corona_statement_2 == 3 ~ "covid_invest_clients_ING",
    corona_statement_2 == 4 ~ "covid_business_impact_clients_ING",
    corona_statement_2 == 5 ~ "covid_more_time_clients_ING",
    corona_statement_2 == 6 ~ "covid_bills_clients_ING",
    corona_statement_2 == 7 ~ "covid_open_clients_ING",
    TRUE ~ "No score " )
  ) %>%
  dplyr::group_by(country, quarter_measurement, b_value, ING_clients_busy) %>%
  dplyr::tally(wt=weight) %>%
  tidyr::spread(ING_clients_busy, n) 
  corona_busy_clients[is.na(corona_busy_clients)] <- 0
  corona_busy_clients <- corona_busy_clients %>%
  #dplyr::group_by(country) %>%
  dplyr::mutate(
    total_clients_ING = covid_income_drop_clients_ING + covid_online_clients_ING+ covid_invest_clients_ING + 
      covid_business_impact_clients_ING + covid_more_time_clients_ING + covid_bills_clients_ING +
      covid_open_clients_ING
    ) %>%
    dplyr::mutate(
      covid_income_drop_ING = (covid_income_drop_clients_ING / total_clients_ING) *100,
      covid_online_ING = (covid_online_clients_ING / total_clients_ING) *100,
      covid_invest_ING = (covid_invest_clients_ING  / total_clients_ING) *100,
      covid_business_impact_ING = (covid_business_impact_clients_ING / total_clients_ING) *100,
      covid_more_time_ING = (covid_more_time_clients_ING / total_clients_ING) *100,
      covid_bills_ING = (covid_bills_clients_ING / total_clients_ING) *100,
      covid_open_ING = (covid_open_clients_ING / total_clients_ING) *100
    )
    
corona_busy_clients$covid_bills_clients_ING <- NULL
corona_busy_clients$covid_business_impact_clients_ING <- NULL
corona_busy_clients$covid_income_drop_clients_ING <- NULL
corona_busy_clients$covid_invest_clients_ING<- NULL
corona_busy_clients$covid_more_time_clients_ING <- NULL
corona_busy_clients$covid_online_clients_ING <- NULL
corona_busy_clients$covid_open_clients_ING <- NULL
corona_busy_clients$total_clients_ING <- NULL

  
data1 <- full_join(data1, corona_busy_clients, by=c("b_value", "country", "quarter_measurement"))

### Interest
interest <- temp %>% 
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 2 ~ "Two",
      score == 3 ~ "Three",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_two = sum(weight[score_category == "Two"]),
    score_three = sum(weight[score_category == "Three"]),
    score_total = sum(weight) 
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    interest_known = (score_one / score_total) *100,
    interest_notsure = (score_two / score_total) *100,
    interest_notaware = (score_three / score_total) *100,
  ) %>% 
  dplyr::filter(type == "interest_value")

interest$score_one <- NULL
interest$score_two <- NULL
interest$score_three <- NULL
interest$score_total <- NULL
interest$type <- NULL

data1 <- full_join(data1, interest, by=c("b_value", "country", "quarter_measurement"))
rm(interest)  

#interest_awareness_clients_ING <- result %>%
#  dplyr::select(interest_value, client_value, b_value, quarter_measurement, country, weight) %>%
#  dplyr::filter(!is.na(interest_value)) %>%
#  dplyr::filter(b_value==1 & client_value ==1) %>%
#  dplyr::mutate(interest_awareness = case_when(
#    interest_value ==1 ~ "interest_known_ING_clients",
#    interest_value ==2 ~ "interest_notsure_ING_clients",
#    interest_value ==3 ~ "interest_notaware_ING_clients",
#  )) %>%
#  dplyr::group_by(country, quarter_measurement, b_value, interest_awareness) %>%
#  dplyr::tally(wt=weight) %>%
#  dplyr::mutate(percentage_ING_clients_interest_aw = case_when(
#    interest_awareness == "interest_known_ING_clients" ~ n /sum(n) *100,
#    interest_awareness == "interest_notsure_ING_clients" ~ n /sum(n) *100,
#    interest_awareness == "interest_notaware_ING_clients" ~ n /sum(n) *100,
#  )) %>%
#  dplyr::select(country, quarter_measurement, b_value, interest_awareness, percentage_ING_clients_interest_aw) %>%
#  tidyr::spread(interest_awareness, percentage_ING_clients_interest_aw)
  
#data1 <- full_join(data1, interest_awareness_clients_ING, by=c("b_value", "country", "quarter_measurement"))

interest_action <- temp %>% 
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 2 ~ "Two",
      score == 3 ~ "Three",
      score == 4 ~ "Four",
      score == 5 ~ "Five",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_two = sum(weight[score_category == "Two"]),
    score_three = sum(weight[score_category == "Three"]),
    score_four = sum(weight[score_category == "Four"]),
    score_five = sum(weight[score_category == "Five"]),
    score_total = sum(weight) 
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    interest_action = ((score_one + score_two) / score_total) *100
  ) %>% 
  dplyr::filter(type == "interest_action_value")

interest_action$score_one <- NULL
interest_action$score_two <- NULL
interest_action$score_four <- NULL
interest_action$score_three <- NULL
interest_action$score_five <- NULL
interest_action$score_total <- NULL
interest_action$type <- NULL

data1 <- full_join(data1, interest_action, by=c("b_value", "country", "quarter_measurement"))
rm(interest_action)  

### app usage

app <- temp %>% 
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 2 ~ "Two",
      score == 3 ~ "Three",
      score == 4 ~ "Four",
      score == 5 ~ "Five",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(weight[score_category == "One"]),
    score_two = sum(weight[score_category == "Two"]),
    score_three = sum(weight[score_category == "Three"]),
    score_four = sum(weight[score_category == "Four"]),
    score_five = sum(weight[score_category == "Five"]),
    score_total = sum(weight) 
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    app_use = (score_one + score_two) / score_total,
    app_not_use = (score_five + score_four + score_three) / score_total
  ) %>% 
  dplyr::filter(type == "app_usage_value")

app$score_one <- NULL
app$score_two <- NULL
app$score_four <- NULL
app$score_three <- NULL
app$score_five <- NULL
app$score_total <- NULL
app$type <- NULL

data1 <- full_join(data1, app, by=c("b_value", "country", "quarter_measurement"))
rm(app)  

## split for app users
App_usage <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, nps_value, client_value, app_usage_value, main_bank_value) %>%
  #dplyr::filter(country ==9 | country ==3) %>%
  dplyr::filter(!is.na(nps_value) & client_value==1 & !is.na(app_usage_value)) %>%
  dplyr::mutate(app_usage = case_when(
    app_usage_value==1 ~ "Yes, at least five times",
    app_usage_value==2 ~ "Yes, but not very often, fewer than five times",
    app_usage_value==3 ~ "No, but I do have the app on my smartphone",
    app_usage_value==4 ~ "No, and I don’t have the app on my smartphone either",
    app_usage_value==5 ~ "No, I don’t have a smartphone",
    TRUE ~ "NA_real_"
  )) %>%
  dplyr::group_by(country, quarter_measurement, b_value, app_usage) %>%
  dplyr::tally(wt=weight) %>%
  dplyr::mutate(
    percentage = n /sum(n) *100,
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, quarter_measurement, b_value, app_usage, percentage) %>%
  dplyr::distinct() %>%
  #tibble::rowid_to_column() %>%
  tidyr::spread(app_usage, percentage) 

data1 <- full_join(data1, App_usage, by=c("b_value", "country", "quarter_measurement"))
rm(App_usage)

App_usage <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, nps_value, client_value, app_usage_value, main_bank_value) %>%
  #dplyr::filter(country ==9) %>%
  dplyr::filter(!is.na(nps_value) & client_value==1 & !is.na(app_usage_value)) %>%
  dplyr::filter(main_bank_value==1) %>%
  dplyr::mutate(app_usage = case_when(
    app_usage_value==1 ~ "MB_Yes, at least five times",
    app_usage_value==2 ~ "MB_Yes, but not very often, fewer than five times",
    app_usage_value==3 ~ "MB_No, but I do have the app on my smartphone",
    app_usage_value==4 ~ "MB_No, and I don’t have the app on my smartphone either",
    app_usage_value==5 ~ "MB_No, I don’t have a smartphone",
    TRUE ~ "NA_real_"
  )) %>%
  dplyr::group_by(country, quarter_measurement, b_value, app_usage) %>%
  dplyr::tally(wt=weight) %>%
  dplyr::mutate(
    percentage = n /sum(n) *100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, quarter_measurement, b_value, app_usage, percentage) %>%
  dplyr::distinct() %>%
  #tibble::rowid_to_column() %>%
  tidyr::spread(app_usage, percentage) 

# to add Q figures to data set, split for clients
data1 <- full_join(data1, App_usage, by=c("b_value", "country", "quarter_measurement"))

App_usage_easy <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, client_value, app_usage_value, easy_value, meets_needs_value) %>%
  dplyr::filter(client_value==1 & !is.na(app_usage_value) & b_value <6 & !is.na(easy_value)) %>%
  dplyr::mutate(app_usage = case_when(
    app_usage_value==1 ~ 1,
    app_usage_value==2 ~ 1,
    app_usage_value==3 ~ 0,
    app_usage_value==4 ~ 0,
    app_usage_value==5 ~ 0,
    TRUE ~ 0)
  ) %>% 
  dplyr::mutate(
    personal = meets_needs_value * weight,
    easy = easy_value * weight
  ) %>%
  dplyr::select(country, quarter_measurement, b_value, app_usage, easy, weight) %>%
  dplyr::group_by(quarter_measurement, country, b_value, app_usage) %>%
  dplyr::summarize(
    easy = mean(easy) / mean(weight)
  ) %>%
  dplyr::filter(!is.na(easy)) %>%
  dplyr::ungroup() %>%
  tidyr::spread(app_usage, easy) %>%
  dplyr::mutate(
    easy_non_app_users =App_usage_easy$`0`,
    easy_app_users =App_usage_easy$`1`
  )

App_usage_easy$`0` <- NULL
App_usage_easy$`1` <- NULL

data1 <- full_join(data1, App_usage_easy, by=c("b_value", "country", "quarter_measurement"))


price_awareness <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, client_value, interest_value, price_perception_value) %>%
  dplyr::filter(client_value==1 & !is.na(price_perception_value) & b_value <6 & !is.na(interest_value)) %>%
  dplyr::mutate(interest_awareness = case_when(
    interest_value==1 ~ 1,
    interest_value==2 ~ 0,
    interest_value==3 ~ 0,
    TRUE ~ 0)
  ) %>% 
  dplyr::mutate(
    price_perception_awareness = price_perception_value * weight
  ) %>%
  dplyr::select(country, quarter_measurement, b_value, interest_awareness, price_perception_awareness, weight) %>%
  dplyr::group_by(quarter_measurement, country, b_value, interest_awareness) %>%
  dplyr::summarize(
    price_perception_awareness = mean(price_perception_awareness) / mean(weight)
  ) %>%
  dplyr::filter(!is.na(price_perception_awareness)) %>%
  dplyr::ungroup() %>%
  tidyr::spread(interest_awareness, price_perception_awareness) %>%
  dplyr::mutate(
    price_unaware_interest =price_awareness$`0`,
    price_aware_interest =price_awareness$`1`
  )

price_awareness$`0` <- NULL
price_awareness$`1` <- NULL

data1 <- full_join(data1, price_awareness, by=c("b_value", "country", "quarter_measurement"))


### Labels
labels_countries <- data1 %>%
  dplyr::select(country) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(
    labels_countries = case_when(
      country == 1 ~ "Australia",
      country == 2 ~ "Austria",
      country == 3 ~ "Belgium",
      country == 4 ~ "Czech Republic",
      country == 5 ~ "France",
      country == 6 ~ "Germany",
      country == 7 ~ "Italy",
      country == 8 ~ "Luxembourg",
      country == 9 ~ "The Netherlands",
      country == 10 ~ "Poland",
      country == 11 ~ "Romania",
      country == 12 ~ "Spain",
      country == 13 ~ "Turkey",
      country == 14 ~ "The Philippines",
      TRUE ~ "NA_real_")
  )
data1 <- left_join(data1, labels_countries, by=c("country"))

# Important to add the new labels
labels_quarters <-data1 %>%
  dplyr::select(quarter_measurement) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(
    labels_quarters = case_when(
      quarter_measurement == 21 ~ "2020 Q1",
      quarter_measurement == 22 ~ "2020 Q2",
      quarter_measurement == 23 ~ "2020 Q3",
      quarter_measurement == 24 ~ "2020 Q4",
      TRUE ~ "NA_real_"))

data1 <- left_join(data1, labels_quarters, by=c("quarter_measurement"))

### dummy for main competition
main_competition <- data1 %>% 
  dplyr::mutate(
    main_competition = dplyr::case_when(
      country == 1 & b_value < 6 ~ 1,
      country == 2 & b_value < 7 ~ 1,
      country == 3 & b_value < 5 ~ 1,
      country == 4 & b_value < 6 ~ 1,
      country == 5 & b_value < 8 ~ 1,
      country == 6 & b_value < 7 ~ 1,
      country == 7 & b_value < 9 ~ 1,
      country == 8 & b_value < 7 ~ 1,
      country == 9 & b_value < 5 ~ 1,
      country == 10 & b_value < 7 ~ 1,
      country == 11 & b_value < 6 ~ 1,
      country == 12 & b_value < 7 ~ 1,
      country == 13 & b_value < 7 ~ 1,
      country == 14 & b_value < 8 ~ 1,
      TRUE ~ 0
    )
  )
data1 <- main_competition
rm(main_competition)

## Add brand names
X20200209_Competitorlist <- read.csv("20200209_Competitorlist.csv", sep=";") 

X20200209_Competitorlist$label <-str_replace_all(X20200209_Competitorlist$label, "[^[:alnum:]]", " ")
data1 <-left_join(data1, X20200209_Competitorlist, by=c("country", "b_value"))
rm(X20200209_Competitorlist)    

# name it data1 differently 
data_April <- data1
data_April <- write_excel_csv(data_April, "data_April")
data1 <-write_csv(data1, "data1.csv")
#resulthis <- write_csv(result, "resulthis.csv")

## Connect with "data" (history data Q4 2014- Q4 2019)

## First the data set which is leading. Otherwise columns will be dropped
test <- dplyr::bind_rows(data1, data)
test$labels_quarters <- as.yearqtr(unlist(test$labels_quarters), format='%Y Q%q')
test <-write_csv(test, "test.csv")

  


