# BEFORE RUNNING CHANGE WORKING DIRECTORY TO SOURCE FILE LOCATION
# Dependencies
library(tidyverse)
source("add_questionnaire_responds_category.R")
source("changed_messed_up_variable_names.R")
## Load and transform BAM data Q1



raw_data2 <- raw_data2 %>%
  dplyr::select(-contains("SPSS"))

# Add in X1 which is a row count
raw_data1 <- raw_data2 %>% 
  dplyr::mutate(
    X1 = 1:dplyr::n()
  )

ID_char1 <- c("X1", "quarter_measurement", "country", "age", "age_groups", "gender", 
              "weight", "Love_ING", "Love_Google", "freedom_segment")

base_data1 <- raw_data1 %>% 
  dplyr::select(ID_char1)

# Wanted categories
categories <- c("unaided", "aided", "fami", "consideration", "preference", "relationship", "desirability",
                "price_perception", "easy", "smart", "meets_needs", "nps", "reptrak1", "reptrak2", "reptrak3", 
                "reptrak4", "reptrak5", "reptrak6", "reptrak7", "reptrak8", "reptrak9", "reptrak10", "reptrak11",  
                "trust4", "image20", "freedom_statement", "digital_experience", "app_usage", "client", 
                "ad_awareness", "main_bank", "product_usage_p1", "product_usage_p2", "product_usage_p3", 
                "product_usage_p4", "product_usage_p5", "product_usage_p6", "product_usage_p7", 
                "product_usage_p8", "product_usage_p9")
                
              
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
                    freedom_segment, Love_Google) %>%
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
    unaided = (score_one + score_two) / score_total,
    toma = score_one / score_total
  ) %>% 
  dplyr::filter(type == "unaided_value")

temp3$score_one <- NULL
temp3$score_zero <- NULL
temp3$score_total <- NULL
temp3$score_two <- NULL
temp3$score_nine_nine <- NULL
temp3$type <- NULL

data1 <- full_join(temp4, temp3, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

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

## meets my needs
meets_needs_overview <- result %>%
  dplyr::select(country, quarter_measurement, b_value, meets_needs_value, weight) %>%
  dplyr::filter(!is.na(meets_needs_value)) %>%
  dplyr::group_by(country, quarter_measurement, b_value) %>% 
  dplyr::summarise(
    meets_needs_mean = mean(meets_needs_value * weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, meets_needs_overview, by=c("country", "quarter_measurement", "b_value"))
rm(meets_needs_overview) 

## smart
smart_overview <- result %>%
  dplyr::select(country, quarter_measurement, b_value, smart_value, weight) %>%
  dplyr::filter(!is.na(smart_value)) %>%
  dplyr::group_by(country, quarter_measurement, b_value) %>% 
  dplyr::summarise(
    smart_mean = mean(smart_value * weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data1 <- full_join(data1, smart_overview, by=c("country", "quarter_measurement", "b_value"))
rm(smart_overview) 

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
  #dplyr::filter(Love_ING %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) %>% 
  dplyr::group_by(country, quarter_measurement) %>% 
  dplyr::summarise(
    love_mean_ING_client = mean(Love_ING * weight) 
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

## NPS
#NPS_trial <- result %>%
#  dplyr::filter(!is.na(nps_value)) %>%
#  dplyr::group_by(country, quarter_measurement, b_value, client_value) %>%
#  dplyr::mutate(
#    nps_clients = case_when(
#      client_value == 1 ~ as.numeric(nps_value) * weight,
#      TRUE ~ NA_real_)
#  ) %>%
#  dplyr::mutate(nps_cat = case_when(
#    nps_clients >=9.0 ~ "promotors",
#    nps_clients <= 9.0 & nps_clients >= 7.0 ~ "neutrals",
#    nps_clients  <= 6.0 ~ "detractors",
#    TRUE ~ "NA_real_")) %>%
#  dplyr::filter(nps_cat %in% c("detractors", "neutrals", "promotors")) %>%
#  dplyr::group_by(country, b_value, quarter_measurement, nps_cat) %>%
#  dplyr::tally() %>%
#  dplyr::mutate(
#    percentage = n /sum(n)
#  ) %>% 
#  dplyr::ungroup()

#NPS_score <- NPS_trial %>%
#  select(-n) %>%
#  tidyr::spread(nps_cat, percentage) %>%
#  dplyr::mutate(
#    nps_score = promotors - detractors
#  )

#data1 <- left_join(NPS_score, data1, by=c("country", "b_value", "quarter_measurement"))

#rm(NPS_trial, NPS_score)

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

labels_quarters <-data1 %>%
  dplyr::select(quarter_measurement) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(
    labels_quarters = case_when(
      quarter_measurement == 21 ~ "2020 Q1",
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
      country == 6 & b_value < 6 ~ 1,
      country == 7 & (b_value < 8 | b_value == 10) ~ 1,
      country == 8 & b_value < 7 ~ 1,
      country == 9 & b_value < 5 ~ 1,
      country == 10 & b_value < 7 ~ 1,
      country == 11 & b_value < 6 ~ 1,
      country == 12 & b_value < 5 ~ 1,
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

data1 <-write_csv(data1, "data1.csv")
#resulthis <- write_csv(result, "resulthis.csv")

## Connect with current dataset

test <- dplyr::bind_rows(data1, data)
test$labels_quarters <- as.yearqtr(unlist(test$labels_quarters), format='%Y Q%q')
test <-write_csv(test, "test.csv")
