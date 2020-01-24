### exploration

### apply function to change names ###
raw_data <- changed_messed_up_variable_names(raw_data, cat = "product_usage")

#### ADDING QUESTIONNAIRE RESPONSES  ####

# Add all the cats to our long data frame

for (x in categories) {
  
  if (x == categories[1]){
    result <- raw_data %>%
      dplyr::select(X1, ID, quarter_measurement, country, age, gender, income, Weight, weight_nps, love_ING, love_Google) %>%
      add_questionaire_responds_category(raw_data, x)
  }
  else{
    print(x)
    result <- add_questionaire_responds_category(
      base_data = result, 
      raw_data = raw_data, 
      cat = x
    )
  }
}

### make sure that laptop will not explode
rm(base_data)
rm(raw_data)

##### tidy up
# gather further to get all the scores in one column to reshape the data 
temp <- result %>% 
  select(-X1, -ID) %>% 
  gather("type", "score", -quarter_measurement, -country, -age, -gender, -income, -Weight, -weight_nps, -love_ING, -love_Google, -b_value)

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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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
  dplyr::filter(score %in% c(1, 2, 99)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 2 ~ "Two",
      score == 99 ~ "Nine Nine",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_nine_nine = sum(Weight[score_category == "Nine Nine"]),
    score_total = sum(Weight)
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

data <- full_join(temp3, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp3, temp4)

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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### opinion
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    opinion = score_one / score_total
  ) %>% 
  dplyr::filter(type == "opinion_value")

temp4$score_one <- NULL
temp4$score_zero <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### image1
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image1 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image1_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### image2
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image2 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image2_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### image3
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image3 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image3_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### image4
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image4 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image4_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### image5
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image5 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image5_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### image6
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image6 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image6_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### image7
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image7 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image7_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### image13
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image13 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image13_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### image16
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image16 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image16_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### image19
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    image19 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "image19_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### trust4
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    trust4 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "trust4_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### empower1
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    empower1 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "empower_1_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### empower2
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    empower2 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "empower_2_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### empower3
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    empower3 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "empower_3_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### empower4
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    empower4 = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "empower_4_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### love_respected
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    love_respected = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "love_respected_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### love_meaningful
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    love_meaningful = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "love_meaningful_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### love_irresistible
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    love_irresistible = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "love_irresistible_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### love_irreplaceable
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_three = sum(Weight[score_category == "Three"]),
    score_four = sum(Weight[score_category == "Four"]),
    score_five = sum(Weight[score_category == "Five"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    love_irreplaceable = (score_four + score_five) / score_total
  ) %>% 
  dplyr::filter(type == "love_irreplaceable_value")

temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### consideration_p1
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    consideration_p1 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "consideration_p1_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### consideration_p2
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    consideration_p2 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "consideration_p2_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### consideration_p3
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    consideration_p3 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "consideration_p3_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### consideration_p4
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    consideration_p4 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "consideration_p4_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### preference_p1
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    preference_p1 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "preference_p1_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### preference_p2
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    preference_p2 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "preference_p2_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### preference_p3
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    preference_p3= score_one / score_total
  ) %>% 
  dplyr::filter(type == "preference_p3_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### preference_p4
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    preference_p4= score_one / score_total
  ) %>% 
  dplyr::filter(type == "preference_p4_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_awareness_p1
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_awareness_p1 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_awareness_p1_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_awareness_p2
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_awareness_p2 = score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_awareness_p2_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_awareness_p3
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_awareness_p3= score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_awareness_p3_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### product_awareness_p4
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    product_awareness_p4= score_one / score_total
  ) %>% 
  dplyr::filter(type == "product_awareness_p4_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
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

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### message_recall
temp4 <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(score %in% c(0, 1, 2)) %>% 
  dplyr::mutate(
    score_category = case_when(
      score == 1 ~ "One",
      score == 0 ~ "Zero",
      score == 2 ~ "Two",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::mutate(
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_two = sum(Weight[score_category == "Two"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    message_recall= (score_one + score_two) / score_total
  ) %>% 
  dplyr::filter(type == "message_recall_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_two <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### desirability
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
    score = score * Weight
  ) %>%
  dplyr::group_by(type, b_value, country, quarter_measurement) %>%
  dplyr::summarize(
    score_one = sum(Weight[score_category == "One"]),
    score_zero = sum(Weight[score_category == "Zero"]),
    score_total = sum(Weight)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
   desirability= score_one / score_total
  ) %>% 
  dplyr::filter(type == "desirability_value")

temp4$score_zero <- NULL
temp4$score_one <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement"))
rm(temp4)

### reptrak #########################################
reptrak_statements <- temp %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::filter(type %in% c("reptrak1_value", "reptrak2_value", 
                            "reptrak3_value", "reptrak4_value"
                           # "reptrak5_value", "reptrak6_value", 
                           # "reptrak7_value", "reptrak8_value",
                           # "reptrak9_value", "reptrak10_value", 
                           # "reptrak11_value"
                            )) %>% 
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7)) %>%
  dplyr::group_by(b_value, country, quarter_measurement, type) %>% 
  dplyr::summarise(
    mean_score = mean(score * Weight, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    rep_trak1 = dplyr::case_when(
      grepl("^reptrak1", type) ~ mean_score / 7,
      TRUE ~ NA_real_)) %>%
  dplyr::mutate(
    rep_trak2 = dplyr::case_when(
      grepl("^reptrak2", type) ~ mean_score / 7,
      TRUE ~ NA_real_)) %>%
  dplyr::mutate(
    rep_trak3 = dplyr::case_when(
      grepl("^reptrak3", type) ~ mean_score / 7,
      TRUE ~ NA_real_)) %>%
  dplyr::mutate(
    rep_trak4 = dplyr::case_when(
    grepl("^reptrak4", type) ~ mean_score / 7,
    TRUE ~ NA_real_)) 
  #dplyr::mutate(
 # rep_trak5 = dplyr::case_when(
 #   grepl("^reptrak5", type) ~ mean_score / 7,
 #   TRUE ~ NA_real_)) %>%
 # dplyr::mutate(
 #   rep_trak6 = dplyr::case_when(
 #     grepl("^reptrak6", type) ~ mean_score / 7,
 #     TRUE ~ NA_real_)) %>%
 # dplyr::mutate(
 #   rep_trak7 = dplyr::case_when(
 #     grepl("^reptrak7", type) ~ mean_score / 7,
 #     TRUE ~ NA_real_)) %>%
 # dplyr::mutate(
 #   rep_trak8 = dplyr::case_when(
 #     grepl("^reptrak8", type) ~ mean_score / 7,
 #     TRUE ~ NA_real_)) %>%
 # dplyr::mutate(
 #   rep_trak9 = dplyr::case_when(
 #     grepl("^reptrak9", type) ~ mean_score / 7,
 #     TRUE ~ NA_real_)) %>%
 # dplyr::mutate(
 #   rep_trak10 = dplyr::case_when(
 #     grepl("^reptrak10", type) ~ mean_score / 7,
 #     TRUE ~ NA_real_)) %>%
 # dplyr::mutate(
 #   rep_trak11 = dplyr::case_when(
 #     grepl("^reptrak11", type) ~ mean_score / 7,
 #     TRUE ~ NA_real_
 #    )
 #  ) 

reptrak_statements <- reptrak_statements %>%
  dplyr:select(b_value, quarter_measurement, country, rep_trak1, rep_trak2, rep_trak3, rep_trak4)

reptrak_statements<- reptrak_statements[,!names(reptrak_statements) %in% c("type","mean_score")]
reptrak_statements <- reptrak_statements$rep_trak1[!is.na(reptrak_statements$rep_trak1)]
data <- full_join(data, reptrak_statements, by=c("b_value", "country", "quarter_measurement"))
rm(reptrak_statements) 

love_overview <- result %>%
  dplyr::filter(!is.na(love_ING)) %>%
  dplyr::filter(love_ING %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) %>% 
  dplyr::group_by(country, quarter_measurement) %>% 
  dplyr::summarise(
    love_mean_ING = mean(love_ING * Weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data <- full_join(data, love_overview, by=c("country", "quarter_measurement"))
rm(love_overview) 

google_overview <- result %>%
  dplyr::filter(!is.na(love_Google)) %>%
  dplyr::filter(love_Google %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) %>% 
  dplyr::group_by(country, quarter_measurement) %>% 
  dplyr::summarise(
    love_mean_Google = mean(love_Google * Weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data <- full_join(data, google_overview, by=c("country", "quarter_measurement"))
rm(google_overview) 

proximity_overview <- result %>%
  dplyr::filter(!is.na(proxi_value)) %>%
  dplyr::filter(proxi_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::group_by(country, quarter_measurement, b_value) %>% 
  dplyr::summarise(
    proximity = mean(proxi_value * Weight, na.rm = T) 
  ) %>%
  dplyr::ungroup()

data <- full_join(data, proximity_overview, by=c("country", "quarter_measurement", "b_value"))
rm(proximity_overview) 

### NPS ###
NPS_trial <-result %>%
  dplyr::filter(!is.na(nps_value)) %>%
  dplyr::group_by(country, quarter_measurement, b_value) %>%
  dplyr::mutate(
    nps_clients = case_when(
      client_value == 1 ~ nps_value * weight_nps,
      TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(nps_cat = case_when(
    nps_clients >=9.0 ~ "promotors",
    nps_clients <= 9.0 & nps_clients >= 7.0 ~ "neutrals",
    nps_clients  <= 6.0 ~ "detractors",
    TRUE ~ "NA_real_")) %>%
  dplyr::group_by(nps_cat, country, b_value, quarter_measurement) %>%
  dplyr::tally() %>%
  dplyr::mutate(
    percentage = n /sum(n)
  ) %>% 
  dplyr::ungroup()

NPS_result <- NPS_trial %>%
  dplyr::filter(nps_cat %in% c("promotors", "detractors", "neutrals")) %>%
  dplyr::group_by(country, quarter_measurement, b_value) %>%
  dplyr::mutate(
    nps = percentage[nps_cat=="promotors"] - percentage[nps_cat=="detractors"]
  ) %>%
  dplyr::ungroup()
  

### Desirability

desi <- data %>%
  dplyr::select(b_value, labels_countries, labels_quarters, desirability) %>%
  dplyr::filter(b_value %in% c(1)) %>%
  dplyr::group_by(b_value, labels_countries, labels_quarters) %>%
  dplyr::distinct()

### clients

clients <- data %>%
  dplyr::select(b_value, labels_countries, labels_quarters, desirability) %>%
  dplyr::filter(b_value %in% c(1)) %>%
  dplyr::group_by(b_value, labels_countries, labels_quarters) %>%
  dplyr::distinct()

desi$labels_countries <-desi %>%
  dplyr::select(country) %>%
  dplyr::mutate(
    labels_countries = case_when(
      country == 1 ~ "Autralia",
      country == 2 ~ "Austria",
      country == 3 ~ "Belgium",
      country == 4 ~ "Czech",
      country == 5 ~ "France",
      country == 6 ~ "Germany",
      country == 7 ~ "Italy",
      country == 8 ~ "Luxembourg",
      country == 9 ~ "Netherlands",
      country == 10 ~ "Poland",
      country == 11 ~ "Romania",
      country == 12 ~ "Spain",
      country == 13 ~ "Turkey",
      country == 14 ~ "The Philippines",
      TRUE ~ "NA_real_"))
data$labels_countries <- labels_countries$labels_countries

desi$labels_quarters <-desi %>%
  dplyr::select(quarter_measurement) %>%
  dplyr::mutate(
    labels_quarters = case_when(
      quarter_measurement == 0 ~ "Q4_2014",
      quarter_measurement == 1 ~ "Q1_2015",
      quarter_measurement == 2 ~ "Q2_2015",
      quarter_measurement == 3 ~ "Q3_2015",
      quarter_measurement == 4 ~ "Q4_2015",
      quarter_measurement == 5 ~ "Q1_2016",
      quarter_measurement == 6 ~ "Q2_2016",
      quarter_measurement == 7 ~ "Q3_2016",
      quarter_measurement == 8 ~ "Q4_2016",
      quarter_measurement == 9 ~ "Q1_2017",
      quarter_measurement == 10 ~ "Q2_2017",
      quarter_measurement == 11 ~ "Q3_2017",
      quarter_measurement == 12 ~ "Q4_2017",
      quarter_measurement == 13 ~ "Q1_2018",
      quarter_measurement == 14 ~ "Q2_2018",
      quarter_measurement == 15 ~ "Q3_2018",
      quarter_measurement == 16 ~ "Q4_2018",
      quarter_measurement == 17 ~ "Q1_2019",
      quarter_measurement == 18 ~ "Q2_2019",
      quarter_measurement == 19 ~ "Q3_2019",
      TRUE ~ "NA_real_"))
data$labels_quarters <- labels_quarters$labels_quarters

desi %>%
  filter(labels_quarters=="Q3_2019") %>%
  plot_ly(x = ~labels_countries, y =~desirability *100, type='bar',
          marker = list(color = 'rgb(255,098,000)', width = 1.5)) %>%
  layout(title = "Desirability",
         xaxis = list(title = "countries"),
         yaxis = list(title = "desirability", range= c(0,100),
         annotations = annotations)
         )

desi$labels_countries <- labels_countries$labels_countries

annotations <- list()
for (i in 1:length(desi$desirability)) {
  annotations[[i]] <- list(y = desi$desirability[[i]],
                           text = desi$desirability[[i]],
                           yanchor='bottom',
                           showarrow = FALSE)
}

value <- data %>%
  dplyr::select(b_value, labels_countries, labels_quarters, image7) %>%
  dplyr::filter(b_value %in% c(1)) %>%
  dplyr::filter(labels_quarters %in% c("Q1_2019","Q2_2019", "Q3_2019")) %>%
  dplyr::distinct()

write.table(value, "/Users/xo21bm/Documents/Lokaal/BAM2/exploration/value.txt", sep=",")


Awareness <- data %>%
  dplyr::select(b_value, country, quarter_measurement, toma, aided, unaided) %>%
  dplyr::filter(b_value==1, country==6) %>%
  dplyr::group_by(quarter_measurement) %>%
  dplyr::distinct() 

"C:/Lokaal/BAM2/exploration/Awareness.csv"