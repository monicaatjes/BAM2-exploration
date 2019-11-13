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
                            "reptrak3_value", "reptrak4_value")) %>% 
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) %>% 
  dplyr::mutate(
    score = dplyr::case_when(
      grepl("^reptrak", type) & score == 8 ~ NA_real_,
      TRUE ~ score
    )
  ) %>% 
  dplyr::group_by(b_value, country, quarter_measurement, type) %>% 
  dplyr::summarise(
    mean_score = mean(score * Weight, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    rep_trak1 = dplyr::case_when(
      grepl("^reptrak1_value", type) ~ mean_score / 7,
      TRUE ~ mean_score)) %>%
  dplyr::mutate(
    rep_trak2 = dplyr::case_when(
      grepl("^reptrak2_value", type) ~ mean_score / 7,
      TRUE ~ mean_score)) %>%
  dplyr::mutate(
    rep_trak3 = dplyr::case_when(
      grepl("^reptrak3_value", type) ~ mean_score / 7,
      TRUE ~ mean_score)) %>%
  dplyr::mutate(
    rep_trak4 = dplyr::case_when(
    grepl("^reptrak4_value", type) ~ mean_score / 7,
    TRUE ~ mean_score
    )
  ) 

reptrak_statements<- reptrak_statements[,!names(reptrak_statements) %in% c("type","mean_score")]
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


### nps ###
### dont forget to filter for clients!
temp4 <- result %>%
  dplyr::filter(!is.na(nps_value)) %>%
  #dplyr::filter(nps_value %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) %>% 
  dplyr::mutate(
    score_category = case_when(
      nps_value == 0 ~ "Zero",
      nps_value == 1 ~ "One",
      nps_value == 2 ~ "Two",
      nps_value == 3 ~ "Three",
      nps_value == 4 ~ "Four",
      nps_value == 5 ~ "Five",
      nps_value == 6 ~ "Six",
      nps_value == 7 ~ "Seven",
      nps_value == 8 ~ "Eight",
      nps_value == 9 ~ "Nine",
      nps_value == 10 ~ "Ten",
      TRUE ~ "No Score"
    )
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement, client_value) %>%
  dplyr::summarize(
    score_one = sum(weight_nps[score_category == "One"]),
    score_zero = sum(weight_nps[score_category == "Zero"]),
    score_two = sum(weight_nps[score_category == "Two"]),
    score_three = sum(weight_nps[score_category == "Three"]),
    score_four = sum(weight_nps[score_category == "Four"]),
    score_five = sum(weight_nps[score_category == "Five"]),
    score_six = sum(weight_nps[score_category == "Six"]),
    score_seven = sum(weight_nps[score_category == "Seven"]),
    score_eight = sum(weight_nps[score_category == "Eight"]),
    score_nine = sum(weight_nps[score_category == "Nine"]),
    score_ten = sum(weight_nps[score_category == "Ten"]),
    score_total = sum(weight_nps)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    detractors = (score_one + score_zero + score_two + score_three + score_four + score_five + score_six) / score_total,
    neutrals = (score_seven + score_eight) / score_total,
    promotors = (score_nine + score_ten) / score_total,
    nps = promotors - detractors
  ) 

temp4$score_one <- NULL
temp4$score_zero <- NULL
temp4$score_two <- NULL
temp4$score_three <- NULL
temp4$score_four <- NULL
temp4$score_five <- NULL
temp4$score_six <- NULL
temp4$score_seven <- NULL
temp4$score_eight <- NULL
temp4$score_nine <- NULL
temp4$score_ten <- NULL
temp4$score_total <- NULL
temp4$type <- NULL

temp4$client <- temp4$client_value
data <- full_join(data, temp4, by=c("b_value", "country", "quarter_measurement", "client"))
rm(temp4)

temp_nps <- temp4 %>%
  dplyr::select("nps", "b_value", "country", "quarter_measurement", "client") %>%
  dplyr::filter(!is.na(nps), b_value==1, quarter_measurement==19, client==1) 

## NPS
nps <- data %>%
  dplyr::select("nps", "b_value", "country", "quarter_measurement", "client") %>%
  dplyr::filter(!is.na(nps), b_value==1, quarter_measurement==19, client==1) 

## does not work yet

## consideration
consideration<- data %>%
  dplyr::select("preference", "country", "quarter_measurement", "b_value") %>%
  dplyr::filter(quarter_measurement==19, b_value==1) 



write.csv(data, file="data.csv")



