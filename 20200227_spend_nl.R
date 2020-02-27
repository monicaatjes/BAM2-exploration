library(lubridate)

## Spending NL
spend<- read_excel("data/Nielsen ING en concurrenten wk1 2019-wk 2 2020-20200121.xlsx")

## add column date and quarter
spend$date<- as.Date(paste(spend$Jaar, spend$`Week Week code II`, 1, sep="-"), "%Y-%U-%u")
spend$labels_quarters <- as.yearqtr(spend$date, format ="%Y-%m-%d")

spend$Concern <- NULL
spend$Adverteerder <- NULL
spend$Productfamilie <- NULL
spend$Productgroep <- NULL
spend$Jaar <- NULL
spend$`Week Week code II` <- NULL
spend$Product <- NULL

spend <- spend %>%
  mutate(
    label = case_when(
      Merk =="ING BANK" ~ "ING",
      Merk =="RABOBANK" ~ "Rabobank",
      Merk =="SNS" ~ "SNS bank",
      Merk =="TRIODOS BANK" ~ "Triodos Bank",
      TRUE ~ "NA_real") 
    )

## Select NL data 

data_NL <- data %>%
  filter(country==9) %>%
  filter(quarter_measurement > 12) %>%
  distinct()

labels_quarters <-spend %>%
  dplyr::select(labels_quarters) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(
    labels_quarters = case_when(
      labels_quarters == "2018 Q1" ~ "Q1_2018",
      labels_quarters == "2018 Q2" ~ "Q2_2018",
      labels_quarters == "2018 Q3" ~ "Q3_2018",
      labels_quarters == "2018 Q4" ~ "Q4_2018",
      labels_quarters == "2019 Q1" ~ "Q1_2019",
      labels_quarters == "2019 Q2" ~ "Q2_2019",
      labels_quarters == "2019 Q3" ~ "Q3_2019",
      labels_quarters == "2019 Q4" ~ "Q4_2019",
      labels_quarters == "2020 Q1" ~ "Q1_2020",
      TRUE ~ "NA_real_"))

spend <- spend %>%
  dplyr::select('label', 'labels_quarters', 'Spend', 'Mediumtypen (magazines totaal)', 'Productklasse') %>%
  dplyr::mutate(
    labels_quarters = as.yearqtr(labels_quarters, format='Q%q_%Y')) %>%
  distinct()

data_NL <-full_join(data_NL, spend, by=c("labels_quarters", "label"))



  