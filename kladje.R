## klad blaadje
# Goal write NPS code

NPS <- result %>%
  dplyr::filter(!is.na(nps_value)) %>%
  dplyr::filter(score %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>% 
  #dplyr::summarise(
   # mean_score = mean(score * weight_nps, na.rm = T)
  #) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    NPS = dplyr::case_when(
      client_value ==1 ~ nps_value
      grepl("^reptrak11", type) ~ mean_score / 7,
      TRUE ~ NA_real_
    )
  ) 

case_when(
  x %% 35 == 0 ~ 35,
  x %% 5 == 0 ~ 5,
  x %% 7 == 0 ~ 7,
  TRUE ~ NA_real_
)

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
  dplyr::group_by(country, quarter_measurement, b_value) %>%
  dplyr::tally() %>%
  dplyr::mutate(
    promotors_perc = n[nps_cat=="promotors"] /sum(n),
    neutral_perc =n[nps_cat=="neutrals"]/ sum(n),
    detractors = n[nps_cat=="detractors"]/ sum(n),
  ) 
  
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

NPS_output <- NPS_trial %>% 
  dplyr::filter(nps_cat %in% c("promotors", "detractors")) %>% 
  tidyr::spread("nps_cat", "percentage") %>% 
  dplyr::mutate(
    promotors = dplyr::lead(promotors)
  ) %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate(
    nps = promotors - detractors
  ) %>% 
  dplyr::select(-detractors, -promotors, -n)


check <- data %>%
  dplyr::select(unaided, toma, aided, fami, opinion, consideration, preference, quarter_measurement, b_value, country) %>%
  dplyr::filter(quarter_measurement ==20 & b_value==1 & country==1)

check <- data %>%
  dplyr::select(unaided, toma, aided, fami, opinion, consideration, preference, quarter_measurement, b_value, country) %>%
  dplyr::filter(quarter_measurement ==20 & country==1) %>%
  dplyr::mutate(
    unaided_best = max(unaided, na.rm=T), 
    aided_best = max(aided, na.rm=T), 
    toma_best = max(toma, na.rm=T), 
    fami_best = max(fami, na.rm=T), 
    opinion_best = max(opinion, na.rm=T), 
    consideration_best = max(opinion, na.rm=T), 
    preference_best = max(opinion, na.rm=T), 
  ) %>%
  unique()
  group_by(b_value)

    
  
result <- df %>% 
  group_by(A, B) %>%
  filter(value == max(value)) %>%
  arrange(A,B,C)


### NPS ###
NPS_trial <-result %>%
  dplyr::select(client_value, main_bank_value, nps_value, quarter_measurement, country, b_value, weight_nps) %>%
  dplyr::filter(!is.na(nps_value)) %>%
  #dplyr::group_by(country, quarter_measurement, b_value) %>%
  dplyr::mutate(
    nps_clients = case_when(
      client_value == 1 ~ nps_value * weight_nps,
      TRUE ~ NA_real_)
  ) %>%
# this case when does what it should do
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

NPS_trial_check <- NPS_trial %>%
  dplyr::select(country, b_value, quarter_measurement, n, nps_cat) %>%
  dplyr::filter(country==1 & b_value==1, quarter_measurement==20) 
  
    
  
table(NPS_trial$nps_cat[NPS_trial$country==1])



  