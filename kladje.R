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
  
    
### Labels
data$labels_countries <-data %>%
  dplyr::select(country) %>%
  dplyr::mutate(
    labels_countries = case_when(
      country == 1 ~ "Australia",
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


data$labels_quarters <-data %>%
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
      quarter_measurement == 20 ~ "Q4_2019",
      TRUE ~ "NA_real_"))


x <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2013)
y_television <- c(74, 82, 80, 74, 73, 72, 74, 70, 70, 66, 66, 69)
y_internet <- c(13, 14, 20, 24, 20, 24, 24, 40, 35, 41, 43, 50)
dummydata <- data.frame(x, y_television, y_internet)

### TRIAL PLOTTING #########################################################################


xaxis <- list(title = "",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'inside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 5,
              tickfont = list(family = "ING me",
                              size = 12,
                              color = 'rgb(204, 204, 204)'))

yaxis <- list(title = "",
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              showticklabels = FALSE,
              range =c(0,10))
margin <- list(autoexpand = F)

# Build the annotations

love_ING_clients <- list(
  xref = '',
  yref = '',
  x = 0.05,
  y = love$love_mean_ING_client[1],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('love ING clients ', love_mean_ING_client[1], '%'),
  font = list(family = 'ING me',
              size = 16,
              color = 'rgba(67,67,67,1)'),
  showarrow = FALSE)

love_mean_ING <- list(
  xref = '',
  yref = '',
  x = 0.05,
  y = love$love_mean_ING[1],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('love ING general pop', love_mean_ING[1], '%'),
  font = list(family = 'ING me',
              size = 16,
              color = 'rgba(49,130,189, 1)'),
  showarrow = FALSE)

love_mean_Google <- list(
  xref = 'paper',
  x = 0.95,
  y = love$love_mean_Google[3],
  xanchor = 'left',
  yanchor = 'middle',
  text = paste('love Google', love$love_mean_Google[3], '%'),
  font = list(family = 'ING me',
              size = 16,
              color = 'rgba(67,67,67,1)'),
  showarrow = FALSE)

l<-love %>%
  plot_ly(x = ~x) %>%
  add_lines(y = ~love_mean_ING_client, type='scatter', name='love ING clients', 
            line = list(color = 'rgb(255,098,000)', type='scatter', width = 4, mode = 'markers')) %>%
  add_lines(y =~love_mean_Google, type='scatter', name='love Google',
            line = list(color = 'rgb(255,098,000)', width = 4, mode = 'markers')) %>%
  add_lines(y =~love_mean_ING, type='scatter', name='love ING') %>%
  layout(title = "love",
         xaxis = xaxis,
         yaxis = yaxis)


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

  