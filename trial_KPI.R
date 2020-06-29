

desi_target <- test %>%
  dplyr::select(quarter_measurement, labels_quarters, b_value, main_competition, labels_countries, country, desirability,
                'RepTrak® Pulse', pulse) %>%
  dplyr::filter(main_competition==1) %>%
  dplyr::filter(quarter_measurement >=15) %>%
  dplyr::group_by(country, quarter_measurement) %>%
  dplyr::mutate(
    desi_MA = mean(desirability)
  ) %>%
  dplyr::filter(b_value==1) %>%
  dplyr::group_by(country) %>%
  dplyr::arrange(quarter_measurement) %>%
  dplyr::mutate(
    desi_MA_RA = rollapply(desi_MA, FUN = mean, width =4, na.pad=T, descending=T, align="right")
  ) %>%
  dplyr::mutate(
    diff_brand_MA = desirability - desi_MA
    ) %>%
  dplyr::filter(b_value==1) %>%
  dplyr::mutate(
    diff_brand_MA_RA = rollapply(diff_brand_MA, FUN = mean, width =4, na.pad=T, descending=T, align="right")
  ) %>%
  dplyr::mutate(
    ING_RA = rollapply(desirability, FUN = mean, width =4, na.pad=T, descending=T, align="right")
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    target_desirability = case_when(
      labels_countries == "France" | labels_countries == "Australia" | labels_countries == "Austria" |
      labels_countries == "Belgium" | labels_countries == "Czech Republic" ~ 0,
      labels_countries == "Germany" ~ 13,
      labels_countries == "Italy" ~ 8.2,
      labels_countries == "The Netherlands" ~ 12.7,
      labels_countries == "Poland" ~ 10.7,
      labels_countries == "Romania" ~ 13.6,
      labels_countries == "Spain" ~ 17.1,
      labels_countries == "Turkey" ~ -6.7,
      labels_countries == "The Philippines" ~ -2.9,
      labels_countries == "Luxembourg" ~ -2.6
    )) %>%
  dplyr::mutate(
    pulse = case_when(
      country == 1 ~ pulse,
      country == 2 ~ `RepTrak® Pulse`,
      country == 3 ~ `RepTrak® Pulse`,
      country == 4 ~ pulse,
      country == 5 ~ pulse,
      country == 6 ~ `RepTrak® Pulse`,
      country == 7 ~ pulse,
      country == 8 ~ pulse,
      country == 9 ~ `RepTrak® Pulse`,
      country == 10 ~ pulse,
      country == 11 ~ pulse,
      country == 12 ~ pulse,
      country == 13 ~ pulse,
      country == 14 ~ pulse,
      TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(pulse = case_when(
    country ==14 & quarter_measurement==19 ~ pulse /1.603,
    TRUE ~ pulse
  )) 

desi_target$pulse_target <- 70

## spread for quarter
#desi
des_try <- desi_target %>%
  dplyr::filter(labels_quarters >="2019 Q4") %>%
  dplyr::select(labels_quarters, labels_countries, diff_brand_MA_RA, target_desirability) %>%
  tidyr:: gather(variable, value, -c(labels_quarters, labels_countries, target_desirability)) %>%
  unite(temp, labels_quarters, variable) %>%
  spread(temp, value) %>%
  dplyr::mutate(
    `2020 Q1_diff_brand_MA_RA` = round(des_try$`2020 Q1_diff_brand_MA_RA`, digits=2),
    `2019 Q4_diff_brand_MA_RA` = round(des_try$`2019 Q4_diff_brand_MA_RA`, digits=2),
    variance_target_desi = round(des_try$`2020 Q1_diff_brand_MA_RA`, digits=2) - round(des_try$target_desirability, digits=2)
  )

rep_try <- desi_target %>%
  dplyr::filter(labels_quarters >="2019 Q4") %>%
  dplyr::select(labels_quarters, labels_countries, pulse, pulse_target) %>%
  tidyr::gather(variable, value, -c(labels_quarters, labels_countries, pulse_target)) %>%
  unite(temp, labels_quarters, variable) %>%
  spread(temp, value) %>%
  dplyr::mutate(
    `2020 Q1_pulse` = round(rep_try$`2020 Q1_pulse`, digits=2),
    `2019 Q4_pulse` = round(rep_try$`2019 Q4_pulse`, digits=2),
    variance_target_pulse = round(rep_try$`2020 Q1_pulse`, digits=2) - round(rep_try$pulse_target, digits=2)
  )

## Connect together
overview_tab <-left_join(des_try, rep_try, by=c("labels_countries"))

## internal
internal1 <- internal %>%
  dplyr::select(labels_countries, `2019 Q3`, `2019 Q4`, `2020 Q1`, `2020 Q2`) %>%
  dplyr::filter(!is.na(`2019 Q4`) & 
                  !labels_countries=="Americas" &
                  !labels_countries=="European Network" & 
                  !labels_countries=="Asia" &
                  !labels_countries=="UK region" &
                  !labels_countries=="Austria") 

internal1 <- internal1 %>%
  dplyr::mutate(
    '2019 Q3_nps'= round(internal1$'2019 Q3', digits=4) *100,
    '2019 Q4_nps'= round(internal1$'2019 Q4', digits=4) *100,
    '2020 Q1_nps'= round(internal1$'2020 Q1', digits=4) *100,
    '2020 Q2_nps'= round(internal1$'2020 Q2', digits=4) *100) 

internal1$target_internal = internal1$`2019 Q3_nps` + 5


internal1 <- internal1 %>%
  dplyr::mutate(
    variance_target_nps = round(`2020 Q2_nps`, digits=2) - round(target_internal, digits=2)
  ) 
  internal1 <- internal1 %>%
  dplyr::select(labels_countries, target_internal,'2020 Q1_nps', '2020 Q2_nps', variance_target_nps)


## Connect together
overview_tab <-left_join(overview_tab, internal1, by=c("labels_countries"))

## play aroynd wit gt table

tab_style(
  style= list(
    cell_text(weight = "normal", font = "ING me", align="left")
  ),
  locations = tab_header()
# Create a gt table based on preprocessed

tab <-  overview_tab %>%
  gt() %>%
  tab_header(
    title = md("**Brand KPI's**"),
    subtitle = "summary table"
  ) %>%
  tab_spanner(
    label = "Desirability",
    columns = vars("2019 Q4_diff_brand_MA_RA", "2020 Q1_diff_brand_MA_RA", "target_desirability", "variance_target_desi"),
  ) %>%
  tab_spanner(
    label = "Reputation",
    columns = vars("2019 Q4_pulse" , "2020 Q1_pulse" , "pulse_target", "variance_target_pulse")
  ) %>%
  tab_spanner(
    label = "Internal NPS",
    columns = vars("2020 Q1_nps" , "2020 Q2_nps" , "target_internal", "variance_target_nps")
  ) %>%
  tab_source_note(
    source_note = "Source: Brand Affinity Monitor, Reptrek, Internal NPS"
  ) %>%
  cols_label(
    labels_countries = md("**Country**"),
    `2019 Q4_diff_brand_MA_RA` = md("**2019_Q4**"),
    `2020 Q1_diff_brand_MA_RA` = md("**2020_Q1**"),
    target_desirability = md("**target**"),
    variance_target_desi = md("**var.vs.target**"),
    `2019 Q4_pulse` = md("**2019_Q4**"),
    `2020 Q1_pulse` = md("**2020_Q1**"),
    pulse_target = md("**target**"),
    variance_target_pulse = md("**var.vs.target**"),
    `2020 Q1_nps`= md("**2020_Q1**"),
    `2020 Q2_nps` = md("**2020_Q2**"),
    target_internal = md("**target**"),
    variance_target_nps = md("**var.vs.target**")
  ) %>%
  tab_style(
    style = list(
      #cell_fill(color = ""),
      cell_text(weight = "normal", font = "ING me")
    ),
    locations = cells_body(
      columns = vars(
        labels_countries, 
        "2019 Q4_diff_brand_MA_RA", "2020 Q1_diff_brand_MA_RA", "target_desirability" , "variance_target_desi",
        "2019 Q4_pulse", "2020 Q1_pulse", "pulse_target", "variance_target_pulse",
        "2020 Q1_nps",  "2020 Q2_nps", "target_internal", "variance_target_nps"  
        )
      )
  ) %>%
  tab_style(
    style = list(
      #cell_fill(color = ""),
      cell_text(weight = "normal", font = "ING me", align="left")
    ),
    locations = cells_column_labels(
      columns = vars(
        labels_countries, 
        "2019 Q4_diff_brand_MA_RA", "2020 Q1_diff_brand_MA_RA", "target_desirability" , "variance_target_desi",
        "2019 Q4_pulse", "2020 Q1_pulse", "pulse_target", "variance_target_pulse",
        "2020 Q1_nps",  "2020 Q2_nps", "target_internal", "variance_target_nps"  
      )
    )
  ) %>%
  tab_style(
    style = list(
      #cell_fill(color = ""),
      cell_text(weight = "normal", font = "ING me", align="left")
    ),
    locations = cells_column_spanners(
      spanners =(c("Desirability", "Reputation", "Internal NPS"))
      )
    ) %>%
  tab_style(
    style= list(
      #cell_fill(col = 'rgb(255,098,000)'),
      cell_text(weight = "normal", font = "ING me", align="left")
    ),
    locations = cells_title("Brand KPI's")
  ) %>%
 
      
  ) %>%
  text_transform(
    locations = cells_body(
      columns = "var.vs.target",
      rows = '2019_Q4'	< '2020_Q1'),
    fn = function(x) paste(x, up_arrow)
  ) %>%
  text_transform(
    locations = cells_body(
      columns = "var.vs.target",
      rows = '2019_Q4'	> '2020_Q1'),
    fn = function(x) paste(x, down_arrow)
  )

tab_options(
  heading.background.color = 'rgb(255,098,000)'
) %>%

"**Brand KPI's**"
"summary table"
 

up_arrow <- "<span style=\"color:green\">&#9650;</span>"
down_arrow <- "<span style=\"color:red\">&#9660;</span>"


"Desirability", "Reputation", "Internal NPS"
 tab_spanner(label = "Desirability") %>%
  tab_style(
    style = cell_text(font = "ING me"),
    locations = cells_column_spanners()
  ) %>%
  tab_stubhead() %>%
  tab_style(
    style = cell_text(font = "ING me"),
    locations = cells_title()
  )


  
  
  
  
desi_target <- write_csv(desi_target, "desi_target.csv")



## Rolling average desi 2019
desi_rolling_2019 <- desi_target %>%
  dplyr::filter(quarter_measurement >= 17 & quarter_measurement <= 20) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    desi_12m_avg_2019 = mean(desirability) - mean(desi_MA)
  ) %>%
  dplyr::select(desi_12m_avg_2019,  labels_quarters, b_value, country)

## rolling average starting point Q3 2018, Q4 2018, Q1 2019 & Q2 2019
desi_target_set <- desi_target %>%
  dplyr::filter(quarter_measurement >= 15 & quarter_measurement <= 18) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    desi_target_set = mean(desirability) - mean(desi_MA)
  ) %>%
  dplyr::select(desi_target_set,  labels_quarters, b_value, country)

desi_targett <- left_join(desi_target, desi_rolling_2019, by=c("country", "labels_quarters", "b_value"))
desi_targett <- left_join(desi_targett, desi_target_set, by=c("country", "labels_quarters", "b_value"))

## hard coded target 
desi_targett <- desi_targett %>%
  dplyr::mutate(
    target_desirability = case_when(
      labels_countries == "France" | labels_countries == "Australia" | labels_countries == "Austria" |
      labels_countries == "Belgium" | labels_countries == "Czech Republic" ~ 0,
      labels_countries == "Germany" ~ 13,
      labels_countries == "Italy" ~ 8.2,
      labels_countries == "The Netherlands" ~ 12.7,
      labels_countries == "Poland" ~ 10.7,
      labels_countries == "Romania" ~ 13.6,
      labels_countries == "Spain" ~ 17.1,
      labels_countries == "Turkey" ~ -6.7,
      labels_countries == "The Philippines" ~ -2.9,
      labels_countries == "Luxembourg" ~ -2.6
    ))

## rolling average ytd
desi_targett <- desi_targett %>%
  dplyr::mutate(
    desi_ytd_diff = dplyr::case_when(
      quarter_measurement== max(quarter_measurement) ~ (desirability - desi_MA),
      TRUE ~ NA_real_)
  ) %>%
  dplyr::
  (desirability - desi_MA)




desi_targett <-  desi_targett %>%
  dplyr::mutate(
    pulse = case_when(
      country == 1 ~ pulse,
      country == 2 ~ `RepTrak® Pulse`,
      country == 3 ~ `RepTrak® Pulse`,
      country == 4 ~ pulse,
      country == 5 ~ pulse,
      country == 6 ~ `RepTrak® Pulse`,
      country == 7 ~ pulse,
      country == 8 ~ pulse,
      country == 9 ~ `RepTrak® Pulse`,
      country == 10 ~ pulse,
      country == 11 ~ pulse,
      country == 12 ~ pulse,
      country == 13 ~ pulse,
      country == 14 ~ pulse,
      TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(pulse = case_when(
    country ==14 & quarter_measurement==19 ~ pulse /1.603,
    TRUE ~ pulse
  )) 
  
desi_targett$pulse_target <- 70






desi_target <- test %>%
  dplyr::select(quarter_measurement, labels_quarters, b_value, main_competition, labels_countries, country, desirability,
                'RepTrak® Pulse', pulse) %>%
  dplyr::filter(main_competition==1) %>%
  dplyr::group_by(country, quarter_measurement) %>%
  dplyr::mutate(
    desi_MA = mean(desirability)
  ) %>%
  dplyr::mutate(
    diff_brand_MA = desirability - desi_MA
  ) %>%
  dplyr::filter(b_value==1) %>%
  #dplyr::filter(quarter_measurement >= 17 & quarter_measurement <= 20) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    desi_12m_avg_2019 = case_when(
      quarter_measurement >= 17 & quarter_measurement <= 20 ~
      mean(desirability) - mean(desi_MA) ~
        TRUE ~ 0)
  ) %>%
  dplyr::mutate(
    desi_target_set = case_when(
      quarter_measurement >= 18 & quarter_measurement <= 21 ~
        desirability - desi_MA ~
        TRUE ~ 0
    ))



