# Quick reporting

Rom_client <- test %>%
  dplyr::select(labels_quarters, quarter_measurement, b_value, label, labels_countries, country, NPS_non_rolling, client, main_bank, promotors, neutrals, detractors) %>%
  dplyr::filter(quarter_measurement >16 & country==11 & b_value ==1) %>%
  # dplyr::group_by(labels_quarters) %>%
  dplyr::select(label, labels_quarters, NPS_non_rolling, client, main_bank, promotors, neutrals, detractors) %>%
  dplyr::select(label, labels_quarters, client, main_bank, NPS_non_rolling, promotors, neutrals, detractors) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    client = round(client *100, digits=2),
    main_bank = round(main_bank *100, digits=2),
    promotors = round(Rom_client$promotors, digits=2),
    neutrals = round(Rom_client$neutrals, digits=2), 
    detractors = round(Rom_client$detractors, digits=2))
    #labels_quarters = as.yearqtr(unlist(Rom_client$labels_quarters), format='%Y Q%q'))
      
 


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
                              color = 'rgb(105, 105, 105)'))
yaxis <- list(title = "",
              titlefont = list(family = "ING me",
                               size = 12,
                               color = 'rgb(105, 105, 105)'),
              showline = TRUE,
              showgrid = FALSE,
              range =c(0,100),
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = TRUE,
              #dtick = 50.000,
              ticks = 'inside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 1,
              #ticklen = 1,
              tickfont = list(family = "ING me",
                              size = 12,
                              color = 'rgb(105, 105, 105)'))

p1 <- plot_ly(Rom_client, x = Rom_client$labels_quarters, y = ~client, 
              type = 'bar', name = 'client', marker = list(color = 'rgb(82,81,153)')) %>%
  add_lines(x = ~labels_quarters, y = ~main_bank, 
            name = 'main bank', marker = list(color = 'rgb(255,098,000)')) %>%
  layout(xaxis = xaxis,
         yaxis = yaxis,
         barmode = 'group', title= "",
         legend =list(font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))

p1

p <- plot_ly(
  type = 'table',
  columnwidth = c(40, 40, 40, 40, 40, 40, 40),
  columnorder = c(0, 1, 2, 3, 4, 5, 6),
  header = list(
    values = c("<b>Quarter<b>","<b>Client<b>", "<b>Main bank<b>", "<b>NPS non rolling<b>", "<b>promotors<b>","<b>neutrals<b>", "<b>detractors<b>"),
    align = c("left", "right", "right", "right", "right", "right", "right"),
    line = list(width = 1, color = c("rgb 168, 168, 168")),
    fill = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)")),
    font = list(family = "ING me", size = 14, color = "white")
  ),
  cells = list(
    values = rbind(Rom_client$labels_quarters, Rom_client$client, Rom_client$main_bank, 
                   Rom_client$NPS_non_rolling, Rom_client$promotors, Rom_client$neutrals, 
                   Rom_client$detractors),
    align = c("left", "right", "right", "right", "right", "right", "right"),
    line = list(color = c("rgb 168, 168, 168"), width = 1),
    font = list(family = "ING me", size = 12, color = c("rgb 105, 105, 105"))
  ))
p

Rom_clientb <- test %>%
  dplyr::select(labels_quarters, quarter_measurement, b_value, label, labels_countries, country, NPS_non_rolling, client, main_bank, promotors, neutrals, detractors) %>%
  dplyr::filter(quarter_measurement >16 & country==11 & b_value<6) %>%
  # dplyr::group_by(labels_quarters) %>%
  dplyr::select(label, labels_quarters, NPS_non_rolling, client, main_bank, promotors, neutrals, detractors) %>%
  dplyr::select(label, labels_quarters, client, main_bank, NPS_non_rolling, promotors, neutrals, detractors) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    client = round(client *100, digits=2),
    main_bank = round(main_bank *100, digits=2)) %>%
  dplyr::select(label, labels_quarters, NPS_non_rolling) %>%
  tidyr::spread(labels_quarters, NPS_non_rolling)

p1 <- plot_ly(x = Rom_clientb$labels_quarters[Rom_clientb$label=="ING"]) %>%
  add_lines(y = ~Rom_clientb$NPS_non_rolling[Rom_clientb$label=="ING"], name = 'ING', type='scatter', 
            line = list(color = 'rgb(255,098,000)', type='scatter', width = 4, mode = 'lines+markers')) %>%
  add_lines(y = ~Rom_clientb$NPS_non_rolling[Rom_clientb$label=="BCR"], name = 'BCR', type='scatter', 
            line = list(color = 'rgb(96,166,218)', type='scatter', width = 4, mode = 'lines+markers')) %>%
  add_lines(y = ~Rom_clientb$NPS_non_rolling[Rom_clientb$label=="BRD"], name = 'BRD', type='scatter', 
            line = list(color = 'rgb(171,0,102)', type='scatter', width = 4, mode = 'lines+markers')) %>%
  add_lines(y = ~Rom_clientb$NPS_non_rolling[Rom_clientb$label=="Raiffeisen"], name = 'Raiffeisen', type='scatter', 
            line = list(color = 'rgb(208,217,60)', type='scatter', width = 4, mode = 'lines+markers')) %>%
  add_lines(y = ~Rom_clientb$NPS_non_rolling[Rom_clientb$label=="Banca Transilvania"], name = 'Banca Transilvania', type='scatter', 
            line = list(color = 'rgb(52,150,81)', type='scatter', width = 4, mode = 'lines+markers')) %>%
  layout(xaxis = xaxis,
         yaxis = yaxis,
         barmode = 'group', title= "",
         legend =list(font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))

p1

### Generating figures for countries
Ready_to_Interpret <- test %>%
  dplyr::select(labels_countries, labels_quarters, quarter_measurement, label, toma, unaided, aided, fami, consideration,
                preference, desirability, easy_mean, meets_needs_mean, smart_mean, trust4, client, main_bank, pulse,
                love_mean_ING, love_mean_ING_client, love_mean_Google, price_mean, promotors, detractors, neutrals, 
                NPS_non_rolling, rep_trak1, rep_trak2, rep_trak3, rep_trak4) %>%
  dplyr::filter(quarter_measurement >18) %>%
  dplyr::mutate(
    top_of_mind = round(toma *100, digits=2),
    aided = round(aided *100, digits=2),
    unaided = round(unaided *100, digits=2),
    familiarity = round(fami *100, digits=2),
    consideration = round(consideration *100, digits=2),
    preference = round(preference *100, digits=2),
    desirability = round(desirability, digits=2),
    easy = round(easy_mean, digits=2),
    meets_needs = round(meets_needs_mean, digits=2),
    smart = round(smart_mean, digits=2),
    trust =round(trust4 *100, digits=2),
    client =round(client *100, digits=2),
    main_bank =round(main_bank *100, digits=2),
    pulse_BAM = round(pulse, digits=2),
    love_mean_ING = round(love_mean_ING, digits=2),
    love_mean_ING_client = round(love_mean_ING_client, digits=2),
    love_mean_Google = round(love_mean_Google, digits=2),
    price_perception = round(price_mean, digits=2),
    promotors = round(promotors, digits=2),
    neutrals = round(neutrals, digits=2),
    detractors = round(detractors, digits=2),
    NPS_non_rolling =round(NPS_non_rolling, digits=2)
  )

BAM_main_metrics <- Ready_to_Interpret %>%
  dplyr::select(labels_countries, labels_quarters, label, top_of_mind, unaided, aided, familiarity, consideration, 
                preference, desirability, easy, meets_needs, smart, trust, client, main_bank, pulse_BAM, 
                love_mean_ING, love_mean_ING_client, love_mean_Google, price_perception, promotors, neutrals, 
                detractors, NPS_non_rolling, rep_trak1, rep_trak2, rep_trak3, rep_trak4)
  
BAM_main_metrics <-write.csv(BAM_main_metrics, "BAM_main_metrics.csv")


## Reptrak for clients & non clients
### Reptrak statements
tempREP11 <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, reptrak1_value, client_value) %>%
  dplyr::filter(!is.na(reptrak1_value) & client_value==1) %>%
  dplyr::filter(reptrak1_value %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
  dplyr::mutate(
    reptrak1_client = reptrak1_value * weight
  ) %>%
  dplyr::group_by(b_value, country, quarter_measurement, client_value) %>%
  dplyr::summarize(
    reptrak1_client =(mean(reptrak1_client)/7 *100)/ mean(weight)
  )

data1 <- full_join(data1, tempREP11, by=c("b_value", "country", "quarter_measurement"))
rm(tempREP11)

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
rm(tempREP4)

data1<- data1 %>%
  dplyr::mutate(
    pulse_clients = (rep_trak1_client + rep_trak2_client + rep_trak3_client + rep_trak4_client) /4
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
rm(tempREP9)

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
rm(tempREP11)


## split for app users
App_usage <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, nps_value, client_value, app_usage_value, main_bank_value) %>%
  #dplyr::filter(country ==9) %>%
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
    percentage = n /sum(n) *100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, quarter_measurement, b_value, app_usage, percentage) %>%
  dplyr::distinct() %>%
  #tibble::rowid_to_column() %>%
  tidyr::spread(app_usage, percentage) 

# to add Q figures to data set, split for clients
data1 <- full_join(data1, App_usage, by=c("b_value", "country", "quarter_measurement"))

# version to work with
App_usage <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, nps_value, client_value, app_usage_value, main_bank_value) %>%
  #dplyr::filter(country ==9) %>%
  dplyr::filter(!is.na(nps_value) & client_value==1 & !is.na(app_usage_value)) %>%
  dplyr::mutate(app_usage = case_when(
    app_usage_value==1 ~ "Yes, at least five times",
    app_usage_value==2 ~ "Yes, but not very often, fewer than five times",
    app_usage_value==3 ~ "No, but I do have the app on my smartphone",
    app_usage_value==4 ~ "No, and I don’t have the app on my smartphone either",
    app_usage_value==5 ~ "No, I don’t have a smartphone",
    TRUE ~ "NA_real_"
  )) %>%
  dplyr::group_by(country, quarter_measurement, b_value, app_usage, main_bank_value) %>%
  dplyr::tally(wt=weight) %>%
  dplyr::mutate(
    percentage = n /sum(n) *100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, quarter_measurement, b_value, app_usage, percentage, main_bank_value) %>%
  dplyr::distinct() %>%
  #tibble::rowid_to_column() %>%
  tidyr::spread(app_usage, percentage) 

## NL
overview_app_usage <- data1 %>%
  dplyr::select("Yes, but not very often, fewer than five times", "Yes, at least five times", "No, I don’t have a smartphone",
                "No, but I do have the app on my smartphone", "No, and I don’t have the app on my smartphone either",
                labels_countries, labels_quarters, label, quarter_measurement, b_value, country, NPS_non_rolling,
                main_bank) %>%
  #dplyr::filter(country==9) %>%
  dplyr::filter(b_value <5)

## overview plaatje
overview_app_usage <-overview_app_usage %>%
  dplyr::filter(country==9)

p1_NL <- plot_ly(overview_app_usage, x = ~label, y = ~overview_app_usage$`Yes, at least five times`, type = 'bar', name = 'Yes, at least five times', 
              marker = list(color = 'rgb(255,098,000)')) %>%
  add_trace(y = ~overview_app_usage$`Yes, but not very often, fewer than five times`, name = "Yes, but not very often, fewer than five times", 
            marker = list(color = 'rgb(52,150,81)')) %>%
  add_trace(y = ~overview_app_usage$`No, but I do have the app on my smartphone`, name = "No, but I do have the app on my smartphone", 
            marker = list(color = 'rgb(96,166,218)')) %>%
  add_trace(y = ~overview_app_usage$`No, and I don’t have the app on my smartphone either`, name = "No, and I don’t have the app on my smartphone either", 
            marker = list(color = 'rgb(82,81,153)')) %>%
  add_trace(y = ~overview_app_usage$`No, I don’t have a smartphone`, name = "No, I don’t have a smartphone", 
            marker = list(color = "rgb 168, 168, 168")) %>%
  layout(xaxis = xaxis,
         yaxis = yaxis,
         #margin = list(b = 100),
         barmode = 'stack', title= "",
         legend =list(font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
p1_NL

## BE
overview_app_usage <- data1 %>%
  dplyr::select("Yes, but not very often, fewer than five times", "Yes, at least five times", "No, I don’t have a smartphone",
                "No, but I do have the app on my smartphone", "No, and I don’t have the app on my smartphone either",
                labels_countries, labels_quarters, label, quarter_measurement, b_value, country, NPS_non_rolling,
                main_bank) %>%
  #dplyr::filter(country==9) %>%
  dplyr::filter(b_value <5)


overview_app_usage <-overview_app_usage %>%
  dplyr::filter(country==3)
p1_BE <- plot_ly(overview_app_usage, x = ~label, y = ~overview_app_usage$`Yes, at least five times`, type = 'bar', name = 'Yes, at least five times', 
              marker = list(color = 'rgb(255,098,000)')) %>%
  add_trace(y = ~overview_app_usage$`Yes, but not very often, fewer than five times`, name = "Yes, but not very often, fewer than five times", 
            marker = list(color = 'rgb(52,150,81)')) %>%
  add_trace(y = ~overview_app_usage$`No, but I do have the app on my smartphone`, name = "No, but I do have the app on my smartphone", 
            marker = list(color = 'rgb(96,166,218)')) %>%
  add_trace(y = ~overview_app_usage$`No, and I don’t have the app on my smartphone either`, name = "No, and I don’t have the app on my smartphone either", 
            marker = list(color = 'rgb(82,81,153)')) %>%
  add_trace(y = ~overview_app_usage$`No, I don’t have a smartphone`, name = "No, I don’t have a smartphone", 
            marker = list(color = "rgb 168, 168, 168")) %>%
  layout(xaxis = xaxis,
         yaxis = yaxis,
         #margin = list(b = 100),
         barmode = 'stack', title= "",
         legend =list(font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
p1_BE



### NPS
NPS_trial_ORI <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, nps_value, client_value, app_usage_value, main_bank_value) %>%
  dplyr::filter(!is.na(nps_value) & client_value==1) %>% 
  dplyr::filter(b_value<5) %>%
  #dplyr::filter(app_usage_value==1|app_usage_value==2) %>%
  dplyr::filter(country ==9|country==3) %>%
  dplyr::mutate(nps_cat = case_when(
    nps_value >= 9 ~ "promotors",
    nps_value < 9 & nps_value > 6 ~ "neutrals",
    nps_value  <= 6 ~ "detractors",
    TRUE ~ "NA_real_")) %>%
  #dplyr::mutate(app_users = case_when(
  #  app_usage_value <3 ~ 1,
  #  TRUE ~ 0
 # )) %>%
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

NPS_trial_APP <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, nps_value, client_value, app_usage_value, main_bank_value) %>%
  dplyr::filter(!is.na(nps_value) & client_value==1) %>% 
  dplyr::filter(b_value<5) %>%
  dplyr::filter(app_usage_value==1|app_usage_value==2) %>%
  dplyr::filter(country ==9|country==3) %>%
  dplyr::mutate(nps_cat = case_when(
    nps_value >= 9 ~ "promotors_APP",
    nps_value < 9 & nps_value > 6 ~ "neutrals_APP",
    nps_value  <= 6 ~ "detractors_APP",
    TRUE ~ "NA_real_")) %>%
  #dplyr::mutate(app_users = case_when(
  #  app_usage_value <3 ~ 1,
  #  TRUE ~ 0
  # )) %>%
  dplyr::group_by(country, quarter_measurement, b_value, nps_cat) %>%
  dplyr::tally(wt=weight) %>%
  dplyr::mutate(
    percentage = n /sum(n) *100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, quarter_measurement, b_value, nps_cat, percentage) %>%
  tidyr::spread(nps_cat, percentage) %>%
  dplyr::mutate(
    NPS_non_rolling_APP =round(promotors_APP, digits =2)- round(detractors_APP, digits =2)
  ) 


NPS_trial_MAPP <- result %>%
  dplyr::select(weight, country, quarter_measurement, b_value, nps_value, client_value, app_usage_value, main_bank_value) %>%
  dplyr::filter(!is.na(nps_value) & main_bank_value==1) %>% 
  dplyr::filter(b_value<5) %>%
  dplyr::filter(app_usage_value==1|app_usage_value==2) %>%
  dplyr::filter(country ==9|country==3) %>%
  dplyr::mutate(nps_cat = case_when(
    nps_value >= 9 ~ "promotors_MAPP",
    nps_value < 9 & nps_value > 6 ~ "neutrals_MAPP",
    nps_value  <= 6 ~ "detractors_MAPP",
    TRUE ~ "NA_real_")) %>%
  #dplyr::mutate(app_users = case_when(
  #  app_usage_value <3 ~ 1,
  #  TRUE ~ 0
  # )) %>%
  dplyr::group_by(country, quarter_measurement, b_value, nps_cat) %>%
  dplyr::tally(wt=weight) %>%
  dplyr::mutate(
    percentage = n /sum(n) *100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, quarter_measurement, b_value, nps_cat, percentage) %>%
  tidyr::spread(nps_cat, percentage) %>%
  dplyr::mutate(
    NPS_non_rolling_MAPP =round(promotors_MAPP, digits =2)- round(detractors_MAPP, digits =2)
  ) 

NPS_NL_BE <- dplyr::left_join(NPS_trial_ORI, NPS_trial_APP, b=c("country", "quarter_measurement", "b_value"))

NPS_NL_BE <- dplyr::left_join(NPS_NL_BE, NPS_trial_MAPP, b=c("country", "quarter_measurement", "b_value"))

## plot bandwith pulse

rep <- test %>%
  dplyr::select(quarter_measurement, country, b_value, pulse, label, labels_countries) %>%
  dplyr::filter(quarter_measurement== max(quarter_measurement)) %>%
  dplyr::filter(!is.na(pulse)) %>%
  dplyr::filter(country!=3) %>%
  dplyr::filter(country!=6) %>%
  dplyr::filter(country!=9) %>%
  dplyr::mutate(pulse = case_when(
    country ==14 & quarter_measurement==19 ~ pulse /1.603,
    TRUE ~ pulse
  )) %>%
  dplyr::group_by(labels_countries) %>%
  dplyr::mutate(
    min_pulse = min(pulse),
    max_pulse = max(pulse),
    pulse_ING = case_when(
      b_value ==1 ~ pulse,
      TRUE ~ NA_real_
    )
  )

xaxis <- list(title = "countries", 
              titlefont = list(color = 'rgb(105, 105, 105)', size = 12, family = "ING me"),
              #range =c(20,80),
              #dtick = 10,
              showline = TRUE,
              showgrid = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              tickfont = list(family = "ING me",
                              size = 12,
                              color = 'rgb(105, 105, 105)'))
yaxis <- list(title = "pulse",
              titlefont = list(color = 'rgb(105, 105, 105)', size = 12, family = "ING me"),
              range =c(30,100),
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = TRUE,
              #dtick = 5,
              ticks = 'inside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 1,
              #ticklen = 1,
              tickfont = list(family = "ING me",
                              size = 12,
                              color = 'rgb(105, 105, 105)'))

p <- rep %>%
  plot_ly(x = ~labels_countries, y= rep$pulse, type = 'scatter', color ='rgb(255,098,000)',
          mode = 'text+markers', text = ~label, textposition = 'middle right',
          textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 11)) %>%
  layout(title = '',
         xaxis = xaxis,
         yaxis = yaxis)

p

p <- rep %>%
  plot_ly(x = ~labels_countries, y= rep$min_pulse, type = 'scatter', mode= 'markers',
          name = 'min', marker = list(color = 'rgb(52,150,81)', type='scatter', width = 4, mode = 'markers')) %>%
  add_trace(y = ~rep$max_pulse, type = 'scatter', mode= 'markers',
            name = 'max', marker = list(color = 'rgb(96,166,218)', type='scatter', width = 4, mode = 'markers')) %>%
  add_trace(y= ~rep$pulse_ING, type = 'scatter', mode= 'markers',
            name = 'ING', marker = list(color = 'rgb(255,098,000)', type='scatter', width = 4, mode = 'markers')) %>%
  layout(title = '',
         xaxis = xaxis,
         yaxis = yaxis)

p


