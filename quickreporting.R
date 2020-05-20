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
                NPS_non_rolling) %>%
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
                detractors, NPS_non_rolling)
  
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


