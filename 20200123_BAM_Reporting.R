## Before running this script, run the data transformation from wide to long format

## Writing labels

## Brand Funnel per market for every brand
funnel <- data %>%
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
  unique() %>%
  group_by(b_value)

## Struggle to plot this 
p <- funnel %>%
  filter(b_value ==1) %>%
  plot_ly() %>%
  add_trace(
    type = "",
    y = c("aided", "familiarity", "consideration", "preference"),
    x = c(funnel$aided, funnel$fami, funnel$consideration, funnel$preference)) %>%
  layout(yaxis = list(categoryarray = c("aided", "familiarity", "consideration", "preference")))

### Reputation score
rep <- data %>%
  dplyr::select(quarter_measurement, country, b_value, pulse) %>%
  dplyr::filter(quarter_measurement > 19) %>%
  dplyr::mutate(
    pulse = pulse *100
  ) %>%
  distinct()

## Output desirability -> combine this one with perceived expensiveness
desi <- data %>%
  dplyr::select(quarter_measurement, country, b_value, desirability) %>%
  dplyr::filter(quarter_measurement ==19 |20) %>%
  distinct()

love <- data %>%
  dplyr::select(quarter_measurement, labels_countries, love_mean_ING, love_mean_Google) %>%
  dplyr::filter(quarter_measurement >15) %>%
  distinct()
  



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
  


