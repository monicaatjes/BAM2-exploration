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

## Output Reptrak
reptrak<- data %>%
  dplyr::select(quarter_measurement, b_value, country, rep_trak1, rep_trak2, rep_trak3, rep_trak4) %>%
  #dplyr::filter(quarter_measurement ==20 & country==1) %>%
  # Gather
  tidyr::gather("reptrak_type", "reptrak_value", rep_trak1:rep_trak4, na.rm = T) %>%
  dplyr::group_by(b_value, country, quarter_measurement) %>%
  dplyr::summarise(
    pulse = mean(reptrak_value)) %>%
  dplyr::ungroup()

## Output desirability



