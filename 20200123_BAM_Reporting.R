## Before running this script, run the data transformation from wide to long format

## Writing labels

## Brand Funnel per market for every brand

# this code works perfectly fine, it just complains about the kind of plot (funnel), but I installed it according 
# the description https://plot.ly/r/funnel-charts/#basic-funnel-plot
funnel_data <- data %>%
  dplyr::select(unaided, toma, aided, fami, opinion, consideration, preference, quarter_measurement, b_value, country) %>%
  dplyr::filter(quarter_measurement ==20 & country==1) %>%
  dplyr::mutate(
    unaided_best = max(unaided, na.rm=T) *100, 
    aided_best = max(aided, na.rm=T) * 100, 
    toma_best = max(toma, na.rm=T) * 100, 
    fami_best = max(fami, na.rm=T) * 100, 
    opinion_best = max(opinion, na.rm=T) * 100, 
    consideration_best = max(consideration, na.rm=T) * 100, 
    preference_best = max(preference, na.rm=T) * 100, 
    unaided = unaided * 100,
    aided = aided * 100,
    toma = toma * 100,
    fami = fami * 100,
    consideration = consideration *100,
    preference = preference *100,
  ) %>%
  dplyr::distinct() 

funnel_data_diff <- funnel_data %>%
  dplyr::select(unaided_best, aided_best, toma_best, fami_best, opinion_best, 
                consideration_best, preference_best, unaided, aided, toma, fami, 
                opinion, consideration, preference, b_value, quarter_measurement, country) %>%
  dplyr::mutate(
    diff_aided = dplyr::case_when(
      b_value==1 ~ (aided_best - aided),
      TRUE ~ 0),
    diff_fami = dplyr::case_when(
      b_value==1 ~ (fami_best - fami),
      TRUE ~ 0),
    diff_cons = dplyr::case_when(
      b_value==1 ~ (consideration_best - consideration),
      TRUE ~ 0),
    diff_pref = dplyr::case_when(
      b_value==1 ~ (preference_best - preference),
      TRUE ~ 0),
    ) %>%
  dplyr::filter(diff_aided!=0, diff_fami!=0, diff_cons!=0, diff_pref!=0)

funnel_data <- funnel_data %>%
  dplyr::filter(b_value==1)
funnel_data %>%
  plot_ly() %>%
  add_trace(type = "funnel",
            y = c("Aided", "Familiarity", "Consideration", "Preference"),
            x = c(round(funnel_data$aided, digits=2), round(funnel_data$fami, digits=2), 
                  round(funnel_data$consideration, digits=2), round(funnel_data$preference, digits=2)),
            textposition = "inside",
            textinfo = "value",
            opacity = 1.0,
            marker = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)"),
                          line = list(width = c(2, 2, 2, 2), color = c("white", "white", "white", "white"))),
            textfont = list(family = "ING me", size = 14, color = "white"),
            connector = list(line = list(color = "white"))) %>%
  layout(yaxis = list(categoryarray = c("Aided", "Familiarity", "Consideration", "Preference")))

### tiny table with diff with best competitor
tab <- plot_ly(
  type = 'table',
  columnwidth = c(1),
  columnorder = c(0),
  header = list(
    values = c("best comp"),
    align = c("center"),
    line = list(width = 1, color = 'black'),
    fill = list(color = "rgb(255,098,000)"),
    font = list(family = "ING me", size = 14, color = "white")
  ),
  cells = list(
    values = cbind(round(funnel_data_diff$diff_aided, digits=2),
                   round(funnel_data_diff$diff_fami, digits=2),
                   round(funnel_data_diff$diff_cons, digits=2),
                   round(funnel_data_diff$diff_pref, digits=2)),
    align = c("center", "center", "center", "center"),
    line = list(color = c("rgb 168, 168, 168"), width = 1),
    font = list(family = "ING me", size = 12, color = c("rgb 105, 105, 105"))
  ))

### desirability
desi <- data %>%
  dplyr::select(quarter_measurement, country, b_value, desirability) %>%
  dplyr::filter(quarter_measurement >17 & b_value==1) %>%
  dplyr::mutate(
    desirability = desirability *100) %>%
  distinct() %>%
  plot_ly(
    type = 'table',
    columnwidth = c(40, 40, 40, 40),
    columnorder = c(0, 1, 2, 3),
    header = list(
      values = c("country","Q3 2019", "Q4 2019", "Q1 2020"),
      align = c("center", "center", "center", "center"),
      line = list(width = 1, color = 'black'),
      fill = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)")),
      font = list(family = "ING me", size = 14, color = "white")
    ),
    cells = list(
      values = rbind(c,round(desi$desirability[desi$quarter_measurement==18], digits=2),
                     round(desi$desirability[desi$quarter_measurement==19], digits=2), 
                     round(desi$desirability[desi$quarter_measurement==20], digits=2)),
      align = c("center", "center", "center", "center"),
      line = list(color = c("rgb 168, 168, 168"), width = 1),
      font = list(family = "ING me", size = 12, color = c("rgb 105, 105, 105"))
    ))

### Reputation score
rep <- data %>%
  dplyr::select(quarter_measurement, country, b_value, pulse) %>%
  dplyr::filter(quarter_measurement > 16 & b_value==1) %>%
  dplyr::mutate(
    pulse = pulse *100
  ) %>%
  distinct()

rep_CAGR <- data %>%
  dplyr::select(quarter_measurement, country, b_value, pulse) %>%
  dplyr::filter(quarter_measurement >17 & b_value==1) %>%
  dplyr::mutate(
    pulse = pulse *100
  ) %>%
  distinct() %>% 
  # Spread across quarters
  tidyr::spread(quarter_measurement, pulse) %>% 
  # Calculate CAGR
  dplyr::mutate(
    cagr = (`20`/`18`)^(1/3) - 1
  ) %>% 
  # Gather across quarters
  tidyr::gather(quarter_measurement, pulse, -country, -b_value, -cagr) %>% 
  # Filter to per country per time
  dplyr::select(country, cagr) %>% 
  dplyr::distinct()

c <- c("Australia", "Austria", "Belgium", "Czech", "France", "Germany", "Italy", "Luxembourg", 
       "Netherlands", "Poland", "Romania", "Spain", "Turkey", "The Philippines")

r <- plot_ly(
  type = 'table',
  columnwidth = c(70, 40, 40, 40),
  columnorder = c(0, 1, 2, 3),
  header = list(
    values = c("country","Q2 2019", "Q3 2019", "Q4 2019"),
    align = c("center", "center", "center", "center"),
    line = list(width = 1, color = 'black'),
    fill = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)")),
    font = list(family = "ING me", size = 14, color = "white")
  ),
  cells = list(
    values = rbind(c, round(rep$pulse[rep$quarter_measurement==18], digits=2),
                   round(rep$pulse[rep$quarter_measurement==19], digits=2), 
                   round(rep$pulse[rep$quarter_measurement==20], digits=2)),
    align = c("center", "center", "center", "center"),
    line = list(color = c("rgb 168, 168, 168"), width = 1),
    font = list(family = "ING me", size = 12, color = c("rgb 105, 105, 105"))
  ))
r

## Output desirability -> combine this one with perceived expensiveness
desi_comb <- data %>%
  dplyr::select(quarter_measurement, country, label, desirability, price_mean) %>%
  dplyr::filter(quarter_measurement ==20 & country==1) %>%
  dplyr::mutate(
    desirability = desirability *100,
    price_mean = price_mean *100
  ) %>%
  distinct()

desi_combi <- desi_comb %>%
  plot_ly(x = ~price_mean, y = ~desirability, type = 'scatter', color ='rgb(255,098,000)',
           mode = 'text+markers', text = ~label, textposition = 'middle right',
           textfont = list(color = 'rgb(204, 204, 204)', size = 11)) %>%
  layout(title = 'Price perception * desirability',
         xaxis = list(title = 'Price perception',
                      range = c(20, 80)
                      ),
         yaxis = list(title = 'Desirability',
                      range = c(0, 50)
                      ))



love <- data %>%
  dplyr::select(quarter_measurement, country, love_mean_ING, love_mean_Google, love_mean_ING_client) %>%
  dplyr::filter(quarter_measurement >17 & country==1) %>%
  distinct()


love$x <-c("Q2 2019", "Q3 2019", "Q4 2019")
l<-love %>%
  plot_ly(x = ~x) %>%
          add_lines(y = ~love_mean_ING_client, type='scatter', name='love ING clients', 
                    line = list(color = 'rgb(255,098,000)', type='scatter', width = 4, mode = 'lines+markers')) %>%
                      add_lines(y =~love_mean_Google, type='scatter', name='love Google',
                                line = list(color = 'rgb(255,098,000)', width = 4, mode = 'lines+markers')) %>%
                                  add_lines(y =~love_mean_ING, type='scatter', name='love ING') %>%
  layout(title = "love",
         xaxis = list(title = "quarters"),
         yaxis = list(title = "", range= c(0,10))
  )
  
  
### Desirability

### Trial to add CAGR to desi
#LastYear = max(data$quarter_measurement)
#PreviousYear = max(data$quarter_measurement) -n
#n = diff(LastYear-PreviousYear)

#CAGR =((LastYear/PreviousYear)^(1/n))-1

# why are the two below not equal to each other?
data$desirability[max(data$quarter_measurement)]
data$desirability[20]
data$desirability[data$quarter_measurement==20]

data$desirability[max$data]
data$desirability[max(data$quarter_measurement)-n]

LastYear = max(data$quarter_measurement)
PreviousYear = max(data$quarter_measurement) -n
n = 3

desi_CAGR <- data %>%
  dplyr::select(quarter_measurement, country, b_value, desirability) %>%
  dplyr::filter(quarter_measurement >17 & b_value==1) %>%
  dplyr::mutate(
    desirability = desirability *100
    ) %>%
  distinct() %>% 
  # Spread across quarters
  tidyr::spread(quarter_measurement, desirability) %>% 
  # Calculate CAGR
  dplyr::mutate(
    cagr = (`20`/`18`)^(1/3) - 1
  ) %>% 
  # Gather across quarters
  tidyr::gather(quarter_measurement, desirability, -country, -b_value, -cagr) %>% 
  # Filter to per country per time
  dplyr::select(country, cagr) %>% 
  dplyr::distinct()

cagr = (max(quarter_measurement)/max(desi$quarter_measurement)-3)^(1/3) - 1

max(quarter_measurement)
max(desi$quarter_measurement)-3

  
### Trial to create a proper table

c <- c("Australia", "Austria", "Belgium", "Czech", "France", "Germany", "Italy", "Luxembourg", 
"Netherlands", "Poland", "Romania", "Spain", "Turkey", "The Philippines")

p <- plot_ly(
  type = 'table',
  columnwidth = c(70, 40, 40, 40),
  columnorder = c(0, 1, 2, 3),
  header = list(
    values = c("country","Q2 2019", "Q3 2019", "Q4 2019"),
    align = c("center", "center", "center", "center"),
    line = list(width = 1, color = 'black'),
    fill = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)")),
    font = list(family = "ING me", size = 14, color = "white")
  ),
  cells = list(
    values = rbind(c("Australia", "Austria", "Belgium", "Czech", "France", "Germany", "Italy", "Luxembourg", 
                      "Netherlands", "Poland", "Romania", "Spain", "Turkey", "The Philippines"),
                   round(desi$desirability[desi$quarter_measurement==18], digits=2),
                   round(desi$desirability[desi$quarter_measurement==19], digits=2), 
                   round(desi$desirability[desi$quarter_measurement==20], digits=2)),
    align = c("center", "center", "center", "center"),
    line = list(color = "black", width = 1),
    font = list(family = "ING me", size = 12, color = c("black"))
  ))
p

c <- c("Australia", "Austria", "Belgium", "Czech", "France", "Germany", "Italy", "Luxembourg", 
       "Netherlands", "Poland", "Romania", "Spain", "Turkey", "The Philippines")

r <- plot_ly(
  type = 'table',
  columnwidth = c(70, 40, 40, 40),
  columnorder = c(0, 1, 2, 3),
  header = list(
    values = c("country","Q2 2019", "Q3 2019", "Q4 2019"),
    align = c("center", "center", "center", "center"),
    line = list(width = 1, color = 'black'),
    fill = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)")),
    font = list(family = "ING me", size = 14, color = "white")
  ),
  cells = list(
    values = rbind(c, round(rep$pulse[rep$quarter_measurement==18], digits=2),
                   round(rep$pulse[rep$quarter_measurement==19], digits=2), 
                   round(rep$pulse[rep$quarter_measurement==20], digits=2)),
    align = c("center", "center", "center", "center"),
    line = list(color = "black", width = 1),
    font = list(family = "ING me", size = 12, color = c("black"))
  ))
r

### clients

clients <- data %>%
  dplyr::select(b_value, labels_countries, labels_quarters, desirability) %>%
  dplyr::filter(b_value %in% c(1)) %>%
  dplyr::group_by(b_value, labels_countries, labels_quarters) %>%
  dplyr::distinct()

### Reputation country level multiple competitors
rep_brand <- data %>%
  dplyr::select(quarter_measurement, country, b_value, pulse) %>%
  dplyr::filter(quarter_measurement > 16 & country==1 & b_value <6) %>%
  dplyr::mutate(
    pulse = pulse *100
  ) %>%
  distinct() %>%
  # Gather across brands
  tidyr::spread(b_value, pulse) 

rep_brand$x <-c("Q1 2019", "Q2 2019", "Q3 2019", "Q4 2019")
rep_b<-rep_brand %>%
  plot_ly(x = ~x) %>%
  add_lines(y = ~rep_brand$'1', type='scatter', name='ING',
            line = list(color = 'rgb(255,098,000)', type='scatter', width = 4, mode = 'lines+markers')) %>%
  add_lines(y =~rep_brand$'2', type='scatter', name='ANZ',
            line = list(color = 'rgb(96,166,218)', width = 4, mode = 'lines+markers')) %>%
  add_lines(y =~rep_brand$'3', type='scatter', name='Commonwealth Bank',
            line = list(color = 'rgb(171,0,102)', width = 4, mode = 'lines+markers')) %>%
  add_lines(y =~rep_brand$'4', type='scatter', name='NAB',
            line = list(color = 'rgb(208,217,60)', width = 4, mode = 'lines+markers')) %>%
  add_lines(y =~rep_brand$'5', type='scatter', name='Westpac',
          line = list(color = 'rgb(52,150,81)', width = 4, mode = 'lines+markers')) %>%
  layout(title = "Pulse",
         xaxis = list(title = "quarters"),
         yaxis = list(title = "", range= c(0,100))
  )

### Awareness graphic country

awa <- data %>%
  dplyr::select(b_value, country, labels_countries, labels_quarters,
                quarter_measurement, unaided, toma, aided, main_competition, label) %>%
  dplyr::filter(quarter_measurement > 17 & country==1) %>%
  distinct() %>%
  dplyr::group_by(quarter_measurement) %>%
  dplyr::mutate(
    market_average_unaided = case_when(
      main_competition == 1 ~ mean(unaided *100, na.rm =T), 
      TRUE ~ NA_real_),
    market_average_aided = case_when(
      main_competition == 1 ~ mean(aided *100, na.rm =T), 
      TRUE ~ NA_real_),
    market_average_toma = case_when(
      main_competition == 1 ~ mean(toma *100, na.rm =T), 
      TRUE ~ NA_real_),
    best_unaided = case_when(
      main_competition == 1 ~ max(unaided *100, na.rm =T), 
      TRUE ~ NA_real_),
    best_aided = case_when(
      main_competition == 1 ~ max(aided *100, na.rm =T), 
      TRUE ~ NA_real_),
    best_toma = case_when(
      main_competition == 1 ~ max(toma *100, na.rm =T), 
      TRUE ~ NA_real_),
    unaided_ING = case_when(
      b_value ==1 ~ unaided *100,
      TRUE ~ NA_real_),
    aided_ING = case_when(
      b_value ==1 ~ aided *100,
      TRUE ~ NA_real_),
    toma_ING = case_when(
      b_value ==1 ~ toma *100,
      TRUE ~ NA_real_),
  ) %>%
    # Gather across quarters
   # tidyr::spread(quarter_measurement, market_average_unaided) %>%
  dplyr::ungroup() %>%
  distinct() %>%
  dplyr::select(b_value, quarter_measurement, labels_quarters, market_average_unaided, best_unaided, unaided_ING) %>%
  gather(variable, value, -(b_value:labels_quarters)) %>%
  dplyr::filter(b_value==1)
  #unite(temp, quarter_measurement, variable) %>%
  #spread(temp, value)
  
awa$value[awa$variable==""]
    
x <-unique(awa$labels_quarters)
y1 <- awa$value[awa$variable=='market_average_unaided']
y2 <- awa$value[awa$variable=='best_unaided']
y3 <- awa$value[awa$variable=='unaided_ING']


datatest <- data.frame(x, y1, y2, y3)

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
              showline = TRUE,
              showgrid = FALSE,
              range =c(0,100),
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              dtick = 10,
              ticks = 'inside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 1,
              ticklen = 1,
              tickfont = list(family = "ING me",
                              size = 12,
                              color = 'rgb(204, 204, 204)'))
datatest$x <- factor(datatest$x, levels = data[["x"]])

p <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = 'market average', marker = list(color = 'rgb(82,81,153)')) %>%
  add_trace(y = ~y2, name = 'best', marker = list(color = "rgb 105, 105, 105")) %>%
  add_trace(y = ~y3, name = 'ING', marker = list(color = 'rgb(255,098,000)')) %>%
  layout(xaxis = xaxis,
         yaxis = yaxis,
         #margin = list(b = 100),
         barmode = 'group', title= "Unaided")

p


              





  
### Plot exploration ################################## COPIED FROM EXPLORATION SCRIPT

desi %>%
  filter(labels_quarters=="Q3_2019") %>%
  plot_ly(x = ~labels_countries, y =~desirability *100, type='bar',
          marker = list(color = 'rgb(255,098,000)', width = 1.5)) %>%
  layout(title = "Desirability",
         xaxis = list(title = "countries"),
         yaxis = list(title = "desirability", range= c(0,100),
                      annotations = annotations)
  )

value <- data %>%
  dplyr::select(b_value, labels_countries, labels_quarters, image7) %>%
  dplyr::filter(b_value %in% c(1)) %>%
  dplyr::filter(labels_quarters %in% c("Q1_2019","Q2_2019", "Q3_2019")) %>%
  dplyr::distinct()

write.table(value, "/Users/xo21bm/Documents/Lokaal/BAM2/exploration/value.txt", sep=",")

## Experienced Empowerment 
emp <-data %>%
  dplyr::select(quarter_measurement, b_value, country, empower1, empower2, empower3, empower4) %>%
  dplyr::filter(quarter_measurement >12) %>%
  distinct() 

# write.table(emp, "/Users/xo21bm/Documents/Lokaal/BAM2/exploration/empowerment.csv", sep=",")

## Input NL & BE reputation
repNL <- data %>%
  dplyr::select(consideration, preference, trust4, country, quarter_measurement, b_value) %>%
  dplyr::filter(quarter_measurement >16, country ==3 | country ==9) %>%
  dplyr::group_by(quarter_measurement, country, b_value) %>%
  distinct()

Awareness <- data %>%
  dplyr::select(b_value, country, quarter_measurement, toma, aided, unaided) %>%
  dplyr::filter(b_value==1, country==6) %>%
  dplyr::group_by(quarter_measurement) %>%
  dplyr::distinct() 

"C:/Lokaal/BAM2/exploration/Awareness.csv"

Reputation_Romania <- data %>%
  dplyr::select(rep_trak1, rep_trak2, rep_trak3, rep_trak4, pulse, country, quarter_measurement, b_value) %>%
  dplyr::filter(quarter_measurement >12, country ==11, b_value <6) %>%
  distinct()
write.table(Reputation_Romania, "/Users/xo21bm/Documents/Lokaal/BAM2/exploration/Reputation_Romania.csv", sep=",")


