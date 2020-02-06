## Before running this script, run the data transformation from wide to long format

## Writing labels

## Brand Funnel per market for every brand

# this code works perfectly fine, it just complains about the kind of plot (funnel), but I installed it according 
# the description https://plot.ly/r/funnel-charts/#basic-funnel-plot
funnel_data <- data %>%
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
  dplyr::distinct() %>% 
  dplyr::filter(b_value==1)

funnel_data %>%
  plot_ly() %>%
  add_trace(type = "funnel",
            y = c("aided", "fami", "consideration", "preference"),
            x = c(round(funnel_data$aided, digits=2), round(funnel_data$fami, digits=2), 
                  round(funnel_data$consideration, digits=2), round(funnel_data$preference, digits=2)),
            textposition = "inside",
            textinfo = "value+percent initial",
            opacity = 0.65,
            marker = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)"),
                          line = list(width = c(4, 2, 2, 3), color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)"))),
            connector = list(line = list(color = "royalblue", dash = "dot", width = 3))) %>%
  layout(yaxis = list(categoryarray = c("aided", "fami", "consideration", "preference")))

## desirability
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
      line = list(color = "black", width = 1),
      font = list(family = "ING me", size = 12, color = c("black"))
    ))

### Reputation score
rep <- data %>%
  dplyr::select(quarter_measurement, country, b_value, pulse) %>%
  dplyr::filter(quarter_measurement > 16 & b_value==1) %>%
  dplyr::mutate(
    pulse = pulse *100
  ) %>%
  distinct()

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

## Output desirability -> combine this one with perceived expensiveness
desi_comb <- data %>%
  dplyr::select(quarter_measurement, country, b_value, desirability, price_mean) %>%
  dplyr::filter(quarter_measurement >18 & b_value==1) %>%
  distinct()

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

desi <- data %>%
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
reptrak_Aus$x <-c("Q1 2019", "Q2 2019", "Q3 2019", "Q4 2019")
reptrak_Aus <- reptrak_total %>%
  dplyr::select(country, quarter_measurement, pulse, b_value) %>%
  dplyr::filter(country==1 & b_value %in% c(1,2,3,4))

ING <- list(
  xref = '',
  yref = '',
  x = 0.05,
  y = reptrak_Aus$pulse[reptrak_Aus$b_value==1],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('ING', pulse, '%'),
  font = list(family = 'ING me',
              size = 16,
              color = 'rgb(255,098,000)'),
  showarrow = FALSE)

ANZ <- list(
  xref = '',
  yref = '',
  x = 0.05,
  y = reptrak_Aus$pulse[reptrak_Aus$b_value==2],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('ING', pulse, '%'),
  font = list(family = 'ING me',
              size = 16,
              color = 'rgb(255,098,000)'),
  showarrow = FALSE)

Commonwealth <- list(
  xref = '',
  yref = '',
  x = 0.05,
  y = reptrak_Aus$pulse[reptrak_Aus$b_value==3],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('ING', pulse, '%'),
  font = list(family = 'ING me',
              size = 16,
              color = 'rgb(255,098,000)'),
  showarrow = FALSE)

r <-plot_ly(reptrak_Aus, x = ~quarter_measurement, y=~ING, typ='scatter', name ='ING') %>%
  add_lines(y = ~ING, type='scatter', name='ING', 
            line = list(color = 'rgb(255,098,000)', type='scatter', width = 4, mode = 'lines')) %>%
  add_lines(y =~ANZ, type='scatter', name='ANZ',
            line = list(color = 'rgb(255,098,000)', width = 4, mode = 'lines')) %>%
  add_lines(y =~Commonwealth, type='scatter', name='Commonwealth',
            line = list(color = 'rgb(255,098,000)', width = 4, mode = 'lines')) %>%
  layout(title = "pulse",
         xaxis = list(title = "quarters"),
         yaxis = list(title = "", range= c(0,100)))






  
  



