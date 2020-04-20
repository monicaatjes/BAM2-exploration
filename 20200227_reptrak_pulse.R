## Add Reptrak pulse scores
# open data latest quarter (Q1 2020)
reptrakq1 <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/20200416 Reputation overview.xlsx", range = "B1:O15")
# Transpose data except quarter column
reptrakq1_ <- as.data.frame(t(reptrakq1[,-1]))

# first row as column names
colnames(reptrakq1_) = as.character(unlist(reptrakq1_[1,]))
reptrakq1_ <- reptrakq1_[-1,]
reptrakq1_$country <-rownames(reptrakq1_)
reptrakq1_<- as_tibble(reptrakq1_)
# add quarter column again
reptrakq1_$quarter <-reptrakq1[c(1:12),1]
reptrakq1_$quarter <- as.yearqtr(unlist(reptrakq1_$quarter), format='Q%q %Y')
rm(reptrakq1)




# open data latest quarter
reptrak <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/201200128 2019 Reputation overview.xlsx", range = "A1:N15")
# Transpose data except quarter column
reptrak_ <- as.data.frame(t(reptrak[,-1]))

# first row as column names
colnames(reptrak_) = as.character(unlist(reptrak_[1,]))
reptrak_ <- reptrak_[-1,]
reptrak_$country <-rownames(reptrak_)
reptrak_<- as_tibble(reptrak_)
# add quarter column again
reptrak_$quarter <-reptrak[c(1:12),1]
reptrak_$quarter <- as.yearqtr(unlist(reptrak_$quarter), format='Q%q %Y')
rm(reptrak)

# open data quarter 3
reptrakQ3_ <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/201200128 2019 Reputation overview.xlsx", 
                       range = "A17:N30", col_names = F)
reptrakQ3 <- as.data.frame(t(reptrakQ3_[,-1]))

# first row as column names
colnames(reptrakQ3) = as.character(unlist(reptrakQ3[1,]))
reptrakQ3 <- reptrakQ3[-1,]
reptrakQ3$country <-reptrak_$country
reptrakQ3<- as_tibble(reptrakQ3)
# add quarter column again
reptrakQ3$quarter <-reptrakQ3_[c(1:12),1]
reptrakQ3$quarter <- as.yearqtr(unlist(reptrakQ3$quarter), format='Q%q %Y')
rownames(reptrakQ3) <- NULL
rm(reptrakQ3_)

# open data quarter 2
reptrakQ2_ <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/201200128 2019 Reputation overview.xlsx", 
                        range = "A32:N45", col_names = F)
reptrakQ2 <- as.data.frame(t(reptrakQ2_[,-1]))

# first row as column names
colnames(reptrakQ2) = as.character(unlist(reptrakQ2[1,]))
reptrakQ2 <- reptrakQ2[-1,]
reptrakQ2$country <-reptrak_$country
reptrakQ2<- as_tibble(reptrakQ2)
# add quarter column again
reptrakQ2$quarter <-reptrakQ2_[c(1:12),1]
reptrakQ2$quarter <- as.yearqtr(unlist(reptrakQ2$quarter), format='Q%q %Y')
rownames(reptrakQ2) <- NULL
rm(reptrakQ2_)

# open data quarter 1
reptrakQ1_ <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/201200128 2019 Reputation overview.xlsx", 
                        range = "A47:N60", col_names = F)
reptrakQ1 <- as.data.frame(t(reptrakQ1_[,-1]))

# first row as column names
colnames(reptrakQ1) = as.character(unlist(reptrakQ1[1,]))
reptrakQ1 <- reptrakQ1[-1,]
reptrakQ1$country <-reptrak_$country
reptrakQ1<- as_tibble(reptrakQ1)
# add quarter column again
reptrakQ1$quarter <-reptrakQ1_[c(1:12),1]
reptrakQ1$quarter <- as.yearqtr(unlist(reptrakQ1$quarter), format='Q%q %Y')
rownames(reptrakQ1) <- NULL
rm(reptrakQ1_)

# combine the different quarters in one file
rept1 <- rbind(reptrakQ1, reptrak_)
rept2 <- rbind(reptrakQ3, reptrakQ2)
rept3 <- rbind(rept1, rept2)

## Q UPDATE HERE ######################################################
rept4 <-rbind(rept3, reptrakq1_)
rept3 <- rept4


rept3$labels_quarters <- rept3$quarter
rept3$labels_countries <- rept3$country
rept3$quarter <- NULL
rept3$country <- NULL
rept3$b_value <- 1

# change the factors to something normal
rept3$`RepTrak® Pulse` <- as.numeric(as.character(rept3$`RepTrak® Pulse`))
rept3$`Products & Services` <- as.numeric(as.character(rept3$`Products & Services`))
rept3$Innovation <- as.numeric(as.character(rept3$Innovation)) 
rept3$Workplace <- as.numeric(as.character(rept3$Workplace))
rept3$Governance <- as.numeric(as.character(rept3$Governance))
rept3$Citizenship <- as.numeric(as.character(rept3$Citizenship))
rept3$Leadership <- as.numeric(as.character(rept3$Leadership))
rept3$Performance <- as.numeric(as.character(rept3$Performance))
rept3$Purchase <- as.numeric(as.character(rept3$Purchase))
rept3$Recommend <- as.numeric(as.character(rept3$Recommend))
rept3$`Crisis proof` <- as.numeric(as.character(rept3$`Crisis proof`))
rept3$`Verbal support` <- as.numeric(as.character(rept3$`Verbal support`))
rept3$Invest <- as.numeric(as.character(rept3$Invest))
rept3$`Work for` <- as.numeric(as.character(rept3$`Work for`))

# clean
rm(rept1, rept2, reptrakQ1, reptrakQ2, reptrakQ3, reptrak_)

# Combine in data 
rept3$labels_quarters <-as.yearqtr(unlist(rept3$labels_quarters), format='%Y Q%q')

############# Q update here
test$labels_quarters <-as.yearqtr(unlist(test$labels_quarters), format= '%Y Q%q')
test <- dplyr::left_join(test, rept3, by=c("labels_countries", "labels_quarters", "b_value"))



### INTERNAL NPS 
internal <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/20100124 Internal metrics .xlsx", range = "A26:I47")

# throw away empty column
internal <- internal[,-2]

# without transpose
internal$labels_countries <- internal$`Row Labels`
internal$`Row Labels` <- NULL
colnames(internal) <- as.yearqtr(colnames(internal[1:7]), format= 'Q%q_%Y') 
names(internal)[8] <- "labels_countries" 

internal <- internal %>%
  dplyr::select(labels_countries, `2019 Q1`, `2019 Q2`, 
                `2019 Q3`, `2019 Q4`) %>%
  dplyr::mutate(labels_countries = case_when(
    labels_countries =="Spain & Portugal" ~ "Spain",
    labels_countries =="Germany Region" ~ "Germany",
    TRUE ~ labels_countries)
  )

# and for Q1 2020 NPS 
p79004_AlignmentMonitor_2020Q1 <- read_csv("p79004_AlignmentMonitor_2020Q1_v2.csv")

int <-p79004_AlignmentMonitor_2020Q1
rm(p79004_AlignmentMonitor_2020Q1)
str(int)
as_tibble(int)

int <- int %>%
  dplyr::mutate(
    labels_countries = case_when(
      country == 1 ~ "Netherlands",
      country == 2 ~ "Belgium",
      country == 3 ~ "Luxembourg",
      country == 4 ~ "France",
      country == 5 ~ "Spain",
      country == 6 ~ "Italy",
      country == 7 ~ "Poland",
      country == 8 ~ "Romania",
      country == 9 ~ "Turkey",
      country == 10 ~ "Germany",
      country == 11 ~ "Austria",
      country == 13 ~ "Australia",
      country == 14 ~ "Singapore",
      country == 15 ~ "United Kingdom",
      country == 16 ~ "United States",
      country == 17 ~ "Czech Rebulic",
      country == 18 ~ "Russia",
      country == 19 ~ "Philippines",
      country == 20 ~ "Slovakia",
      country == 21 ~ "other",
      TRUE ~ "NA_real_")
  )

nps_overview <- int %>%
  dplyr::select(labels_countries, weight, q1001) %>%
  dplyr::group_by(labels_countries) %>%
  dplyr::mutate(
    nps_value = q1001
  ) %>%
  dplyr::mutate(nps_cat = case_when(
    nps_value >=9.0 ~ "promotors",
    nps_value <= 9.0 & nps_value >= 7.0 ~ "neutrals",
    nps_value  <= 6.0 ~ "detractors",
    TRUE ~ "NA_real_")) %>%
  dplyr::filter(nps_cat %in% c("detractors", "neutrals", "promotors")) %>%
  dplyr::group_by(labels_countries, nps_cat) %>%
  dplyr::tally() %>%
  dplyr::mutate(
    percentage = n /sum(n)
  ) %>% 
  dplyr::ungroup()

NPS_score <- nps_overview %>%
  dplyr::select(labels_countries, percentage, nps_cat) %>%
  tidyr::spread(nps_cat, percentage) %>%
  dplyr::mutate(
    nps_value = promotors - detractors
  ) %>%
  dplyr::filter(!labels_countries=="Austria") %>%
  dplyr::filter(!labels_countries=="Czech Rebulic") %>%
  dplyr::filter(!labels_countries=="Singapore") %>%
  dplyr::filter(!labels_countries=="Turkey") %>%
  dplyr::filter(!labels_countries=="Slovakia") %>%
  dplyr::filter(!labels_countries=="France") %>%
  dplyr::filter(!labels_countries=="other") %>%
  dplyr::filter(!labels_countries=="Russia") %>%
  dplyr::filter(!labels_countries=="Philippines") %>%
  dplyr::filter(!labels_countries=="Luxembourg")

NPS_score$"2020 Q1" <- NPS_score$nps_value
NPS_score$detractors <- NULL
NPS_score$neutrals <- NULL
NPS_score$promotors <- NULL
NPS_score$nps_value <- NULL

internal <- left_join(internal, NPS_score, by="labels_countries")
internal <- write_csv(internal, "internal.csv")


#nps_plot <- NPS_score %>%
#  plot_ly(x= ~ promotors,
#          y= ~ labels_countries,
#          marker = list(color = 'rgb(255,098,000)'),
#          type="bar", name="promotors") %>%
#  add_trace(x= ~ neutrals,
#            y= ~ labels_countries,
#            marker = list(color = 'rgb(82,81,153)'),
#            type="bar", name="neutrals") %>%
#  add_trace(x= ~ detractors,
#            y= ~ labels_countries,
#            marker = list(color = 'rgb(168, 168, 168)'),
#            type="bar", name="detractors") %>%
#  layout(xaxis = xaxis,
#         yaxis = yaxis,
#         barmode = "stack")
#nps_plot




