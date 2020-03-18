## Add Reptrak pulse scores

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
data$labels_quarters <-as.yearqtr(unlist(data$labels_quarters), format= '%Y Q%q')
data <- dplyr::left_join(data, rept3, by=c("labels_countries", "labels_quarters", "b_value"))



### INTERNAL NPS 
internal <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/20100124 Internal metrics .xlsx", range = "A26:I47")

# throw away empty column
internal <- internal[,-2]

# without transpose
internal$labels_countries <- internal$`Row Labels`
internal$`Row Labels` <- NULL
colnames(internal) <- as.yearqtr(colnames(internal[1:7]), format= 'Q%q_%Y') 
names(internal)[8] <- "labels_countries" 

internal1 <- internal %>%
  dplyr::select(labels_countries, `2019 Q2`, `2019 Q3`, `2019 Q4`) %>%
  dplyr::filter(!is.na(`2019 Q4`)) %>%
  dplyr::mutate(
    # Calculate CAGR
        cagr = (`2019 Q4`/`2019 Q2`)^(1/3) - 1,
  )
  
internal1 <- internal1 %>%
  dplyr::mutate(
    '2019 Q2'= internal1$'2019 Q2' *100,
    '2019 Q3'= internal1$'2019 Q3' *100,
    '2019 Q4'= internal1$'2019 Q4' *100)

internal_tab<- plot_ly(
  type = 'table',
  columnwidth = c(70, 40, 40, 40, 40),
  columnorder = c(0, 1, 2, 3, 4),
  header = list(
    values = c("country","Q2 2019", "Q3 2019", "Q4 2019", "CAGR"),
    align = c("center", "center", "center", "center", "center"),
    line = list(width = 1, color = c("rgb 168, 168, 168")),
    fill = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)")),
    font = list(family = "ING me", size = 14, color = "white")
  ),
  cells = list(
    values = rbind(internal1$labels_countries,
                   round(internal1$`2019 Q2`, digits=2),
                   round(internal1$`2019 Q3`, digits=2),
                   round(internal1$`2019 Q4`, digits=2),
                   round(internal1$cagr, digits=2)),
    align = c("center", "center", "center", "center", "center"),
    line = list(color = c("rgb 168, 168, 168"), width = 1),
    font = list(family = "ING me", size = 12, color = c("rgb 105, 105, 105"))
  ))


