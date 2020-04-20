## Add customer figures
customer2020_latest<-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q1 2020.xlsx", range = "B2:P8")
# Transpose data
customer2020_latest <- as.data.frame(t(customer2020_latest))
colnames(customer2020_latest) = as.character(unlist(customer2020_latest[1,]))
customer2020_latest$country <-rownames(customer2020_latest)
# Add column for quarter
customer2020_latest$quarter <-rownames(customer2020_latest)[1]
customer2020_latest = customer2020_latest[-1,]
customer2020_latest <- as_tibble(customer2020_latest)

customer2020_latest$`Active customers` <- as.numeric(as.character(customer2020_latest$`Active customers`)) * 1000
customer2020_latest$`Total (operative) customers` <- as.numeric(as.character(customer2020_latest$`Total (operative) customers`)) * 1000
customer2020_latest$`Payment customers` <- as.numeric(as.character(customer2020_latest$`Payment customers`)) * 1000
customer2020_latest$`Payment with r.income` <- as.numeric(as.character(customer2020_latest$`Payment with r.income`)) * 1000
customer2020_latest$`Primary bank customers` <- as.numeric(as.character(customer2020_latest$`Primary bank customers`)) * 1000
customer2020_latest$`Avg number of product categories per active customer` <- as.numeric(as.character(customer2020_latest$`Avg number of product categories per active customer`))

# open data latest quarter
customer2019_latest<-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q4 2019.xlsx", range = "B2:P8")
# Transpose data
customer2019_latest <- as.data.frame(t(customer2019_latest))
colnames(customer2019_latest) = as.character(unlist(customer2019_latest[1,]))
customer2019_latest$country <-rownames(customer2019_latest)
# Add column for quarter
customer2019_latest$quarter <-rownames(customer2019_latest)[1]
customer2019_latest = customer2019_latest[-1,]
customer2019_latest <- as_tibble(customer2019_latest)

customer2019_latest$`Active customers` <- as.numeric(as.character(customer2019_latest$`Active customers`)) * 1000
customer2019_latest$`Total (operative) customers` <- as.numeric(as.character(customer2019_latest$`Total (operative) customers`)) * 1000
customer2019_latest$`Payment customers` <- as.numeric(as.character(customer2019_latest$`Payment customers`)) * 1000
customer2019_latest$`Payment with r.income` <- as.numeric(as.character(customer2019_latest$`Payment with r.income`)) * 1000
customer2019_latest$`Primary bank customers` <- as.numeric(as.character(customer2019_latest$`Primary bank customers`)) * 1000
customer2019_latest$`Avg number of product categories per active customer` <- as.numeric(as.character(customer2019_latest$`Avg number of product categories per active customer`))

# open data
customer2019_one<-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q4 2019.xlsx", range = "B10:P16")
# Transpose data
customer2019_one <- as.data.frame(t(customer2019_one))
colnames(customer2019_one) = as.character(unlist(customer2019_one[1,]))
customer2019_one$country <-rownames(customer2019_one)
# Add column for quarter
customer2019_one$quarter <-rownames(customer2019_one)[1]
customer2019_one = customer2019_one[-1,]
customer2019_one <- as_tibble(customer2019_one)

customer2019_one$`Active customers` <- as.numeric(as.character(customer2019_one$`Active customers`)) * 1000
customer2019_one$`Total (operative) customers` <- as.numeric(as.character(customer2019_one$`Total (operative) customers`)) * 1000
customer2019_one$`Payment customers` <- as.numeric(as.character(customer2019_one$`Payment customers`)) * 1000
customer2019_one$`Payment with r.income` <- as.numeric(as.character(customer2019_one$`Payment with r.income`)) * 1000
customer2019_one$`Primary bank customers` <- as.numeric(as.character(customer2019_one$`Primary bank customers`)) * 1000
customer2019_one$`Avg number of product categories per active customer` <- as.numeric(as.character(customer2019_one$`Avg number of product categories per active customer`)) 


# open data
customer2019_two <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q4 2019.xlsx", range = "B18:P24")
# Transpose data
customer2019_two <- as.data.frame(t(customer2019_two))
colnames(customer2019_two) = as.character(unlist(customer2019_two[1,]))
customer2019_two$country <-rownames(customer2019_two)
# Add column for quarter
customer2019_two$quarter <-rownames(customer2019_two)[1]
customer2019_two = customer2019_two[-1,]
customer2019_two <- as_tibble(customer2019_two)

customer2019_two$`Active customers` <- as.numeric(as.character(customer2019_two$`Active customers`)) * 1000
customer2019_two$`Total (operative) customers` <- as.numeric(as.character(customer2019_two$`Total (operative) customers`)) * 1000
customer2019_two$`Payment customers` <- as.numeric(as.character(customer2019_two$`Payment customers`)) * 1000
customer2019_two$`Payment with r.income` <- as.numeric(as.character(customer2019_two$`Payment with r.income`)) * 1000
customer2019_two$`Primary bank customers` <- as.numeric(as.character(customer2019_two$`Primary bank customers`)) * 1000
customer2019_two$`Avg number of product categories per active customer` <- as.numeric(as.character(customer2019_two$`Avg number of product categories per active customer`)) 


# open data
customer2019_three <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q4 2019.xlsx", range = "B26:P32")
# Transpose data
customer2019_three <- as.data.frame(t(customer2019_three))
colnames(customer2019_three) = as.character(unlist(customer2019_three[1,]))
customer2019_three$country <-rownames(customer2019_three)
# Add column for quarter
customer2019_three$quarter <-rownames(customer2019_three)[1]
customer2019_three = customer2019_three[-1,]
customer2019_three <- as_tibble(customer2019_three)

customer2019_three$`Active customers` <- as.numeric(as.character(customer2019_three$`Active customers`)) * 1000
customer2019_three$`Total (operative) customers` <- as.numeric(as.character(customer2019_three$`Total (operative) customers`)) * 1000
customer2019_three$`Payment customers` <- as.numeric(as.character(customer2019_three$`Payment customers`)) * 1000
customer2019_three$`Payment with r.income` <- as.numeric(as.character(customer2019_three$`Payment with r.income`)) * 1000
customer2019_three$`Primary bank customers` <- as.numeric(as.character(customer2019_three$`Primary bank customers`)) * 1000
customer2019_three$`Avg number of product categories per active customer` <- as.numeric(as.character(customer2019_three$`Avg number of product categories per active customer`)) 

## open data
customer2019_q2 <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q2 2019.xlsx", range = "B2:P8")
# Transpose data
customer2019_q2 <- as.data.frame(t(customer2019_q2))
colnames(customer2019_q2) = as.character(unlist(customer2019_q2[1,]))
customer2019_q2$country <-rownames(customer2019_q2)
# Add column for quarter
customer2019_q2$quarter <-rownames(customer2019_q2)[1]
customer2019_q2 = customer2019_q2[-1,]
customer2019_q2 <- as_tibble(customer2019_q2)

customer2019_q2$`Active customers` <- as.numeric(as.character(customer2019_q2$`Active customers`)) * 1000
customer2019_q2$`Total (operative) customers` <- as.numeric(as.character(customer2019_q2$`Total (operative) customers`)) * 1000
customer2019_q2$`Payment customers` <- as.numeric(as.character(customer2019_q2$`Payment customers`)) * 1000
customer2019_q2$`Payment with r.income` <- as.numeric(as.character(customer2019_q2$`Payment with r.income`)) * 1000
customer2019_q2$`Primary bank customers` <- as.numeric(as.character(customer2019_q2$`Primary bank customers`)) * 1000
customer2019_q2$`Avg number of product categories per active customer` <- as.numeric(as.character(customer2019_q2$`Avg number of product categories per active customer`)) 

##
customer2019_q1 <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q2 2019.xlsx", range = "B10:P16")
# Transpose data
customer2019_q1 <- as.data.frame(t(customer2019_q1))
colnames(customer2019_q1) = as.character(unlist(customer2019_q1[1,]))
customer2019_q1$country <-rownames(customer2019_q1)
# Add column for quarter
customer2019_q1$quarter <-rownames(customer2019_q1)[1]
customer2019_q1 = customer2019_q1[-1,]
customer2019_q1 <- as_tibble(customer2019_q1)

customer2019_q1$`Active customers` <- as.numeric(as.character(customer2019_q1$`Active customers`)) * 1000
customer2019_q1$`Total (operative) customers` <- as.numeric(as.character(customer2019_q1$`Total (operative) customers`)) * 1000
customer2019_q1$`Payment customers` <- as.numeric(as.character(customer2019_q1$`Payment customers`)) * 1000
customer2019_q1$`Payment with r.income` <- as.numeric(as.character(customer2019_q1$`Payment with r.income`)) * 1000
customer2019_q1$`Primary bank customers` <- as.numeric(as.character(customer2019_q1$`Primary bank customers`)) * 1000
customer2019_q1$`Avg number of product categories per active customer` <- as.numeric(as.character(customer2019_q1$`Avg number of product categories per active customer`)) 


customer <- dplyr::bind_rows(list(customer2019_latest, customer2019_one), list(customer2019_two, customer2019_three), list(customer2019_q1, customer2019_q2))
customer <- dplyr::bind_rows(list(customer2020_latest, customer))

rm(customer2019_latest, customer2019_one, customer2019_q1, customer2019_q2, customer2019_three, customer2019_two)

## rename
customer <-customer %>%
  dplyr::select("country", "Total (operative) customers","Active customers", "Payment customers", "Payment with r.income", 
         "Primary bank customers", "Avg number of product categories per active customer", "quarter") %>%
  dplyr::mutate(
    labels_countries = case_when(
      country =="AUS" ~ "Australia",
      country =="AUT" ~ "Austria",
      country =="BEL" ~ "Belgium",
      country =="CZE" ~ "Czech",
      country =="FRA" ~ "France",
      country =="GER" ~ "Germany",
      country =="ITA" ~ "Italy",
      country =="LUX" ~ "Luxembourg",
      country =="NL" ~ "The Netherlands",
      country =="POL" ~ "Poland",
      country =="ROM" ~ "Romania",
      country =="SPA" ~ "Spain",
      country =="TUR" ~ "Turkey",
      TRUE ~ "NA_real_") 
    ) %>%
  dplyr::mutate(
    labels_quarters = case_when(
      quarter =="Y2020 March" ~ "2020 Q1",
      quarter =="Y2019 December" ~ "2019 Q4",
      quarter =="Y2019 September"~ "2019 Q3",
      quarter =="Y2018 December" ~ "2018 Q4",
      quarter =="Y2018 September"~ "2018 Q3",
      quarter =="Y2019 June" ~ "2019 Q2",
      quarter =="Y2019 March"~ "2019 Q1",
      TRUE ~ "NA_real_") 
  )

customer$quarter <- NULL
customer$country <- NULL
customer$b_value <- 1

#data$labels_quarters <- as.yearqtr(unlist(data$labels_quarters), format='%Y Q%q')
#customer$labels_quarters <- as.yearqtr(unlist(customer$labels_quarters), format='%Y Q%q')
test_customer_fig <- left_join(test, customer, by=c("labels_countries", "labels_quarters", "b_value"))  

test_customer_fig <- test_customer_fig %>%
  dplyr::mutate(
    labels_quarters = as.yearqtr(labels_quarters, format='%Y Q%q')
  )

test_customer_fig <-write_csv(test_customer_fig, "test_customer_fig.csv")
  

                     


     