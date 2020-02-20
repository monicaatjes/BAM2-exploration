## Add customer figures

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

customer <- dplyr::bind_rows(list(customer2019_latest, customer2019_one), list(customer2019_two, customer2019_three))

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
      country =="NL" ~ "Netherlands",
      country =="POL" ~ "Poland",
      country =="ROM" ~ "Romania",
      country =="SPA" ~ "Spain",
      country =="TUR" ~ "Turkey",
      TRUE ~ "NA_real_") 
    ) %>%
  dplyr::mutate(
    labels_quarters = case_when(
      quarter =="Y2019 December" ~ "Q4_2019",
      quarter =="Y2019 September"~ "Q3_2019",
      quarter =="Y2018 December" ~ "Q4_2018",
      quarter =="Y2018 September"~ "Q3_2018",
      TRUE ~ "NA_real_") 
  ) %>%
  distinct()

customer$quarter <- NULL
customer$country <- NULL
customer$b_value <- 1

rm(customer2019_latest, customer2019_one, customer2019_three, customer2019_two)
data_customer_fig <- left_join(data, customer, by=c("labels_countries", "labels_quarters", "b_value"))  
   
  
  


                            


     