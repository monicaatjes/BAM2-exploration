## Add customer figures

# open data latest quarter
customer2019_latest<-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q4 2019.xlsx", range = "B2:P8")
# Transpose data
customer2019_latest <-as.data.frame(t(customer2019_latest))
# Add column for quarter
customer2019_latest$quarter <-rownames(customer2019_latest)[1]
# Add headers which are row number 1 now and remove first row
names(customer2019_latest) <- lapply(customer2019_latest[1, ], as.character)
customer2019_latest <- customer2019_latest[-1,]    
customer2019_latest$country <- row.names(customer2019_latest)

# open data
customer2019_one<-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q4 2019.xlsx", range = "B10:P16")
# Transpose data
customer2019_one <-as.data.frame(t(customer2019_one))
# Add column for quarter
customer2019_one$quarter <-rownames(customer2019_one)[1]
# Add headers which are row number 1 now and remove first row
names(customer2019_one) <- lapply(customer2019_one[1, ], as.character)
customer2019_one <- customer2019_one[-1,]  
customer2019_one$country <- row.names(customer2019_one)

# open data
customer2019_two <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q4 2019.xlsx", range = "B18:P24")
# Transpose data
customer2019_two <-as.data.frame(t(customer2019_two))
# Add column for quarter
customer2019_two$quarter <-rownames(customer2019_two)[1]
# Add headers which are row number 1 now and remove first row
names(customer2019_two) <- lapply(customer2019_two[1, ], as.character)
customer2019_two <- customer2019_two[-1,] 
customer2019_two$country <- row.names(customer2019_two)

# open data
customer2019_three <-read_excel("/Users/xo21bm/Documents/Lokaal/BAM2/exploration/data/Customer figures update Q4 2019.xlsx", range = "B26:P32")
# Transpose data
customer2019_three <-as.data.frame(t(customer2019_three))
# Add column for quarter
customer2019_three$quarter <-rownames(customer2019_three)[1]
# Add headers which are row number 1 now and remove first row
names(customer2019_three) <- lapply(customer2019_three[1, ], as.character)
customer2019_three <- customer2019_three[-1,] 
customer2019_three$country <- row.names(customer2019_three)

customer <- dplyr::bind_rows(list(customer2019_latest, customer2019_one), list(customer2019_two, customer2019_three))

# gather across quarters
customer_long <-customer %>%
  dplyr::select("country", "Total (operative) customers","Active customers", "Payment customers", "Payment with r.income", 
         "Primary bank customers", "Avg number of product categories per active customer", "Y2019 December",
         "Y2019 September", "Y2018 December", "Y2018 September" ) %>%
  tidyr::gather(quarter, value, "Y2019 December":"Y2018 September") %>%
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
      quarter == "Y2019 December" ~ "Q4_2019",
      quarter == "Y2019 September" ~ "Q3_2019",
      quarter == "Y2018 December" ~ "Q4_2018",
      quarter == "Y2018 Septemberr" ~ "Q3_2018",
      TRUE ~ "NA_real_") 
  ) 

customer_long$quarter <- NULL
customer_long$value   <- NULL
customer_long$country <- NULL
customer_long$b_value <- 1

rm(customer2019_latest, customer, customer2019_one, customer2019_three, customer2019_two)

data_customer_fig <-left_join(data, customer_long, by=c("labels_countries", "labels_quarters", "b_value"))  
  
  
  


                            


     