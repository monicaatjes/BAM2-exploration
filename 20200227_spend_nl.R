library(lubridate)

## Spending NL
spend<- read_excel("data/Nielsen ING en concurrenten wk1 2019-wk 2 2020-20200121.xlsx")

## add column date and quarter
spend$date<- as.Date(paste(spend$Jaar, spend$`Week Week code II`, 1, sep="-"), "%Y-%U-%u")
spend$labels_quarters <- as.yearqtr(spend$date, format ="%Y-%m-%d")
spend <- spend %>%
  dplyr::mutate(
  labels_quarters = as.yearqtr(labels_quarters, format='%Y Q%q')
)

spend$Concern <- NULL
spend$Adverteerder <- NULL
spend$Productfamilie <- NULL
spend$Productgroep <- NULL
spend$Jaar <- NULL
spend$`Week Week code II` <- NULL
spend$Product <- NULL

spend <- spend %>%
  mutate(
    label = case_when(
      Merk =="ING BANK" ~ "ING",
      Merk =="RABOBANK" ~ "Rabobank",
      Merk =="SNS" ~ "SNS bank",
      Merk =="TRIODOS BANK" ~ "Triodos Bank",
      TRUE ~ "NA_real") 
    )

## Select NL data 
data_NL <- data_customer_fig%>%
  filter(country==9) %>%
  filter(quarter_measurement > 12) 

spend$tmp1 <- "ALL"
spend$tmp2 <- "XXX"
spend$tmp3 <- "XXX"

# Labelling product classs
spend[, "prod_code"] <- NA_character_
spend[grepl("Algemeen", spend$Productklasse), "prod_code"] <- "BRAND"
spend[grepl("Hypotheken|Hypotheek", spend$Productklasse), "prod_code"] <- "MORTGAGE"
spend[grepl("Beleggingen", spend$Productklasse), "prod_code"] <- "INVESTMENT"
spend[grepl("Betaaldiensten", spend$Productklasse), "prod_code"] <- "CURRENTACCOUNT"
spend[grepl("Krediet", spend$Productklasse), "prod_code"] <- "LOANS"
spend[grepl("Sparen", spend$Productklasse), "prod_code"] <- "SAVINGS"
spend[grepl("Zakelijk krediet", spend$Productklasse), "prod_code"] <- "BUSINESSLOANS"
spend[grepl("Zakelijk algemeeen", spend$Productklasse), "prod_code"] <- "BUSINESSGENERAL"
spend[grepl("Bedrijfsverzekeringen|Brand|Reis|Ziektekosten|Verzekeringen", spend$Productklasse), "prod_code"] <- "INSURANCE"
spend[grepl("Credit", spend$Productklasse), "prod_code"] <- "CREDITCARD"
spend[is.na(spend$Productklasse), "prod_code"] <- "OTHER"

spend[, "prod_code"] <- NA_character_
spend[grepl("Bioscoop", spend$Productklasse), "prod_code"] <- "CINEMA"
spend[grepl("Dagbladen", spend$Productklasse), "prod_code"] <- "PAPERS"
spend[grepl("DM", spend$Productklasse), "prod_code"] <- "DM"
spend[grepl("Magazines", spend$Productklasse), "prod_code"] <- "MAGAZINES"
spend[grepl("Online", spend$Productklasse), "prod_code"] <- "DIG"
spend[grepl("OOH", spend$Productklasse), "prod_code"] <- "OOH"
spend[grepl("Radio", spend$Productklasse), "prod_code"] <- "RADIO"
spend[grepl("Televisie", spend$Productklasse), "prod_code"] <- "TV"
spend[is.na(spend$Productklasse), "prod_code"] <- "OTH"

## for all media
spend_medium <- spend %>%
  dcast(labels_quarters ~ `Mediumtypen (magazines totaal)` + tmp1 + tmp1, value.var = "Spend", fun.aggregate = sum,
        na.rm=T)
## for all products
spend_products <- spend %>%
  dcast(labels_quarters ~ tmp1 + prod_code + tmp1, value.var = "Spend", fun.aggregate = sum,
        na.rm=T)
## for all labels
spend_labels <- spend %>%
  dcast(labels_quarters ~ tmp1 + tmp1 + label, value.var = "Spend", fun.aggregate = sum,
        na.rm=T)
## all combi
spend_ALL <- spend %>%
  dcast(labels_quarters ~ `Mediumtypen (magazines totaal)` + prod_code + label, value.var = "Spend", fun.aggregate = sum,
        na.rm=T)
## medium * all * label
spend_med_label <- spend %>%
  dcast(labels_quarters ~ `Mediumtypen (magazines totaal)` + tmp1+ label, value.var = "Spend", fun.aggregate = sum,
        na.rm=T)

spend_list <-list(spend_medium, spend_products, spend_labels, spend_ALL, spend_med_label)
spend_1 <- Reduce(function(d1, d2) merge(d1, d2, by = "labels_quarters", all.x = TRUE, all.y = FALSE), 
                spend_list)

data_NL <-full_join(data_NL, spend_1, by=c("labels_quarters"))
  
rm(spend_1, spend_ALL, spend_labels, spend_list, spend_med_label, spend_medium, spend_products)

## add share of spend




