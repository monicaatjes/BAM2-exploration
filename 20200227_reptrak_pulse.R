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
rownames(reptrakQ1) <- NULL
rm(reptrakQ1_)

# combine the different quarters in one file
rept <- dplyr::bind_rows(list(c(reptrakQ1, reptrakQ2, id=NULL)))

# change the factors to something normal
reptrak_$`RepTrak® Pulse` <- as.numeric(as.character(reptrak_$`RepTrak® Pulse`))
reptrak_$`Products & Services` <- as.numeric(as.character(reptrak_$`Products & Services`))
reptrak_$Innovation <- as.numeric(as.character(reptrak_$Innovation)) 
reptrak_$Workplace <- as.numeric(as.character(reptrak_$Workplace))
reptrak_$Governance <- as.numeric(as.character(reptrak_$Governance))
reptrak_$Citizenship <- as.numeric(as.character(reptrak_$Citizenship))
reptrak_$Leadership <- as.numeric(as.character(reptrak_$Leadership))
reptrak_$Performance <- as.numeric(as.character(reptrak_$Performance))
reptrak_$Purchase <- as.numeric(as.character(reptrak_$Purchase))
reptrak_$Recommend <- as.numeric(as.character(reptrak_$Recommend))
reptrak_$`Crisis proof` <- as.numeric(as.character(reptrak_$`Crisis proof`))
reptrak_$`Verbal support` <- as.numeric(as.character(reptrak_$`Verbal support`))
reptrak_$Invest <- as.numeric(as.character(reptrak_$Invest))
reptrak_$`Work for` <- as.numeric(as.character(reptrak_$`Work for`))


