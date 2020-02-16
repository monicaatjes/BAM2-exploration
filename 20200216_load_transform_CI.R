## Add customer figures-> first transpose in Excel
library(readxl)
library(tidyxl)

customer <- readxl::read_excel("Customer figures update Q4 2019.xlsx")[,-c(16:18)] 
ci <- xlsx_cells(customer)
ci <- dplyr::glimpse(customer)

