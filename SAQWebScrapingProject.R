##SAQ Analysis

##Install and load necessary packages

##tidyverse for data wrangling
##install.packages("tidyverse", repo = 'https://mac.R-project.org')
library(tidyverse)
##rvest for HTML/XML parsing
##install.packages('rvest')
library(rvest)
##stringr for string manipulation (TBD if necessary)
##install.packages('stringr')
library(stringr)
##rebus for verbose regular expressions (TBD if necessary)
##install.packages('rebus')
library(rebus)
##lubridate for ease of time&date manipulation
##install.packages('lubridate')
library(lubridate)


##testing on single product

url <- 'https://www.saq.com/fr/12824197'
webpage <- read_html(url)

name <- webpage %>%
  html_nodes(".page-title") %>%
  html_text()
name <- str_replace_all(name, "[\r\n]", "")
price <- webpage %>%
  html_nodes('.price') %>%
  html_text()
price <- str_replace_all(price[1], "[\r\n]", " $")
info <- webpage %>%
  html_nodes('.type') %>%
  html_text()
type <- str_replace_all(info[1], "[\r\n]", "")
volume <- str_replace_all(info[2], "[\r\n]", "")
origin <- str_replace_all(info[3], "[\r\n]", "")
region <- str_replace_all(info[4], "[\r\n]", "")

productinfo <- c(name, price, type, volume, origin, region)
product <- data.frame()
product[1,1] <- productinfo[1]
product[1,2] <- productinfo[2]
product[1,3] <- productinfo[3]
product[1,4] <- productinfo[4]
product[1,5] <- productinfo[5]
product[1,6] <- productinfo[6]
head(product)

##testing on a page

url <- 'https://www.saq.com/fr/produits?p=1&product_list_order=name_asc'
webpage <- read_html(url)

name <- webpage %>%
  html_nodes(".product-item-name") %>%
  html_text()
name <- str_replace_all(name, space(), "")
price <- webpage %>%
  html_nodes('.price-box') %>%
  html_text()
price <- str_replace_all(price, space(), "")
info <- webpage %>%
  html_nodes('.product-item-identity-format') %>%
  html_text()
info <- str_replace_all(info, "[\r\n]", "")
info <- trimws(info)
type <- str_replace_all(substr(info,1, 25), space(), "")
volume <- str_replace_all(substr(info, 150, 250), space(), "")
origin <- str_replace_all(substr(info, 450, 500), space(), "")

products <- cbind(name, price, type, volume, origin)
head(products)

##Now this has been cleared beforehand
##Let us apply this on all pages for SAQ products
##we will use iteration to get the same page info far all available pages
##Note that we must verify the number of products available which may change
##on a daily basis depending on offerings and availability

productnumber <- 13993
pagenumber <- ceiling(productnumber/24)

namelist <- list()
pricelist <- list()
typelist <- list()
volumelist <- list()
originlist <- list()

for (j in 2:pagenumber){
  url <- paste0('https://www.saq.com/fr/produits?p=', j, '&product_list_order=name_asc')
  webpage <- read_html(url)
  
  price <- webpage %>%
    html_nodes('.price-box') %>%
    html_text()
  price <- str_replace_all(price, space(), "")
    name <- webpage %>%
      html_nodes(".product-item-name") %>%
      html_text()
    name <- str_replace_all(name, space(), "")
    info <- webpage %>%
      html_nodes('.product-item-identity-format') %>%
      html_text()
    info <- str_replace_all(info, "[\r\n]", "")
    info <- trimws(info)
    type <- str_replace_all(substr(info,1, 25), space(), "")
    volume <- str_replace_all(substr(info, 150, 250), space(), "")
    origin <- str_replace_all(substr(info, 450, 500), space(), "")
    namelist[[(j-1)]] <- name
    pricelist[[(j-1)]] <- price
    typelist[[(j-1)]] <- type
    volumelist[[(j-1)]] <- volume
    originlist[[(j-1)]] <- origin

  newproducts <- cbind(name, price, type, volume, origin)
  products <- rbind(products, newproducts)

}

head(products)

##this gives us a large dataset with product names, prices, type,
##volume & country of origin
##let us create a csv with the information

write.csv(products, "saqproducts20200719.csv")


