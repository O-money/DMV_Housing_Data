
### R Packages ###
{
library(ggplot2)
library(dplyr)### Cool Graphs
library(tidyverse) ### Different Data configuartions
library(data.table)
library(MASS) ### Robustness
library(wooldridge) ### Built in Dataset from Text book
library(stargazer) ### Latex Output.
library(dplyr) ### ?
library(stringdist) ### Fuzzy Match
library(sf)
library(readxl)
library(lmtest)
library(reshape2)
library(rvest)
library(rvest)
library(tidyverse)
library(stringr)
library(rvest)
library(httr)
library(writexl)
library(readxl)
library(purrr)
library(jsonlite)
library(tidycensus) 
library(googleway)
library(tidygeocoder)
library(googlesheets4)
google_api_key<-(Sys.getenv("GOOGLE_API_KEY"))
file.edit("~/.Renviron")

}
### Don't Run !!Web scrape DMV ###
{
  
  ALX_scrape_zillow_page <- function(page_number) {
    base_url <- "https://www.zillow.com/alexandria-city-va/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <- paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                    page_number,
                    "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A38.85256100252295%2C%22south%22%3A38.77765885481591%2C%22east%22%3A-77.0233605788574%2C%22west%22%3A-77.15828642114256%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%2C%22manu%22%3A%7B%22value%22%3Afalse%7D%2C%22apa%22%3A%7B%22value%22%3Afalse%7D%2C%22auc%22%3A%7B%22value%22%3Afalse%7D%2C%22nc%22%3A%7B%22value%22%3Afalse%7D%2C%22fsbo%22%3A%7B%22value%22%3Afalse%7D%2C%22mf%22%3A%7B%22value%22%3Afalse%7D%7D%2C%22isListVisible%22%3Atrue%2C%22mapZoom%22%3A13%2C%22usersSearchTerm%22%3A%22Alexandria%20City%2C%20VA%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A3253%2C%22regionType%22%3A4%7D%5D%7D")
    full_url <- paste0(base_url, page_suffix, query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(5, 10, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
  
  Aling_scrape_zillow_page <- function(page_number) {
    base_url <- "https://www.zillow.com/arlington-va/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <- paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                    page_number,
                    "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A38.955630236789695%2C%22south%22%3A38.805964274059896%2C%22east%22%3A-76.96607815771485%2C%22west%22%3A-77.23592984228516%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%2C%22manu%22%3A%7B%22value%22%3Afalse%7D%2C%22apa%22%3A%7B%22value%22%3Afalse%7D%2C%22auc%22%3A%7B%22value%22%3Afalse%7D%2C%22nc%22%3A%7B%22value%22%3Afalse%7D%2C%22fsbo%22%3A%7B%22value%22%3Afalse%7D%2C%22mf%22%3A%7B%22value%22%3Afalse%7D%7D%2C%22isListVisible%22%3Atrue%2C%22mapZoom%22%3A12%2C%22usersSearchTerm%22%3A%22Arlington%2C%20VA%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A30258%2C%22regionType%22%3A6%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix,query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(5, 10, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
  
  Mont_Cy_scrape_zillow_page <- function(page_number) {
    base_url <- "https://www.zillow.com/montgomery-county-md/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <- paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                    page_number,
                    "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A39.441821531971065%2C%22south%22%3A38.84538403704076%2C%22east%22%3A-76.66839163085938%2C%22west%22%3A-77.74779836914063%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%2C%22manu%22%3A%7B%22value%22%3Afalse%7D%2C%22apa%22%3A%7B%22value%22%3Afalse%7D%2C%22auc%22%3A%7B%22value%22%3Afalse%7D%2C%22nc%22%3A%7B%22value%22%3Afalse%7D%2C%22fsbo%22%3A%7B%22value%22%3Afalse%7D%2C%22mf%22%3A%7B%22value%22%3Afalse%7D%7D%2C%22isListVisible%22%3Atrue%2C%22usersSearchTerm%22%3A%22Montgomery%20County%2C%20MD%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A2975%2C%22regionType%22%3A4%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix,query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(5, 10, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
  
  PG_Cy_scrape_zillow_page <- function(page_number) {
    base_url <- "https://www.zillow.com/prince-georges-county-md/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <- paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                    page_number,
                    "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A39.132885280826706%2C%22south%22%3A38.53382780994984%2C%22east%22%3A-76.3396836308594%2C%22west%22%3A-77.41909036914065%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%2C%22manu%22%3A%7B%22value%22%3Afalse%7D%2C%22apa%22%3A%7B%22value%22%3Afalse%7D%2C%22auc%22%3A%7B%22value%22%3Afalse%7D%2C%22nc%22%3A%7B%22value%22%3Afalse%7D%2C%22fsbo%22%3A%7B%22value%22%3Afalse%7D%2C%22mf%22%3A%7B%22value%22%3Afalse%7D%7D%2C%22isListVisible%22%3Atrue%2C%22usersSearchTerm%22%3A%22Prince%20Georges%20County%2C%20MD%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A3246%2C%22regionType%22%3A4%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix,query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(2, 5, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
  
  WASH_scrape_zillow_page <- function(page_number) {
    base_url <- "https://www.zillow.com/washington-dc/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <- paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                    page_number,
                    "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A39.04315092156964%2C%22south%22%3A38.74387315322994%2C%22east%22%3A-76.74472431542968%2C%22west%22%3A-77.2844276845703%7D%2C%22usersSearchTerm%22%3A%22Washington%20DC%22%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%2C%22manu%22%3A%7B%22value%22%3Afalse%7D%2C%22land%22%3A%7B%22value%22%3Afalse%7D%2C%22auc%22%3A%7B%22value%22%3Afalse%7D%2C%22fore%22%3A%7B%22value%22%3Afalse%7D%2C%22cmsn%22%3A%7B%22value%22%3Afalse%7D%2C%22nc%22%3A%7B%22value%22%3Afalse%7D%2C%22fsbo%22%3A%7B%22value%22%3Afalse%7D%7D%2C%22isListVisible%22%3Atrue%2C%22mapZoom%22%3A11%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A41568%2C%22regionType%22%3A6%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix,query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(5, 10, by = 0.5), 1))
    return(data.frame(Price = price, Address = address,Specs = specs, stringsAsFactors = FALSE)) 
  }
  
  Fax_scrape_zillow_page <- function(page_number) {
    base_url <- "https://www.zillow.com/fairfax-county-va/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <- paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                    page_number,
                    "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A39.057004%2C%22south%22%3A38.605617%2C%22east%22%3A-77.041362%2C%22west%22%3A-77.536718%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%7D%2C%22isListVisible%22%3Atrue%2C%22usersSearchTerm%22%3A%22Fairfax%20County%2C%20VA%22%2C%22category%22%3A%22cat1%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A1694%2C%22regionType%22%3A4%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix, query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(5, 10, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
  
  pw_scrape_zillow_page <- function(page_number ) {
    base_url <- "https://www.zillow.com/prince-william-county-va/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <- paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                    page_number,
                    "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A39.02158438979498%2C%22south%22%3A38.421587334531345%2C%22east%22%3A-76.92758663085938%2C%22west%22%3A-78.00699336914063%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%7D%2C%22isListVisible%22%3Atrue%2C%22usersSearchTerm%22%3A%22Prince%20William%20County%2C%20VA%22%2C%22category%22%3A%22cat1%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A3247%2C%22regionType%22%3A4%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix,query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(5, 10, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
  
  lc_scrape_zillow_page <- function(page_number ) {
    base_url <- "https://www.zillow.com/loudoun-county-va/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <- paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                    page_number,
                    "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A39.38372261855509%2C%22south%22%3A38.786791064745366%2C%22east%22%3A-77.10449613085936%2C%22west%22%3A-78.18390286914061%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%7D%2C%22isListVisible%22%3Atrue%2C%22usersSearchTerm%22%3A%22Loudoun%20County%2C%20VA%22%2C%22category%22%3A%22cat1%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A1887%2C%22regionType%22%3A4%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix,query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(5, 10, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
  
  fc_scrape_zillow_page <- function(page_number) {
    base_url <- "https://www.zillow.com/frederick-county-md/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <-paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                   page_number,
                   "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A39.766806293727036%2C%22south%22%3A39.17314382948595%2C%22east%22%3A-76.85223313085937%2C%22west%22%3A-77.93163986914062%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%7D%2C%22isListVisible%22%3Atrue%2C%22usersSearchTerm%22%3A%22Frederick%20County%2C%20MD%22%2C%22category%22%3A%22cat1%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A2699%2C%22regionType%22%3A4%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix,query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(2, 5, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
  
  aac_scrape_zillow_page <- function(page_number) {
    base_url <- "https://www.zillow.com/anne-arundel-county-md/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <-paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                   page_number,
                   "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A39.27305949279543%2C%22south%22%3A38.6751886070646%2C%22east%22%3A-76.05522513085938%2C%22west%22%3A-77.13463186914063%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%7D%2C%22isListVisible%22%3Atrue%2C%22usersSearchTerm%22%3A%22Anne%20Arundel%20County%2C%20MD%22%2C%22category%22%3A%22cat1%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A3152%2C%22regionType%22%3A4%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix,query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(5, 10, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
  
  cc_scrape_zillow_page <- function(page_number) {
    base_url <- "https://www.zillow.com/charles-county-md/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <- paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                    page_number,
                    "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A38.73977383945408%2C%22south%22%3A38.13740804481022%2C%22east%22%3A-76.45305813085936%2C%22west%22%3A-77.53246486914061%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%7D%2C%22isListVisible%22%3Atrue%2C%22usersSearchTerm%22%3A%22Charles%20County%2C%20MD%22%2C%22category%22%3A%22cat1%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A1607%2C%22regionType%22%3A4%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix,query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(5, 10, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
  
  Staf_scrape_zillow_page <- function(page_number) {
    base_url <- "https://www.zillow.com/stafford-county-va/"
    page_suffix <- ifelse(page_number == 1, "", paste0(page_number, "_p/"))
    query <- paste0("?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A",
                    page_number,
                    "%7D%2C%22isMapVisible%22%3Atrue%2C%22mapBounds%22%3A%7B%22north%22%3A38.71821928386597%2C%22south%22%3A38.11567292063594%2C%22east%22%3A-76.92103013085938%2C%22west%22%3A-78.00043686914063%7D%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%7D%2C%22isListVisible%22%3Atrue%2C%22usersSearchTerm%22%3A%22Stafford%20County%2C%20VA%22%2C%22category%22%3A%22cat1%22%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A2556%2C%22regionType%22%3A4%7D%5D%7D")
    full_url <- paste0(base_url,page_suffix,query)
    webpage <- read_html(full_url)
    price <- html_nodes(webpage, ".jCoXOF") %>% html_text()
    address <- html_nodes(webpage, "address") %>% html_text()
    specs <- html_nodes(webpage, ".fqJdKU") %>% html_text()
    Sys.sleep(sample(seq(5, 10, by = 0.5), 1))
    return(data.frame(Price = price, Address = address, Specs = specs, stringsAsFactors = FALSE))
  }
}
### Don't Run !! Data Set DMV ###
{
ALX_zillow_data <- lapply(1:10,ALX_scrape_zillow_page)
ALing_zillow_data <- lapply(1:10,Aling_scrape_zillow_page)
Mont_Cy_data<-lapply(1:15,Mont_Cy_scrape_zillow_page)
PG_CY_data<-lapply(1:15,PG_Cy_scrape_zillow_page)
WASH_DC_data<-lapply(1:15,WASH_scrape_zillow_page)
FAX_CY_data<-lapply(1:15,Fax_scrape_zillow_page)
Pr_Will_CY_data<-lapply(1:10,pw_scrape_zillow_page)
Loud_CY_data<-lapply(1:10,lc_scrape_zillow_page)
FredMD_CY_data<-lapply(1:8,fc_scrape_zillow_page)
AnneAD_CY<-lapply(1:10,aac_scrape_zillow_page)
Charles_CY<-lapply(1:8,cc_scrape_zillow_page)
Stafford_CY<-lapply(1:8,Staf_scrape_zillow_page)

}
 ### Combine all the data and clean it ###
{
zillow_df <- bind_rows(WASH_DC_data,ALX_zillow_data,ALing_zillow_data
                       ,Mont_Cy_data,PG_CY_data,FAX_CY_data,Pr_Will_CY_data,
                       Loud_CY_data,FredMD_CY_data,Charles_CY,Stafford_CY)


zillow_df1 <- zillow_df[!duplicated(zillow_df$Address), ]

zillow_df_New <- zillow_df1 %>%
  separate(Specs, into = c("Details", "Type"), sep = "-") %>%
  mutate(
    Bedrooms = str_extract(Details, "\\d+ bd"),
    Bathrooms = str_extract(Details, "\\d+ ba"),
    Sqft = str_extract(Details, "\\d{1,3}(,\\d{3})?\\ssqft"),
    zip = str_extract(Address, "\\d{5}$"),
    State = str_extract(Address, "\\b[A-Z]{2}\\b(?=\\s\\d{5})"),
    City = str_extract(Address, "(?<=,\\s)[^,]+(?=,\\s[A-Z]{2}\\s\\d{5})"))
zillow_df_New$Price<- ifelse(str_detect(zillow_df_New$Price, "K"),gsub("K", ",000", zillow_df_New$Price),zillow_df_New$Price) 
zillow_df_New$Num_price<-gsub("\\$", "",zillow_df_New$Price)
zillow_df_New$Num_Bedrooms<-gsub("bd", "",zillow_df_New$Bedrooms)
zillow_df_New$Num_Bathrooms<-gsub("ba", "",zillow_df_New$Bathrooms)
zillow_df_New$Num_Sqfts<-gsub("sqft", "",zillow_df_New$Sqft)

zillow_df_New <- zillow_df_New %>%
  mutate(
    Num_price = as.numeric(str_remove_all(Num_price, "[^0-9]")),
    Num_Bedrooms = as.numeric(str_remove_all(Num_Bedrooms, "[^0-9]")),
    Num_Bathrooms = as.numeric(str_remove_all(Num_Bathrooms, "[^0-9\\.]")), 
    Num_Sqfts = as.numeric(str_remove_all(Num_Sqfts, "[^0-9]")))
  
}
### Demographic
{
  my_variables <- c(
    total_pop = "B01003_001",
    med_income = "B19013_001",
    total_households = "B11001_001",
    Older_25 ="B15003_001",
    BS_BA= "B15003_022",
    MS_MA= "B15003_023",
    White= "B02001_002",
    Asian="B02001_005",
    Black="B02001_003",
    Overall_Hisp="B03002_012",
    Overall_Poor="B17001_002",
    Gross_Rent="B25064_001",
    Month_MortgaePlus = "B25088_002")
  
  # Use a list of unique ZIP codes from your dataset to avoid redundant API calls.
  zip_list <- unique(zillow_df_New$zip)
  
  results_list <- purrr::map(zip_list, function(zip_code) {
    safely_get_acs <- purrr::possibly(get_acs, otherwise = NULL)
    safely_get_acs(
      geography = "zcta",
      variables = my_variables,
      zcta = zip_code,
      year = 2023,
      output = "wide"
    )
  })
  # Get rid of error response
  demo_data_wide <- results_list %>%
    discard(is.null) %>%
    bind_rows()
  # 1. Select the necessary columns from demo_data_wide and rename them for clarity.
  #    We will select GEOID (the zip code) and total_popM (the population estimate).
  #    Renaming 'GEOID' to 'zip' allows the join to be performed automatically.
  
  population_data <- demo_data_wide %>%
    dplyr::select(GEOID, total_popE,med_incomeE, total_householdsE,BS_BAE, MS_MAE, WhiteE, AsianE,BlackE, Overall_HispE, Overall_PoorE,Older_25E,Gross_RentE, Month_MortgaePlusE) %>%
    dplyr::rename(zip = GEOID, 
                  population = total_popE,
                  Med_Income = med_incomeE, 
                  Med_Household = total_householdsE,
                  Older25= Older_25E,
                  BS_BA = BS_BAE,
                  MS_MA = MS_MAE,
                  White = WhiteE,
                  Asian = AsianE,
                  Black = BlackE,
                  Overall_Hisp = Overall_HispE,
                  Overall_Poor = Overall_PoorE,
                  Gross_Rent = Gross_RentE,
                  Month_MortgaePlus = Month_MortgaePlusE)
  # Renaming for a clean final dataset
  # 2. Perform a left_join() to merge the data frames.
  #    The 'by = "zip"' argument tells R to match rows where the 'zip' column is the same.
  zillow_df_New <- left_join(zillow_df_New, population_data, by = "zip")
}
### Get Distance for Downtown Washington D.C
{
  zip_list <- unique(zillow_df_New$zip)
  results_list_Dist <- list()
  destination_zip <- "20005"
  for (i in seq_along(zip_list)) {
    origin_zip <- zip_list[i]
    
    distance_result <- google_distance(
      origins = origin_zip,
      destinations = destination_zip,
      mode = "driving",
      key = google_api_key)
    
    distance_value <- NA
    duration_value <- NA
    error_message <- NA
    
    if (!is.null(distance_result$rows) &&
        length(distance_result$rows$elements) > 0 &&
        !is.null(distance_result$rows$elements[[1]]$distance) &&
        !is.null(distance_result$rows$elements[[1]]$duration)) {
      
      distance_value <- distance_result$rows$elements[[1]]$distance$value
      duration_value <- distance_result$rows$elements[[1]]$duration$value
    } else {
      error_message <- if (!is.null(distance_result$error_message)) distance_result$error_message else "Unknown error"
    }
    
    results_list_Dist[[i]] <- data.frame(
      origin = origin_zip,
      destination = destination_zip,
      distance_m = distance_value,
      duration_s = duration_value,
      error = error_message,
      stringsAsFactors = FALSE
    )
  }
  final_dist_data <-bind_rows(results_list_Dist)
  final_dist_data$miles<- round(final_dist_data$distance_m*(0.000621371),digits = 2)
  final_dist_data$minuties<- round(final_dist_data$duration_s/(60) ,digits = 2)
  final_dist_data$Hour<- round(final_dist_data$minuties/(60) ,digits = 2)
  print(final_dist_data)
  Distance_X <- final_dist_data %>%
    dplyr::select(origin,miles,minuties,Hour) %>%
    dplyr::rename(zip =origin, Miles_DT_DC=miles, Min_DT_DC = minuties, Hour_DT_DC = Hour)
  
  zillow_df_New <- left_join(zillow_df_New,Distance_X, by = "zip")
  
}
### Lon and Lat
{
  zip_list_Cri<-zip_list
  results_list_Crime <- list()
  for (i in seq_along(zip_list_Cri)) {
    zip_Crime <- zip_list_Cri[i]
    
    geo <- geo(address = zip_Crime, method = 'osm')
    lat <- geo$lat
    lon <- geo$long
    
    results_list_Crime[[i]] <- data.frame(
      zip=geo,
      Lat=lat,
      Lon=lon
      )
    
  }
  final_dist_data_crime <-bind_rows(results_list_Crime)
 
  Distance_C <- final_dist_data_crime %>%
    dplyr::select(zip.address,Lat,Lon) %>%
    dplyr::rename( zip = zip.address,latitude= Lat,longitude=Lon )
  
  zillow_df_New <- left_join(zillow_df_New,Distance_C, by = "zip")
  
  
  
}
### Make Binary Variables and Other Categories ###
{
### State 
zillow_df_New$In_VA<-ifelse(zillow_df_New$State=="VA",1,0)
zillow_df_New$In_MD<-ifelse(zillow_df_New$State=="MD",1,0)
zillow_df_New$In_DC<-ifelse(zillow_df_New$State=="DC",1,0)
### Property type #
zillow_df_New$Is_Condo<-ifelse(zillow_df_New$Type==" Condo for sale",1,0)
zillow_df_New$Is_Townhouse<-ifelse(zillow_df_New$Type==" Townhouse for sale",1,0)
zillow_df_New$Is_House <- ifelse((zillow_df_New$Type == " House for sale"),1,0)
#(zillow_df_New$Type == " New construction" & zillow_df_New$Num_Sqfts > 3000) |
#(zillow_df_New$Type == " Coming soon" & !grepl("APT|#", zillow_df_New$Address))
zillow_df_New$Is_Land_Lot<-ifelse(zillow_df_New$Type==" Lot / Land for sale",1,0)

### Percent Race and Age
zillow_df_New$WhitePercent<-round(zillow_df_New$White/zillow_df_New$population, digits = 2)
zillow_df_New$BlackPercent<-round(zillow_df_New$Black/zillow_df_New$population, digits = 2)
zillow_df_New$AsianPercent<-round(zillow_df_New$Asian/zillow_df_New$population, digits = 2)
zillow_df_New$HispanicPercent<-round(zillow_df_New$Overall_Hisp/zillow_df_New$population, digits = 2)
zillow_df_New$BA_BA_DegreePer<-round(zillow_df_New$BS_BA/zillow_df_New$Older25, digits = 2)
zillow_df_New$MA_MS_DegreePer<-round(zillow_df_New$MS_MA/zillow_df_New$Older25, digits = 2)
zillow_df_New$People_Young_Than_25<-zillow_df_New$population-zillow_df_New$Older25
zillow_df_New$People_Young_Than_25Percent<-round((zillow_df_New$People_Young_Than_25/zillow_df_New$population), digits = 2)
zillow_df_New$Poverty_Percent<-round((zillow_df_New$Overall_Poor/zillow_df_New$population), digits = 2)
### Majority Group
zillow_df_New$Maj_White<-ifelse(zillow_df_New$WhitePercent>=.50,1,0)
zillow_df_New$Maj_Black<-ifelse(zillow_df_New$BlackPercent>=.50,1,0)
zillow_df_New$Maj_Hispan<-ifelse(zillow_df_New$HispanicPercent>=.50,1,0)
zillow_df_New$A_Lot_Asian<-ifelse(zillow_df_New$AsianPercent>=.35,1,0)
}
### Analysis
{
  mean(zillow_df_New$Num_price[zillow_df_New$Is_Condo==1])
  mean(zillow_df_New$Num_price[zillow_df_New$Is_Townhouse==1])
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1])
  ### DMW State by Townhome
  mean(zillow_df_New$Num_price[zillow_df_New$Is_Townhouse==1 & zillow_df_New$In_VA==1])
  mean(zillow_df_New$Num_price[zillow_df_New$Is_Townhouse==1 & zillow_df_New$In_DC==1])
  mean(zillow_df_New$Num_price[zillow_df_New$Is_Townhouse==1 & zillow_df_New$In_MD==1])
  ### DMW State by House
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1])
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_DC==1])
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_MD==1])
  ### DMW State by Condo
  mean(zillow_df_New$Num_price[zillow_df_New$Is_Condo==1 & zillow_df_New$In_VA==1])
  mean(zillow_df_New$Num_price[zillow_df_New$Is_Condo==1 & zillow_df_New$In_DC==1])
  mean(zillow_df_New$Num_price[zillow_df_New$Is_Condo==1 & zillow_df_New$In_MD==1])
  
  # Size
  ###DMV Size House
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1])
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_House==1 & zillow_df_New$In_DC==1])
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_House==1 & zillow_df_New$In_MD==1])
  ###DMV Size Townhouse
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_Townhouse==1 & zillow_df_New$In_VA==1])
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_Townhouse==1 & zillow_df_New$In_DC==1])
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_Townhouse==1 & zillow_df_New$In_MD==1])
  ###DMV Size Condo
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_Condo==1 & zillow_df_New$In_VA==1 ])
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_Condo==1 & zillow_df_New$In_DC==1])
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_Condo==1 & zillow_df_New$In_MD==1])
  
  
  mean(zillow_df_New$Num_price[zillow_df_New$WhitePercent>=.50 & zillow_df_New$In_VA==1 & zillow_df_New$Is_House==1], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$BlackPercent>=.25 & zillow_df_New$In_VA==1 & zillow_df_New$Is_House==1], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$HispanicPercent>=.25 & zillow_df_New$In_VA==1 & zillow_df_New$Is_House==1], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$AsianPercent >=.25 & zillow_df_New$In_VA==1 & zillow_df_New$Is_House==1], na.rm = TRUE)
  
  
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1 & zillow_df_New$Miles_DT_DC<=10 ], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1 & zillow_df_New$Miles_DT_DC<=15 ], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1 & zillow_df_New$Miles_DT_DC<=30 ], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1 & zillow_df_New$Miles_DT_DC<=45 ], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1 & zillow_df_New$Miles_DT_DC<=60 ], na.rm = TRUE)
  
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1 & zillow_df_New$Miles_DT_DC<=10 ], na.rm = TRUE)
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1 & zillow_df_New$Miles_DT_DC<=15 ], na.rm = TRUE)
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1 & zillow_df_New$Miles_DT_DC<=30 ], na.rm = TRUE)
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1 & zillow_df_New$Miles_DT_DC<=45 ], na.rm = TRUE)
  mean(zillow_df_New$Num_Sqfts[zillow_df_New$Is_House==1 & zillow_df_New$In_VA==1 & zillow_df_New$Miles_DT_DC<=60 ], na.rm = TRUE)
  
  
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_MD==1 & zillow_df_New$Miles_DT_DC<=10 ], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_MD==1 & zillow_df_New$Miles_DT_DC<=15 ], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_MD==1 & zillow_df_New$Miles_DT_DC<=30 ], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_MD==1 & zillow_df_New$Miles_DT_DC<=45 ], na.rm = TRUE)
  mean(zillow_df_New$Num_price[zillow_df_New$Is_House==1 & zillow_df_New$In_MD==1 & zillow_df_New$Miles_DT_DC<=60 ], na.rm = TRUE)
  ### DMV
  FirsRegress<-lm(log(Num_price)
                  ~Num_Sqfts+Num_Bathrooms+Num_Bedrooms
                  +Med_Income+BA_BA_DegreePer+MA_MS_DegreePer
                  +Miles_DT_DC+In_VA+In_MD+
                  +Is_Condo+Is_Townhouse
                  +People_Young_Than_25Percent
                  +WhitePercent
                  +BlackPercent
                  ,zillow_df_New)
  summary(FirsRegress)
  ### DC ###
  zillow_DC<-filter(zillow_df_New, In_DC==1)
  zillow_DC$NW<-ifelse(!grepl("NW",zillow_DC$Address),1,0)
  zillow_DC$NE<-ifelse(!grepl("NE",zillow_DC$Address),1,0)
  zillow_DC$SE<-ifelse(!grepl("SE",zillow_DC$Address),1,0)
  zillow_DC$SW<-ifelse(!grepl("SW",zillow_DC$Address),1,0)
  DC_Regress<-lm(Gross_Rent
              ~Num_Sqfts+Num_Bathrooms+Num_Bedrooms
              +BA_BA_DegreePer+MA_MS_DegreePer
              +Is_Condo+Is_Townhouse
              +Med_Income
              +Miles_DT_DC
              +People_Young_Than_25Percent
              +WhitePercent
              +BlackPercent
              +SW+SE+NE,zillow_DC)
  summary(DC_Regress)
  bptest(DC_Regress)
  ### MD ###
  zillow_MD<-filter(zillow_df_New, In_MD==1)
  MD_Regress<-lm(Num_price
                 ~Num_Sqfts+Num_Bathrooms+Num_Bedrooms
                 +BA_BA_DegreePer+MA_MS_DegreePer
                 +Is_Condo+Is_Townhouse
                 +People_Young_Than_25Percent
                 +Med_Income
                 +Miles_DT_DC
                 +WhitePercent
                 +BlackPercent
                 +AsianPercent
                 ,zillow_MD)
  summary(MD_Regress)
  ### VA ###
  zillow_VA<-filter(zillow_df_New, In_VA==1)
  VA_Regress<-lm(log(Num_price)
                 ~Num_Sqfts+Num_Bathrooms+Num_Bedrooms
                 +BA_BA_DegreePer+MA_MS_DegreePer
                 +Is_Condo+Is_Townhouse
                 +Miles_DT_DC
                 +Med_Income
                 +People_Young_Than_25Percent
                 +WhitePercent
                 +BlackPercent,
                 zillow_VA)
  summary(VA_Regress)
  ### Homes outside 
  zillow_Out_DC<-filter(zillow_df_New,In_MD==1| In_VA==1)
  Out_DC_Regress<-lm(log(Num_price)
                 ~Num_Sqfts+Num_Bathrooms+Num_Bedrooms
                 +BA_BA_DegreePer+MA_MS_DegreePer
                 +Is_Condo+Is_Townhouse
                 +Miles_DT_DC
                 +log(Med_Income)
                 +People_Young_Than_25Percent
                 +WhitePercent
                 +BlackPercent
                 +AsianPercent
                 ,zillow_Out_DC)
  summary(Out_DC_Regress)
  bptest(Out_DC_Regress)
  
  
  
  median(zillow_df_New$Num_Sqfts[zillow_df_New$City=="Alexandria" & zillow_df_New$Is_Townhouse],na.rm = TRUE)
  
  
  names(zillow_df_New)
  
 
  ggplot(data = zillow_df_New, mapping = aes(x = White , y = log(Num_price))) +
    geom_point(alpha = 0.6, color = "darkblue") + # Add a scatter plot layer
    labs(
      title = "",x = "Miles to Downtown DC",y = "Squ") +
    theme_minimal() + # Use a minimal theme for a clean look
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  
  
  
  
} 

DMV_Housing_Data<-zillow_df_New

write_xlsx(DMV_Housing_Data,path = "repo:O-money/DMV_Housing_Data/zilliow_Webscrape.xlsx")




#rm(zillow_df_New,zillow_df,demo_data_dist,demo_data_wide)
#rm(results_list,results_list_Dist,results_listDist)


  
  
