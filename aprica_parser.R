library(rvest)

apri.list.all <- "http://www.apricastyle.com.tw/ProductList.aspx"

apri_goods <- apri.list.all %>%
    read_html() %>% 
    html_nodes(xpath = "//*[@class='product-name']") %>%
    html_text()

apri_price <- apri.list.all %>%
    read_html() %>% 
    html_nodes(xpath = "//*[@class='price ']/b") %>%
    html_text()

library(lubridate)
apri.dt <- data.frame(PARSE_DATE = today(), GOOD_NAME = apri_goods, PRICE = apri_price)

#設定存檔資料夾
set.dir <- "E:/Google 雲端硬碟/PCHOME_parser"
if(file.exists(set.dir) == FALSE){
    set.dir <- getwd() 
} 

library(xlsx)
write.xlsx(apri.dt, paste0(set.dir, "/apricastyle_", gsub(x = today(), "-", ""), ".xlsx"), sheetName = paste(today()))
