rm(list = ls())

#載入/安裝必要套件
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(httr, rvest, magrittr, jsonlite, dplyr, tidyr, lubridate, openxlsx)

#設定分類對應網址參數
cate <- c("3c", "nb", "mobile", "digi", "ce", "cp")
cate.name <- c("3C", "NB", "通訊", "數位", "家電", "周邊")
# cate <- c("3c", "nb", "mobile", "digi", "ce") #cp(周邊)爬蟲過程有錯，先跳過
# cate.name <- c("3C", "NB", "通訊", "數位", "家電") #cp(周邊)爬蟲過程有錯，先跳過
# cate <- c("3c") #cp(周邊)爬蟲過程有錯，先跳過
# cate.name <- c("3C") #cp(周邊)爬蟲過程有錯，先跳過

#組合網址list
url.head <- "http://ecapi.pchome.com.tw/cdn/ecshop/cateapi/v1.5/region&sign=h24%252F"
url.middle <- "&_callback=cb_ecshopCategoryRegion&"
url.num <- 24724000 + as.integer(runif(1, 2000, 3000))

cate.url.list <- paste0(url.head, cate, url.middle, url.num)

#抓取子分類參數代碼
get.sub.cate <- function(url){
    res_df <- url %>% 
        read_html() %>% html_text() %>% 
        gsub("try\\{cb_ecshopCategoryRegion\\(", "", .) %>% 
        gsub("\\);\\}catch\\(e\\)\\{if\\(window.console\\)\\{console\\.log\\(e\\);\\}\\}", "", .) %>% 
        fromJSON() 
    return(res_df)
}
sub.cate <- lapply(cate.url.list, get.sub.cate) %>% 'names<-'(cate.name)

sub.cate.df <- data.frame()
for(i in 1:length(cate.name)){
    temp <- sub.cate[[i]] %>% mutate(Category = names(sub.cate[i])) 
    sub.cate.df <- rbind(sub.cate.df, temp)
}

sub.cate.url.head <- "http://ecapi.pchome.com.tw/cdn/ecshop/cateapi/v1.5/region/"
cate.url.middle <- "/menu&_callback=jsonp_nemu&"
subcate.url.end <- "?_callback=jsonp_nemu"

cate.url.list <- paste0(sub.cate.url.head, sub.cate.df$Id, cate.url.middle, url.num, subcate.url.end)

get.sub.sub.cate <- function(url){
    res_df <- url %>% 
        read_html() %>% html_text() %>% 
        gsub("try\\{jsonp_nemu\\(", "", .) %>% 
        gsub("\\);\\}catch\\(e\\)\\{if\\(window.console\\)\\{console\\.log\\(e\\);\\}\\}", "", .) %>% 
        fromJSON() %>% .$Nodes %>% bind_rows()
    return(res_df)
}

sub.sub.cate <- lapply(cate.url.list, get.sub.sub.cate) %>% 'names<-'(sub.cate.df$Id)

sub.sub.cate.df <- data.frame()
for(i in 1:length(sub.sub.cate)){
    temp <- sub.sub.cate[[i]] %>% mutate(Sub.Id = Id, Id = names(sub.sub.cate[i])) 
    sub.sub.cate.df <- rbind(sub.sub.cate.df, temp)
}

full.cate.df <- sub.cate.df %>% left_join(., sub.sub.cate.df, by = "Id")

small.cate.url.head <- "http://ecapi.pchome.com.tw/ecshop/prodapi/v2/store/"
small.cate.url.end <- "/prod&offset=8&limit=36&fields=Id,Nick,Pic,Price,Discount,isSpec,Name,isCarrier,isSnapUp,isBigCart&_callback=jsonp_prodgrid?_callback=jsonp_prodgrid"

small.cate.url.list <- paste0(small.cate.url.head, full.cate.df$Sub.Id, small.cate.url.end)

full.cate.df <- full.cate.df %>% 
    mutate(small.cate.url.list = paste0(small.cate.url.head, full.cate.df$Sub.Id, small.cate.url.end))

parse.small.cate <- function(url){
    res_df.temp <- url %>% 
        read_html() %>% html_text() %>% 
        gsub("try\\{jsonp_prodgrid\\(", "", .) %>% 
        gsub("\\);\\}catch\\(e\\)\\{if\\(window.console\\)\\{console\\.log\\(e\\);\\}\\}", "", .) %>% 
        fromJSON()
    
    if(length(res_df.temp) == 0){
        res_df <- data.frame()
    }else{
        res_df <- res_df.temp %>% select(-c(Pic, Price)) %>% 
            mutate(Price_M = res_df.temp$Price$M, Price_P = res_df.temp$Price$P)
    }
    return(res_df)
}

df.colnames <- c("Web.Id","Web.Sub.Id","Category","Name.1","Name.2",
                 "Prod.Id","Nick","Discount" ,"isSpec","Name",
                 "isCarrier","isSnapUp","isBigCart","Price_M","Price_P")

pchome.cate.df <- data.frame()
ptm.start <- proc.time()
for(n in 1:length(full.cate.df$small.cate.url.list)){
    tryCatch(
        {small.cat.df <- parse.small.cate((full.cate.df$small.cate.url.list[n]))
        },error = function(e){cat("ERROE:", conditionMessage(e), "\n")
        },warning = function(w){cat("WARNING:", warning(w), "\n")}
    )
    Sys.sleep(runif(1, 3, 5))
    ptm.end <- proc.time()
    ptm.loop <- ptm.end - ptm.start
    cat(n, "/", length(small.cate.url.list), "sub-categories,", 
        "progress:", sprintf("%1.2f%%", 100 * n/length(small.cate.url.list)),
        ", spend", ptm.loop["elapsed"], "seconds", "\n")
    
    info.df <- full.cate.df[n, ] %>% as_data_frame() %>% 
        select(Id, Sub.Id, Category, Name.x, Name.y) %>%
        .[rep(seq_len(nrow(.)), each = nrow(small.cat.df)),]
    pchome.cate.df <- rbind(pchome.cate.df, cbind(info.df, small.cat.df))
    if(n %% 200 == 0){
        pchome.df <- pchome.cate.df %>% 'names<-'(df.colnames)
        pchome.df <- pchome.df %>% mutate(Parse.Date = as_date(today())) %>%
            select(Parse.Date, everything())
        write.xlsx(pchome.df, file = "pchome_temp.xlsx", sheetName = paste(today()))
    }
    pchome.cate.df
}

colnames(pchome.cate.df) <- df.colnames

pchome.df <- pchome.cate.df %>% mutate(Parse.Date = as_date(today())) %>%
    select(Parse.Date, everything())

saveRDS(pchome.df, "pchome_df.rds")
write.xlsx(pchome.df, file = paste0("pchome_", today(), ".xlsx"), sheetName = paste(today()))