rm(list = ls())

#載入/安裝必要套件
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(httr, rvest, magrittr, jsonlite, dplyr, tidyr, lubridate, openxlsx, progress, gmailr)

#設定存檔資料夾
set.dir <- "E:/Google 雲端硬碟/PCHOME_parser"
if(file.exists(set.dir) == FALSE){
    set.dir <- getwd() 
} 

#設定大分類對應網址代碼
cate <- c("3c", "nb", "mobile", "digi", "ce", "cp")
cate.name <- c("3C", "NB", "通訊", "數位", "家電", "周邊")

#抓取中分類資料的網址list
url.head <- "http://ecapi.pchome.com.tw/cdn/ecshop/cateapi/v1.5/region&sign=h24%252F"
url.middle <- "&_callback=cb_ecshopCategoryRegion&"
url.num <- 24724000 + as.integer(runif(1, 2000, 3000)) #規則未知，隨機給一組數字

cate.url.list <- paste0(url.head, cate, url.middle, url.num)

#抓取中分類代碼資料
get.sub.cate <- function(url){
    res_df <- url %>% 
        read_html() %>% html_text() %>% 
        gsub("try\\{cb_ecshopCategoryRegion\\(", "", .) %>% 
        gsub("\\);\\}catch\\(e\\)\\{if\\(window.console\\)\\{console\\.log\\(e\\);\\}\\}", "", .) %>% 
        fromJSON() 
    return(res_df)
}

sub.cate <- lapply(cate.url.list, get.sub.cate) %>% 'names<-'(cate.name)

#大中分類名稱及對應代碼資料 sub.cate.df
sub.cate.df <- data.frame()
for(i in 1:length(cate.name)){
    temp <- sub.cate[[i]] %>% mutate(Category = names(sub.cate[i])) 
    sub.cate.df <- rbind(sub.cate.df, temp)
}

#抓取小分類資料的網址list
sub.cate.url.head <- "http://ecapi.pchome.com.tw/cdn/ecshop/cateapi/v1.5/region/"
cate.url.middle <- "/menu&_callback=jsonp_nemu&"
subcate.url.end <- "?_callback=jsonp_nemu"

cate.url.list <- paste0(sub.cate.url.head, sub.cate.df$Id, cate.url.middle, url.num, subcate.url.end)

#抓取小分類代碼資料
get.sub.sub.cate <- function(url){
    res_df <- url %>% 
        read_html() %>% html_text() %>% 
        gsub("try\\{jsonp_nemu\\(", "", .) %>% 
        gsub("\\);\\}catch\\(e\\)\\{if\\(window.console\\)\\{console\\.log\\(e\\);\\}\\}", "", .) %>% 
        fromJSON() %>% .$Nodes %>% bind_rows()
    return(res_df)
}

sub.sub.cate <- lapply(cate.url.list, get.sub.sub.cate) %>% 'names<-'(sub.cate.df$Id)

#中小分類代碼及小分類名稱資料 sub.sub.cate.df
sub.sub.cate.df <- data.frame()
for(i in 1:length(sub.sub.cate)){
    temp <- sub.sub.cate[[i]] %>% mutate(Sub.Id = Id, Id = names(sub.sub.cate[i])) 
    sub.sub.cate.df <- rbind(sub.sub.cate.df, temp)
}

#大中小分類名稱及對應代碼資料 full.cate.df
full.cate.df <- sub.cate.df %>% left_join(., sub.sub.cate.df, by = "Id") %>% distinct()

#抓取小分類代碼對應商品資料的網址list
small.cate.url.head <- "http://ecapi.pchome.com.tw/ecshop/prodapi/v2/store/"
small.cate.url.end <- "/prod&offset=8&limit=36&fields=Id,Nick,Pic,Price,Discount,isSpec,Name,isCarrier,isSnapUp,isBigCart&_callback=jsonp_prodgrid?_callback=jsonp_prodgrid"

small.cate.url.list <- paste0(small.cate.url.head, full.cate.df$Sub.Id, small.cate.url.end)

full.cate.df <- full.cate.df %>% 
                mutate(small.cate.url.list = paste0(small.cate.url.head, full.cate.df$Sub.Id, small.cate.url.end))

#抓取商品資料
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

pchome.temp.df <- data.frame()
ptm.start <- proc.time()

#在console中以progress bar呈現進度
pb <- progress_bar$new(
      format = "Sub-categories :N [:bar] :per , elapsed time :ela ",
      clear = FALSE, total = length(full.cate.df$small.cate.url.list), width = 120)

for(n in 1:length(full.cate.df$small.cate.url.list)){
    tryCatch(
        {
        small.cat.df <- parse.small.cate((full.cate.df$small.cate.url.list[n]))
           
        Sys.sleep(runif(1, 1, 2))
        
        ptm.end <- proc.time()
        ptm.loop <- ptm.end - ptm.start
        
        # cat(n, "/", length(small.cate.url.list), "sub-categories,", 
        #     "progress:", sprintf("%1.3f%%", 100 * n/length(small.cate.url.list)),
        #     ", elapsed time:", as.character(seconds_to_period(trunc(ptm.loop["elapsed"]))), "\n")
        
        #progress bar 設定
        pb$tick(tokens = list(N = paste0(n, "/", length(full.cate.df$small.cate.url.list)),
                              per = sprintf("%1.3f%%", 100 * n/length(small.cate.url.list)),
                              ela = paste0(as.character(seconds_to_period(trunc(ptm.loop["elapsed"]))))))
        
            
            info.df <- full.cate.df[n, ] %>% as_data_frame() %>% 
                       select(Id, Sub.Id, Category, Name.x, Name.y) %>%
                       .[rep(seq_len(nrow(.)), each = nrow(small.cat.df)), ]
            
            pchome.temp.df <- rbind(pchome.temp.df, cbind(info.df, small.cat.df))
            
                if(n %% 500 == 0){
                    pchome.df <- pchome.temp.df %>% 'names<-'(df.colnames)
                    pchome.df <- pchome.df %>% mutate(Parse.Date = as_date(today())) %>%
                                select(Parse.Date, everything())
                    write.csv(pchome.df, file = paste0(set.dir, "/pchome_temp.csv"))
                    cat("Save", n, "small-categories records in temp csv file \n")
                }
            pchome.temp.df
            
        }, error = function(e){cat("ERROE:", conditionMessage(e), "\n")
        }, warning = function(w){cat("WARNING:", warning(w), "\n")}
        )
}

colnames(pchome.temp.df) <- df.colnames

pchome.df <- pchome.temp.df %>% distinct() %>% 
             mutate(Parse.Date = as_date(today())) %>% select(Parse.Date, everything())

saveRDS(pchome.df, paste0(set.dir, "/pchome_", gsub(x = today(), "-", ""), ".rds"))
write.csv(pchome.df, file = paste0(set.dir, "/pchome_", gsub(x = today(), "-", ""), ".csv"))

category.df <- full.cate.df %>% select(Category, m.Category = Name.x, sl.Category = Name.y , Id, Sub.Id) %>% distinct()
write.csv(category.df, file = paste0(set.dir, "/pchome_cate_list_", gsub(x = today(), "-", ""), ".csv"))

cat("Mission complete,", nrow(pchome.temp.df), paste0("records save in \"" ,set.dir, "/pchome_", gsub(x = today(), "-", ""), ".csv\""))

#完成後寄Gmail通知
complete.mail <- mime() %>% 
                 from("kenny1224@gmail.com") %>% 
                 subject(paste0("PCHOME(", today(), ") web scraping is complete")) %>%  
                 text_body(paste0("Download: ", paste(nrow(pchome.temp.df), "records"), "\n",
                                  "Elapsed time: ", as.character(seconds_to_period(trunc(ptm.loop["elapsed"]))), "\n", 
                                  "Download files from : https://drive.google.com/open?id=0BzYj5oybW9P1LXJrMmVSaVRtd0U"))

send_message(complete.mail %>% to("kenny1224@gmail.com"))
send_message(complete.mail %>% to("lara720608@gmail.com"))
