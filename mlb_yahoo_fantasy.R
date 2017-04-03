rm(list = ls())

library(readxl)
mlb <- read_excel("E:/R/MyData/MLB_preview.xlsx", "MLB") %>% as.data.frame()


library(tidyverse)
mlb.df <- mlb %>% gather(tmp, player, -TEAM, -VALUE) %>% select(-tmp) %>% .[complete.cases(.), ]


mlb.pre <- mlb.df %>% separate(player, c("POS", "PLAYER"), "-") %>% 
    mutate(PLAYER = trimws(PLAYER), 
           VALUE = factor(VALUE, levels = c("Top5","1st-R","First-3-R","Up-R","Mid-R","Mid-Down-R","Down-R"))) %>% 
    arrange(VALUE)

mlb.pitcher <- mlb.pre %>% filter(grepl("P", POS)) %>% as.data.frame()
mlb.batter <- mlb.pre %>% filter(!grepl("P", POS)) %>% as.data.frame()

mlb.pitcher %>% group_by(VALUE, POS) %>% tally()
mlb.batter %>% group_by(VALUE, POS) %>% tally()


    library(rvest)
    
    ### Batters' data from yahoo mlb fantasy
    batter.base.url <- "https://baseball.fantasysports.yahoo.com/b1/118475/players?status=A&pos=B&cut_type=33&stat1=S_S_2016&myteam=0&sort=OR&sdir=1&count="
    batter.page <- 0:20*25
    
    batter.url <- paste0(batter.base.url, batter.page)

    batter_function <- function(url){

            Batters <-  url %>% read_html() %>% 
                        html_nodes(xpath = "//*[@class='Nowrap name F-link']") %>%
                        html_text() %>% data.frame() %>% `colnames<-`("Batter")

            TEAM.POS <-  url %>% read_html() %>% 
                        html_nodes(xpath = "//*[@class='Fz-xxs']") %>%
                        html_text() %>% data.frame() %>% `colnames<-`("POS") %>% separate(POS, c("TEAM","POS"), "-")
            
            H.AB <-  url %>% read_html() %>% 
                        html_nodes(xpath = "//*[@class='Ta-end F-faded']") %>%
                        html_text() %>% matrix(ncol = 1, byrow = TRUE) %>% .[-1,] %>% data.frame() %>% `colnames<-`(c("H/AB"))
            
            R.RBI.AVG.SLG <-  url %>% read_html() %>% 
                        html_nodes(xpath = "//*[@class='Alt Ta-end']") %>%
                        html_text() %>% matrix(ncol = 4, byrow = TRUE) %>% .[-1,] %>% data.frame() %>% `colnames<-`(c("R", "RBI", "AVG", "SLG"))
            
            HR.SB.OBP <-  url %>% read_html() %>% 
                        html_nodes(xpath = "//*[@class='Ta-end']") %>%
                        html_text() %>% matrix(ncol = 3, byrow = TRUE) %>% .[-1,] %>% data.frame() %>% `colnames<-`(c("HR", "SB", "OBP"))
            
            SLAM <-  url %>% read_html() %>% 
                        html_nodes(xpath = "//*[@class='Last Ta-end']") %>%
                        html_text() %>% .[-1] %>% data.frame() %>% `colnames<-`(c("SLAM"))
            
            STATUS <-  url %>% read_html() %>% 
                        html_nodes(xpath = "//*[@class='ysf-player-status F-injury Fz-xxs Grid-u Lh-xs Mend-xs']") %>%
                        html_text() %>% data.frame() %>% `colnames<-`(c("STATUS"))
            
            # Sys.sleep(runif(1,1,2))
            
        Batter.df <- bind_cols(Batters, TEAM.POS, H.AB,R.RBI.AVG.SLG, HR.SB.OBP, SLAM, STATUS)
    }
    
    Batter.df <- map_df(batter.url, batter_function)

    ### pitchers' data from yahoo mlb fantasy
    pitcher.base.url <- "https://baseball.fantasysports.yahoo.com/b1/118475/players?status=A&pos=P&cut_type=33&stat1=S_S_2016&myteam=0&sort=OR&sdir=1&count="
    pitcher.page <- 0:20*25
    
    pitcher.url <- paste0(pitcher.base.url, pitcher.page)
    
    pitcher_function <- function(url){
        
        pitchers <-  url %>% read_html() %>% 
                    html_nodes(xpath = "//*[@class='Nowrap name F-link']") %>%
                    html_text() %>% data.frame() %>% `colnames<-`("pitcher")
        
        TEAM.POS <-  url %>% read_html() %>% 
                    html_nodes(xpath = "//*[@class='Fz-xxs']") %>%
                    html_text() %>% data.frame() %>% `colnames<-`("POS") %>% separate(POS, c("TEAM","POS"), "-")
        
        IP <-  url %>% read_html() %>% 
                    html_nodes(xpath = "//*[@class='Ta-end F-faded']") %>%
                    html_text() %>% matrix(ncol = 1, byrow = TRUE) %>% .[-1,] %>% data.frame() %>% `colnames<-`(c("IP"))
        
        W.ERA.KBB <-  url %>% read_html() %>% 
                    html_nodes(xpath = "//*[@class='Alt Ta-end']") %>%
                    html_text() %>% matrix(ncol = 3, byrow = TRUE) %>% .[-1,] %>% data.frame() %>% `colnames<-`(c("W", "ERA", "KBB"))
        
        K.WHIP.QS <-  url %>% read_html() %>% 
                    html_nodes(xpath = "//*[@class='Ta-end']") %>%
                    html_text() %>% matrix(ncol = 3, byrow = TRUE) %>% .[-1,] %>% data.frame() %>% `colnames<-`(c("K","WHIP","QS"))
                
        SVH <-  url %>% read_html() %>% 
                    html_nodes(xpath = "//*[@class='Alt Last Ta-end']") %>%
                    html_text() %>% .[-1] %>% data.frame() %>% `colnames<-`(c("SVH"))
        
        STATUS <-  url %>% read_html() %>% 
                    html_nodes(xpath = "//*[@class='ysf-player-status F-injury Fz-xxs Grid-u Lh-xs Mend-xs']") %>%
                    html_text() %>% data.frame() %>% `colnames<-`(c("STATUS"))
        
        # Sys.sleep(runif(1,1,2))
        
        pitcher.df <- bind_cols(pitchers, TEAM.POS, IP, W.ERA.KBB, K.WHIP.QS, SVH, STATUS)
    }
    
    pitcher.df <- map_df(pitcher.url, pitcher_function)

    
    mlb.pitcher.preview <- mlb.pitcher %>% select(-c(TEAM, POS)) %>% left_join(., pitcher.df, by = c("PLAYER"="pitcher"))
    mlb.batter.preview <- mlb.batter %>% select(-c(TEAM, POS)) %>% left_join(., Batter.df, by = c("PLAYER"="Batter"))
    
    write.csv(pitcher.df, "pitcher.csv")
    write.csv(mlb.pitcher, "mlb_pitcher.csv")
    write.csv(pitcher.df, "Batter.csv")
    write.csv(mlb.pitcher, "mlb_Batter.csv")
    
    ##############
    Batter.df.tmp <- read_excel("Batter_fix.xlsx") %>% select(-HAB) %>% 
                    mutate(VALUE = factor(VALUE, levels = c("Top5","1st-R","First-3-R","Up-R","Mid-R","Mid-Down-R","Down-R")),
                           STATUS = coalesce(STATUS, "")) %>% 
                    arrange(VALUE)
    pitcher.df.tmp <- read_excel("pitcher_fix.xlsx") %>% 
                    mutate(VALUE = factor(VALUE, levels = c("Top5","1st-R","First-3-R","Up-R","Mid-R","Mid-Down-R","Down-R")),
                           STATUS = coalesce(STATUS, "")) %>% 
                    arrange(VALUE)
    
    
    library(LICORS)
    library(broom)
    library(plotly)
    
    min_max_fun <- function(x){(x - min(x))/(max(x) - min(x))}
    
    set.seed(123456)
    ### Batter cluster
    Batter.clust.df <- Batter.df.tmp %>% select(-c(VALUE, Batter,TEAM,POS,STATUS,SLAM)) %>% mutate_all(funs(min_max_fun))
    Batter.kclusts <- data.frame(k=1:9) %>% group_by(k) %>% do(kclust=kmeanspp(Batter.clust.df, .$k, iter.max = 500))
    Batter.clusters <- Batter.kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
    Batter.assignments <- Batter.kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], Batter.clust.df))
    Batter.clusterings <- Batter.kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))
    
    ggplot(Batter.clusterings, aes(k, tot.withinss)) + geom_line()
    ggplotly()
    
    Batter.clust.plot <- Batter.assignments %>% ungroup() %>% filter(k==8) %>% select(-k) %>% 
                        group_by(.cluster) %>% summarise_all(funs(mean = mean)) %>% gather(data, val, -.cluster)
    
    ggplot(Batter.clust.plot, aes(x = data, y = val, colour = .cluster, group = .cluster)) + geom_point() + geom_line()
    ggplotly()
    
    Batter.clust <- bind_cols(Batter.df.tmp, Batter.assignments %>% ungroup() %>% filter(k==8) %>% select(.cluster)) %>%
                    mutate(PRIORITY = case_when(.$.cluster == 6 ~ "MUST",
                                                .$.cluster == 1 ~ "MUST",
                                                .$.cluster == 5 ~ "SB",
                                                .$.cluster == 4 ~ "nono",
                                                TRUE ~ "soso"))
    Batter.clust %>% filter(.cluster==6) %>% as.data.frame() %>% head(30)
    
    pos.list <- c("OF","3B","2B","1B","SS","C")
    
    for(i in pos.list){
    tmp <- Batter.clust %>% filter(grepl(i, POS))
    xlsx::write.xlsx(tmp, "Batter_final.xlsx", sheetName=paste0(i), append=TRUE)
    }
    xlsx::write.xlsx(Batter.clust, "Batter_final.xlsx", sheetName="ALL", append=TRUE)
    
    ### Pitcher cluster
    pitcher.clust.df <- pitcher.df.tmp %>% select(-c(VALUE, pitcher,TEAM,POS,STATUS)) %>% mutate_all(funs(min_max_fun))
    pitcher.kclusts <- data.frame(k=1:9) %>% group_by(k) %>% do(kclust=kmeanspp(pitcher.clust.df, .$k, iter.max = 500))
    pitcher.clusters <- pitcher.kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
    pitcher.assignments <- pitcher.kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], pitcher.clust.df))
    pitcher.clusterings <- pitcher.kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))
    
    ggplot(pitcher.clusterings, aes(k, tot.withinss)) + geom_line()
    ggplotly()
    
    pitcher.clust.plot <- pitcher.assignments %>% ungroup() %>% filter(k==7) %>% select(-k) %>% 
        group_by(.cluster) %>% summarise_all(funs(mean = mean)) %>% gather(data, val, -.cluster)
    
    ggplot(pitcher.clust.plot, aes(x = data, y = val, colour = .cluster, group = .cluster)) + geom_point() + geom_line()
    ggplotly()
    
    pitcher.clust <- bind_cols(pitcher.df.tmp, pitcher.assignments %>% ungroup() %>% filter(k==7) %>% select(.cluster))%>%
        mutate(PRIORITY = case_when(.$.cluster == 2 ~ "MUST",
                                    .$.cluster == 6 ~ "PRIOR",
                                    .$.cluster == 7 ~ "RP",
                                    .$.cluster == 4 ~ "RP-2nd",                                    
                                    .$.cluster == 3 ~ "nono",
                                    TRUE ~ "soso"))
    pitcher.clust %>% filter(.cluster==6) %>% as.data.frame() %>% head(30)
    
    pos.list <- c("SP","RP")
    
    for(i in pos.list){
        tmp <- pitcher.clust %>% filter(grepl(i, POS))
        xlsx::write.xlsx(tmp, "pitcher_final.xlsx", sheetName=paste0(i), append=TRUE)
    }
    xlsx::write.xlsx(pitcher.clust, "pitcher_final.xlsx", sheetName="ALL", append=TRUE)
    
    
    xlsx::write.xlsx(pitcher.clust, "pitcher_final.xlsx")
    pitcher.clust %>% filter(.cluster==2) %>% as.data.frame() %>% head(30)
    