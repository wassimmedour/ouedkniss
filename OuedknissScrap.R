library(rvest)
library(tidyverse)
title<-function(x){ x%>% read_html(x) %>% html_nodes('h2') %>% html_text()}

annonce_price<-function(x){
   y<-x %>% read_html(x) %>% html_nodes('.button_details') %>% html_attr("href")
   z<-sapply(y, function(y){paste0("https://www.ouedkniss.com/",y)}) 
   sapply(z, function(z){z %>% read_html() %>% html_nodes('[itemprop="price"]') %>%html_text()})
   
  
  }
 
annonce_title<-function(x){
  y<-x %>% read_html(x) %>% html_nodes('.button_details') %>% html_attr("href")
  z<-sapply(y, function(y){paste0("https://www.ouedkniss.com/",y)}) 
  sapply(z, function(z){z %>% read_html() %>% html_nodes('#Title') %>%html_text()})
         
         }

annonce_details_label<-function(x){
  y<-x %>% read_html(x) %>% html_nodes('.button_details') %>% html_attr("href")
  z<-sapply(y, function(y){paste0("https://www.ouedkniss.com/",y)}) 
  sapply(z, function(z){z %>% read_html() %>% html_nodes('.description_label ') %>%html_text() %>% unlist()})
}

a<-sapply(a,function(x){str_replace(x,"Ã©","é")})

annonce_details<-function(x){
  y<-x %>% read_html(x) %>% html_nodes('.button_details') %>% html_attr("href")
  z<-sapply(y, function(y){paste0("https://www.ouedkniss.com/",y)}) 
  sapply(z, function(z){z %>% read_html() %>% html_nodes('.description_span') %>%html_text() %>% unlist()})
}

data %>% 
  group_by_at(vars(-b)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=a, value=b) %>%    # spread
  select(-row_id) %>%  # drop the index
  View()


a<-sapply(a,function(x){str_replace(x,"Ã©","é")})
details<-function(x){annonce_details(x) 
                    annonce_details_label(x)
                    }
