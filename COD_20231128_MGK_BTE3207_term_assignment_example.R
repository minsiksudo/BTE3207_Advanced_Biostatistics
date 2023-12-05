
# Description -------------------------------------------------------------

#This file shows how to to the final term task for BTE3207 at Inha University.



#Loading libraries 

# Roading files -----------------------------------------------------------


install.packages(c("tidyverse", "ggplot2"))


library(tidyverse)


# Loading your file -------------------------------------------------------

#If you are using Korean version R,
        read.csv("dataset/restaurants_by_size_2017_2019.csv", header = T, fileEncoding = "euc-kr")

#If you are using ENG version R,
food_data <- read.csv("dataset/restaurants_by_size_2017_2019.csv", header = T, fileEncoding = "euc-kr")
        #This is my setting I am going to choose this funciton.
#If you are loading .xlsx,
install.packages("readxl")
library(readxl)
        read_excel("dataset/restaurants_by_size_2017_2019.xlsx")

#If you are using ENG version R,
        read.csv("dataset/restaurants_by_size_2017_2019.csv", header = T, fileEncoding = "euc-kr")


# Exploring data ----------------------------------------------------------

        head(food_data)

        

# Data manipulation -------------------------------------------------------
# This process is not necessary, if your data is tidy
        
        food_data$시도별 %>% unique
        food_data <- food_data %>% filter(`시도별` != "전국")
        # we have higer level categories.
        
        food_data$항목 %>% unique
        food_data <- food_data %>% filter(`항목` != "사업체수")
        # we have higer level categories.
        
        
        
        
        food_data$산업별 %>% unique
        # we have multiple categories.
        
        #Subsetting categories
        chinesefood_data <- food_data %>% filter(`산업별` == "중식 음식점업")
        

        #Double-checking numbers
        chinesefood_data$데이터
        # I see some "-" and "0" values.
        chinesefood_data <- chinesefood_data %>% filter(`데이터` != "0")
        # Changing data type into numerical variable
        chinesefood_data$데이터 <- as.numeric(chinesefood_data$데이터)

        chinesefood_data%>%names
        chinesefood_data <- chinesefood_data %>% 
                mutate(region = case_when(`시도별` == "서울특별시" ~ "Seoul",
                                          `시도별` == "경기도" ~ "Gyeonggi",
                                          `시도별` == "경상남도" ~ "GS-S",
                                          `시도별` == "경상북도" ~ "GS-N",
                                          `시도별` == "광주광역시" ~ "GJ",
                                          `시도별` == "대구광역시" ~ "DG",
                                          `시도별` == "대전광역시" ~ "DJ",
                                          `시도별` == "부산광역시" ~ "BS",
                                          `시도별` == "세종특별자치시" ~ "SJ",
                                          `시도별` == "울산광역시" ~ "US",
                                          `시도별` == "인천광역시" ~ "IC",
                                          `시도별` == "전라남도" ~ "JL-S",
                                          `시도별` == "전라북도" ~ "JL-N",
                                          `시도별` == "제주특별자치도" ~ "JJ",
                                          `시도별` == "충청남도" ~ "CC-S",
                                          `시도별` == "충청북도" ~ "SS-N",
                                          `시도별` == "강원도" ~ "GW"
                                          
                )) %>%
                rename(year = "시점",
                       restaurants = "데이터",
                       category = "항목") %>%
                select(-c("시도별", "산업별"))

# Adding data -------------------------------------------------------------
# This process is not necessary, if your data is having all the variables that you need
        
        population_data <- read.csv("dataset/korea_population_2017_2019.csv", header = T, fileEncoding = "euc-kr") %>%
                filter(`항목` == "총인구 (명)") %>%
                mutate(region = case_when(`행정구역별.읍면동.` == "서울특별시" ~ "Seoul",
                                          `행정구역별.읍면동.` == "경기도" ~ "Gyeonggi",
                                          `행정구역별.읍면동.` == "경상남도" ~ "GS-S",
                                          `행정구역별.읍면동.` == "경상북도" ~ "GS-N",
                                          `행정구역별.읍면동.` == "광주광역시" ~ "GJ",
                                          `행정구역별.읍면동.` == "대구광역시" ~ "DG",
                                          `행정구역별.읍면동.` == "대전광역시" ~ "DJ",
                                          `행정구역별.읍면동.` == "부산광역시" ~ "BS",
                                          `행정구역별.읍면동.` == "세종특별자치시" ~ "SJ",
                                          `행정구역별.읍면동.` == "울산광역시" ~ "US",
                                          `행정구역별.읍면동.` == "인천광역시" ~ "IC",
                                          `행정구역별.읍면동.` == "전라남도" ~ "JL-S",
                                          `행정구역별.읍면동.` == "전라북도" ~ "JL-N",
                                          `행정구역별.읍면동.` == "제주특별자치도" ~ "JJ",
                                          `행정구역별.읍면동.` == "충청남도" ~ "CC-S",
                                          `행정구역별.읍면동.` == "충청북도" ~ "SS-N",
                                          `행정구역별.읍면동.` == "강원도" ~ "GW"
                                          
                )) %>%
                rename(`popluation` = "데이터",
                       `year` = "시점") %>%
                        select(-c("항목", "행정구역별.읍면동."))
                        
        

        
        chinesefood_data_pop <- inner_join(population_data,
                                           chinesefood_data, 
                                           by=c("region" = "region", 
                                                "year" = "year"))
        
        chinesefood_data_pop$over100 <- 
                ifelse(chinesefood_data_pop$category == "100석 이상", 1, 0)
        
        chinesefood_data_pop$cr_per_mil <- chinesefood_data_pop$restaurants / chinesefood_data_pop$popluation * 1000000
                
        chinesefood_data_pop$region <- factor(chinesefood_data_pop$region,
                                              levels = c("IC", "Seoul", "BS", "DG", "GJ", "DJ", "US",
                                                         "SJ", "Gyeonggi", "GW", "SS-N",  "CC-S",  "JL-N", 
                                                         "JL-S", "GS-N", "GS-S",  "JJ"))
        
        
# Research questions ------------------------------------------------------

        
# Can I open a Chinese restaurant in Inchoen? isn't it a red-ocean? Where should I open a new Chinese restaurant?

        
        
# I tested 4 statistica questions to get my statistical inference. However, you only 1 test is necessary for the assignment.
        
        
        
        #Statistical question 1.
        
        # Is incheon having different number of Chinese restaurants, relative to other cities, after adjusting the effect of size of restaurant?
        # H0: Incheon has the same number of Chinese restaurants relative to other cities after adjusting the effect of size of restaurant.
        # Ha: Incheon has the larger number of Chinese restaurants relative to other cities after adjusting the effect of size of restaurant.
        
        lm(data = chinesefood_data_pop, restaurants ~ (region=="IC") + category) %>% 
                anova

        
#No. Inchoen is not having statistically higher number of restaurants in Korea, relative to other cities..

        #Statistical question 2.
        
        # Is incheon having different number of Chinese restaurants per million population, relative to other cities, after adjusting the effect of size of restaurant?
        # H0: Incheon has the same number of Chinese restaurants per million population relative to other cities after adjusting the effect of size of restaurant.
        # Ha: Incheon has the larger number of Chinese restaurants per million population relative to other cities after adjusting the effect of size of restaurant.
        lm(data = chinesefood_data_pop, cr_per_mil ~ (region=="IC") + category) %>% 
                anova
        
        lm(data = chinesefood_data_pop, cr_per_mil ~ (region=="IC") + category) %>% 
                summary

# Though it is not significant, Incheon`s Chinese restaurants are not red ocean. 
        
        #Statistical question 3.
        
        # is any city having different number of Chinese restaurants, relative to Incheon, after adjusting the size of restaurant?
        # H0: There is no city that has different number of Chinese restaurants relative to Incheon, after adjusting the size of restaurant.
        # Ha: There is a city that has different number of Chinese restaurants relative to Incheon, after adjusting the size of restaurant.
        
        lm(data = chinesefood_data_pop, restaurants ~ region + category) %>% 
                anova
        
        lm(data = chinesefood_data_pop, restaurants ~ region + category) %>% 
                summary
        
# Incheon's number of Chinese restaurants are much higher than Seoul and Sejong. 
        
        #Statistical question 4.
        
        # is any city having different number of Chinese restaurants per million, relative to Incheon, after adjusting the size of restaurant?
        # H0: There is no city that has different number of Chinese restaurants per million relative to Incheon, after adjusting the size of restaurant.
        # Ha: There is a city that has different number of Chinese restaurants per million relative to Incheon, after adjusting the size of restaurant.
        
        lm(data = chinesefood_data_pop, cr_per_mil ~ region + category) %>% 
                anova
        
        lm(data = chinesefood_data_pop, cr_per_mil ~ region + category) %>% 
                summary
        
        
        
# Data visualization 
        chinesefood_data_pop %>%
        boxplot(data = ., cr_per_mil ~ region)

        

# Conclusions -------------------------------------------------------------

        
#Statistical conclusion        
        # Per million capita, Gwangju, followed by Inchoen, had the lowest number of Chinese restaurants.
        
# Inference
        # Incheon's Chinese restaurant market is not red-ocean, after adjusting for size of restaurants.. Meanwhile, it would be better to choose Gwangju.
        
        

                