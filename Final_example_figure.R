data <-read.csv("final_example.csv")


library(lme4)
library(lmerTest)
library(tidyverse)
library(ggplot2)


data2 <- data.frame(SBP = c(120, 121, 122, 123, 124, 125, 126),
                   DBP = c(80, 80, 85, 83, 87, 88, 88),
                   Subject = factor(c(1, 2, 1, 2, 2, 1, 2)))



data_ls <- data.frame(landslides = c(1,1,1,1,1,1,1,1, 0,0,0,0,0,0,0,0),
                      rainfall = c(20,30,20,23,42,23,34,17, 12,24,29,24,20,10,8,8),
                      rainfallP = c(12,20,30,23,22,43,34,18, 30,28,12,20,20,8,43,32))

lm(data = data_ls, landslides ~ rainfall + rainfallP)%>% anova

glm(data = data_ls, as.factor(landslides) ~ rainfall, family = "binomial")

glm(data = data_ls, as.factor(landslides) ~ rainfall + rainfallP, family = "binomial")




lmer(data = data, SBP_change ~ DBP + (1|Subject)) %>% summary
lmer(data = data2, SBP ~ DBP + (1|subject)) %>% summary
aov(data = data2, SBP ~ DBP) %>% summary
view(data2)


data2 %>% 
        ggplot(., aes(x = DBP, y = SBP, shape = Subject, linetype = Subject)) +
        geom_point(size = 5) +
        geom_smooth(method = "lm", se = F, fullrange = TRUE) +
        theme_bw(base_size = 15) +
        scale_shape(solid = FALSE) +
        xlab("DBP (mmHg)") +
        ylab("SBP (mmHg)")
