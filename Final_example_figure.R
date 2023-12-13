data <-read.csv("final_example.csv")


library(lme4)
library(lmerTest)
library(tidyverse)
library(ggplot2)


data2 <- data.frame(SBP = c(120, 121, 122, 123, 124, 125, 126),
                   DBP = c(80, 80, 85, 83, 87, 88, 88),
                   Subject = factor(c(1, 2, 1, 2, 2, 1, 2)))


lmer(data = data, SBP_change ~ DBP + (1|Subject)) %>% summary
lmer(data = data2, SBP ~ DBP + (1|subject)) %>% summary

view(data2)


data2 %>% 
        ggplot(., aes(x = DBP, y = SBP, shape = Subject, linetype = Subject)) +
        geom_point(size = 5) +
        geom_smooth(method = "lm", se = F, fullrange = TRUE) +
        theme_bw(base_size = 15) +
        scale_shape(solid = FALSE) +
        xlab("DBP (mmHg)") +
        ylab("SBP (mmHg)")
