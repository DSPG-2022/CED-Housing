library(readxl)
library(tidyverse)


df <- read_csv("education_median_income.csv")
colnames(df) <- c("Education.Level","MN_E","MN_M", "MO_E", "MO_M")
df$MN_M <- c(706, 311, 185, 454, 561)
df$MO_M <- c(484, 190, 260, 300, 672)

ggplot(data=df, aes(x=reorder(Education.Level, MN_E), y=MN_E))+ geom_col(fill="blue")+
  theme(legend.position = 'none')+
  ylab("Median Income")+
  xlab("Education Level")+
  labs(title = "Education Level distribution in Minnesota")+
  geom_text(aes(label = MN_E),hjust =-1.25) + coord_flip() +
  geom_errorbar(aes(ymin=MN_E-MN_M, ymax=MN_E+MN_M), width=.2,
           position=position_dodge(.9)) 

ggplot(data=df, aes(x=reorder(Education.Level, MO_E), y=MO_E))+ geom_col(fill="red")+
  theme(legend.position = 'none')+
  ylab("Median Income")+
  xlab("Education Level")+
  labs(title = "Education Level distribution in Missouri")+
  geom_text(aes(label = MO_E),hjust =-1.25) + coord_flip() +
  geom_errorbar(aes(ymin=MO_E-MO_M, ymax=MO_E+MO_M), width=.2,
                position=position_dodge(.9)) 
