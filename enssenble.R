
# enssenble

setwd("/Users/Sooyoung/kaggle/CreditRisk/data")

df1 = read.csv("tidy_xgb_test0.76516.csv")
df2 = read.csv("tidy_xgb_4_0.75453.csv")

library(dplyr)

df1 %>% left_join(df2, by="SK_ID_CURR") %>% mutate(TARGET = TARGET.x*0.9 + TARGET.y*0.1) %>% 
    select(SK_ID_CURR,TARGET) %>% write.csv("result9010.csv",row.names=F)
