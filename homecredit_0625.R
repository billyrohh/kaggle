
library(dplyr)
library(data.table)
library(magrittr)
library(caret)
library(tidyverse)
library(lightgbm)
library(reshape2)

setwd("/Users/sooyoung/Desktop/kaggle/homecredit")

#Loda data
tr = read_csv("application_train.csv") 
te = read_csv("application_test.csv")
bureau = read_csv("bureau.csv")
bureau_blc = read_csv("bureau_balance.csv")
paym = read_csv('installments_payments.csv')
prev = read_csv('previous_application.csv')
pos = read_csv('POS_CASH_balance.csv')
crd = read_csv('credit_card_balance.csv')

# prev
prev_cons_lns = prev %>% filter(NAME_CONTRACT_TYPE == "Consumer loans") %>% select(SK_ID_CURR,AMT_ANNUITY,
                                                                                   AMT_APPLICATION,
                                                                                   AMT_CREDIT,
                                                                                   AMT_DOWN_PAYMENT,
                                                                                   AMT_GOODS_PRICE,
                                                                                   WEEKDAY_APPR_PROCESS_START,
                                                                                   HOUR_APPR_PROCESS_START,
                                                                                   FLAG_LAST_APPL_PER_CONTRACT,
                                                                                   NFLAG_LAST_APPL_IN_DAY,
                                                                                   RATE_DOWN_PAYMENT,
                                                                                   RATE_INTEREST_PRIMARY,
                                                                                   RATE_INTEREST_PRIVILEGED,
                                                                                   DAYS_DECISION)

prev_cash_lns = prev %>% filter(NAME_CONTRACT_TYPE == "Cash loans") %>% select(SK_ID_CURR,AMT_ANNUITY,
                                                                               AMT_APPLICATION,
                                                                               AMT_CREDIT,
                                                                               AMT_DOWN_PAYMENT,
                                                                               AMT_GOODS_PRICE,
                                                                               WEEKDAY_APPR_PROCESS_START,
                                                                               HOUR_APPR_PROCESS_START,
                                                                               FLAG_LAST_APPL_PER_CONTRACT,
                                                                               NFLAG_LAST_APPL_IN_DAY,
                                                                               RATE_DOWN_PAYMENT,
                                                                               RATE_INTEREST_PRIMARY,
                                                                               RATE_INTEREST_PRIVILEGED,
                                                                               DAYS_DECISION)


prev_res_lns = prev %>% filter(NAME_CONTRACT_TYPE == "Revolving loans") %>% select(SK_ID_CURR,AMT_ANNUITY,
                                                                                   AMT_APPLICATION,
                                                                                   AMT_CREDIT,
                                                                                   AMT_DOWN_PAYMENT,
                                                                                   AMT_GOODS_PRICE,
                                                                                   WEEKDAY_APPR_PROCESS_START,
                                                                                   HOUR_APPR_PROCESS_START,
                                                                                   FLAG_LAST_APPL_PER_CONTRACT,
                                                                                   NFLAG_LAST_APPL_IN_DAY,
                                                                                   RATE_DOWN_PAYMENT,
                                                                                   RATE_INTEREST_PRIMARY,
                                                                                   RATE_INTEREST_PRIVILEGED,
                                                                                   DAYS_DECISION)

prev_xna_lns = prev %>% filter(NAME_CONTRACT_TYPE == "XNA") %>% select(SK_ID_CURR,AMT_ANNUITY,
                                                                       AMT_APPLICATION,
                                                                       AMT_CREDIT,
                                                                       AMT_DOWN_PAYMENT,
                                                                       AMT_GOODS_PRICE,
                                                                       WEEKDAY_APPR_PROCESS_START,
                                                                       HOUR_APPR_PROCESS_START,
                                                                       FLAG_LAST_APPL_PER_CONTRACT,
                                                                       NFLAG_LAST_APPL_IN_DAY,
                                                                       RATE_DOWN_PAYMENT,
                                                                       RATE_INTEREST_PRIMARY,
                                                                       RATE_INTEREST_PRIVILEGED,
                                                                       DAYS_DECISION)


prev_cons_lns  %<>% mutate_if(is.character, funs(factor(.) %>% as.integer())) 
prev_cash_lns  %<>% mutate_if(is.character, funs(factor(.) %>% as.integer())) 
prev_res_lns  %<>% mutate_if(is.character, funs(factor(.) %>% as.integer())) 
prev_xna_lns  %<>% mutate_if(is.character, funs(factor(.) %>% as.integer())) 

prev_cons_lns_avg = prev_cons_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_cons_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_cash_lns_avg = prev_cash_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_cash_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_res_lns_avg = prev_res_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_res_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_xna_lns_avg = prev_xna_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_xna_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_cons_lns_mx = prev_cons_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_cons_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_cash_lns_mx = prev_cash_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_cash_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_res_lns_mx = prev_res_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_res_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_xna_lns_mx = prev_xna_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_xna_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_cons_lns_mn = prev_cons_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_cons_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_cash_lns_mn = prev_cash_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_cash_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_res_lns_mn = prev_res_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_res_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

prev_xna_lns_mn = prev_xna_lns %>%   group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev_xna_lns %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

rm(prev_cons_lns,prev_cash_lns,prev_res_lns,prev_xna_lns)
gc()
prev %<>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

avg_prev = prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

mx_prev =  prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) 

mn_prev =  prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) 

prev_nm_status1_avg = prev %>% filter(NAME_CONTRACT_STATUS == 1) %>%  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) 

prev_nm_status2_avg = prev %>% filter(NAME_CONTRACT_STATUS == 2) %>%  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) 

prev_nm_status3_avg = prev %>% filter(NAME_CONTRACT_STATUS == 3) %>%  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) 

prev_nm_status4_avg = prev %>% filter(NAME_CONTRACT_STATUS == 4) %>%  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) 

rm(prev)
gc()


bureau = bureau  %<>% mutate_if(is.character, funs(factor(.) %>% as.integer())) 

bureau_type_avg = bureau %>% group_by(SK_ID_CURR,CREDIT_TYPE) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) 

bureau_type_mx = bureau %>% group_by(SK_ID_CURR,CREDIT_TYPE) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) 

bureau_type_mn = bureau %>% group_by(SK_ID_CURR,CREDIT_TYPE) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) 

avg_bureau = bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(buro_count = bureau %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

mx_bureau = bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) 

mn_bureau = bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) 

bureau_actavg = bureau %>% group_by(SK_ID_CURR,CREDIT_ACTIVE) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) 

bureau_actmx = bureau %>% group_by(SK_ID_CURR,CREDIT_ACTIVE) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) 

bureau_actmn = bureau %>% group_by(SK_ID_CURR,CREDIT_ACTIVE) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) 

rm(bureau)
gc()


bureau_blc %<>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

avg_bureau_blc = bureau_blc %>% 
  group_by(SK_ID_BUREAU) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(buro_count = bureau_blc %>%  
           group_by(SK_ID_BUREAU) %>% 
           count() %$% n)

mx_bureau_blc = bureau_blc %>% 
  group_by(SK_ID_BUREAU) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) 

mn_bureau_blc = bureau_blc %>% 
  group_by(SK_ID_BUREAU) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) 

rm(bureau_blc)
gc()

paym %<>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

avg_paym = paym %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(buro_count = paym %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

mx_paym = paym %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) 

mn_paym = paym %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) 

rm(paym)
gc()

crd %<>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

crd_type_avg = crd %>% group_by(SK_ID_CURR,NAME_CONTRACT_STATUS) %>%
  summarise_all(funs(mean(., na.rm = TRUE))) 

crd_type_mx = crd %>% group_by(SK_ID_CURR,NAME_CONTRACT_STATUS) %>%
  summarise_all(funs(max(., na.rm = TRUE))) 

crd_type_mn = crd %>% group_by(SK_ID_CURR,NAME_CONTRACT_STATUS) %>%
  summarise_all(funs(min(., na.rm = TRUE))) 




avg_crd = crd %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(buro_count = crd %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

mx_crd = crd %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) 

mn_crd = crd %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) 

rm(crd)
gc()

pos %<>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

avg_pos = pos %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(buro_count = pos %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

mx_pos = pos %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) 


mn_pos = pos %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) 

rm(pos)
gc()


avg_prev_int = avg_prev %>% left_join(avg_pos,by="SK_ID_CURR") 
avg_prev_int = avg_prev_int %>% left_join(avg_paym,by="SK_ID_CURR") 
avg_prev_int = avg_prev_int %>% left_join(avg_crd,by="SK_ID_CURR")

rm(avg_prev,avg_pos,avg_paym,avg_crd)
gc()

mx_prev_int = mx_prev %>% left_join(mx_pos,by="SK_ID_CURR") 
mx_prev_int = mx_prev_int %>% left_join(mx_paym,by="SK_ID_CURR") 
mx_prev_int = mx_prev_int %>% left_join(mx_crd,by="SK_ID_CURR") 

rm(mx_prev,mx_pos,mx_paym,mx_crd)
gc()

mn_prev_int = mn_prev %>% left_join(mn_pos,by="SK_ID_CURR") 
mn_prev_int = mn_prev_int %>% left_join(mn_paym,by="SK_ID_CURR") 
mn_prev_int = mn_prev_int %>% left_join(mn_crd,by="SK_ID_CURR") 

rm(mn_prev,mn_pos,mn_paym,mn_crd)
gc()

avg_prev_int = avg_prev_int %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

mx_prev_int = mx_prev_int %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

mn_prev_int = mn_prev_int %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

gc()


avg_bureau_int = avg_bureau %>% left_join(avg_bureau_blc,by="SK_ID_BUREAU")%>% select(-SK_ID_BUREAU)

rm(avg_bureau_blc,avg_bureau)
gc()

mx_bureau_int = mx_bureau %>% left_join(mx_bureau_blc,by="SK_ID_BUREAU")%>% select(-SK_ID_BUREAU)
mn_bureau_int = mn_bureau %>% left_join(mn_bureau_blc,by="SK_ID_BUREAU")%>% select(-SK_ID_BUREAU)

rm(mx_bureau_blc,mn_bureau_blc,mx_bureau,mn_bureau)
gc()


avg_bureau_int = avg_bureau_int %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))
mx_bureau_int = mx_bureau_int %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))
mn_bureau_int = mn_bureau_int %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))



prev_cons_lns_avg = prev_cons_lns_avg %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_cons_lns_mx = prev_cons_lns_mx %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_cons_lns_mn = prev_cons_lns_mn %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_cash_lns_avg = prev_cash_lns_avg %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_cash_lns_mx = prev_cash_lns_mx %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_cash_lns_mn = prev_cash_lns_mn %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_res_lns_avg = prev_res_lns_avg %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_res_lns_mx = prev_res_lns_mx %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_res_lns_mn = prev_res_lns_mn %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_xna_lns_avg = prev_xna_lns_avg %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_xna_lns_mx = prev_xna_lns_mx %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_xna_lns_mn = prev_xna_lns_mn %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

bureau_type_avg = bureau_type_avg %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

bureau_type_mx = bureau_type_mx %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

bureau_type_mn = bureau_type_mn %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

crd_type_avg = crd_type_avg %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

crd_type_mx = crd_type_mx %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

crd_type_mn = crd_type_mn %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_nm_status1_avg = prev_nm_status1_avg %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_nm_status2_avg = prev_nm_status2_avg %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_nm_status3_avg = prev_nm_status3_avg %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

prev_nm_status4_avg = prev_nm_status4_avg %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

tr_te = tr %>% 
  select(-TARGET) %>% 
  bind_rows(te) %>%
  left_join(avg_prev_int, by = c("SK_ID_CURR"="SK_ID_CURR")) %>% 
  left_join(avg_bureau_int, by = "SK_ID_CURR") %>%
  left_join(mx_bureau_int, by ="SK_ID_CURR" ) %>%
  left_join(mx_prev_int, by ="SK_ID_CURR" ) %>%
  left_join(mn_bureau_int, by ="SK_ID_CURR" ) %>%
  left_join(mn_prev_int, by ="SK_ID_CURR" ) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .))) 

rm(avg_prev_int,avg_bureau_int,mx_bureau_int,mx_prev_int,mn_bureau_int,mn_prev_int)
gc()

tr_te = tr_te %>% left_join(prev_cons_lns_avg,by="SK_ID_CURR")%>% left_join(prev_cons_lns_mx,by="SK_ID_CURR")%>% left_join(prev_cons_lns_mn,by="SK_ID_CURR")
rm(prev_cons_lns_avg,prev_cons_lns_mx,prev_cons_lns_mn,prev_cons_lns_mx)

tr_te = tr_te %>% left_join(prev_cash_lns_avg,by="SK_ID_CURR") %>% left_join(prev_cash_lns_mx,by="SK_ID_CURR") %>% left_join(prev_cash_lns_mn,by="SK_ID_CURR")
rm(prev_cash_lns_avg,prev_cash_lns_mx,prev_cash_lns_mn,prev_cash_lns_mx)

tr_te = tr_te %>% left_join(prev_res_lns_avg,by="SK_ID_CURR")%>% left_join(prev_res_lns_mx,by="SK_ID_CURR")%>% left_join(prev_res_lns_mn,by="SK_ID_CURR")
rm(prev_res_lns_avg,prev_res_lns_mx,prev_res_lns_mn,prev_res_lns_mx)

tr_te = tr_te %>% left_join(prev_xna_lns_avg,by="SK_ID_CURR")%>% left_join(prev_xna_lns_mx,by="SK_ID_CURR")%>% left_join(prev_xna_lns_mn,by="SK_ID_CURR")
rm(prev_xna_lns_avg,prev_xna_lns_mx,prev_xna_lns_mn,prev_xna_lns_mx)

tr_te = tr_te %>% left_join(prev_nm_status1_avg,by="SK_ID_CURR")
rm(prev_nm_status1_avg)
gc()

tr_te = tr_te %>% left_join(prev_nm_status2_avg,by="SK_ID_CURR")
rm(prev_nm_status2_avg)
gc()

tr_te = tr_te %>% left_join(prev_nm_status3_avg,by="SK_ID_CURR")
rm(prev_nm_status3_avg)
gc()

tr_te = tr_te %>% left_join(prev_nm_status4_avg,by="SK_ID_CURR")
rm(prev_nm_status4_avg)
gc()


tr_te = tr_te %>% left_join(filter(bureau_type_avg,CREDIT_TYPE=="1"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="2"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="3"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="4"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="5"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="6"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="7"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="8"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="9"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="10"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="11"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="12"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="13"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="14"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_avg,CREDIT_TYPE=="15"),by="SK_ID_CURR")
rm(bureau_type_avg)
gc()

tr_te = tr_te %>% left_join(filter(bureau_type_mx,CREDIT_TYPE=="1"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="2"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="3"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="4"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="5"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="6"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="7"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="8"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="9"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="10"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="11"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="12"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="13"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="14"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mx,CREDIT_TYPE=="15"),by="SK_ID_CURR")
rm(bureau_type_mx)
gc()

tr_te = tr_te %>% left_join(filter(bureau_type_mn,CREDIT_TYPE=="1"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="2"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="3"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="4"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="5"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="6"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="7"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="8"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="9"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="10"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="11"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="12"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="13"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="14"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_type_mn,CREDIT_TYPE=="15"),by="SK_ID_CURR")
rm(bureau_type_mn)
gc()

tr_te = tr_te %>% left_join(filter(bureau_actavg,CREDIT_ACTIVE=="1"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_actavg,CREDIT_ACTIVE=="2"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_actavg,CREDIT_ACTIVE=="3"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_actavg,CREDIT_ACTIVE=="4"),by="SK_ID_CURR")

rm(bureau_actavg)
gc()

tr_te = tr_te %>% left_join(filter(bureau_actmn,CREDIT_ACTIVE=="1"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_actmn,CREDIT_ACTIVE=="2"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_actmn,CREDIT_ACTIVE=="3"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_actmn,CREDIT_ACTIVE=="4"),by="SK_ID_CURR")

rm(bureau_actmn)
gc()


tr_te = tr_te %>% left_join(filter(bureau_actmx,CREDIT_ACTIVE=="1"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_actmx,CREDIT_ACTIVE=="2"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_actmx,CREDIT_ACTIVE=="3"),by="SK_ID_CURR") %>% 
  left_join(filter(bureau_actmx,CREDIT_ACTIVE=="4"),by="SK_ID_CURR")

rm(bureau_actmx)
gc()



tr_te = tr_te %>% left_join(filter(crd_type_avg,NAME_CONTRACT_STATUS=="1"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_avg,NAME_CONTRACT_STATUS=="2"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_avg,NAME_CONTRACT_STATUS=="3"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_avg,NAME_CONTRACT_STATUS=="4"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_avg,NAME_CONTRACT_STATUS=="5"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_avg,NAME_CONTRACT_STATUS=="6"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_avg,NAME_CONTRACT_STATUS=="7"),by="SK_ID_CURR") 
rm(crd_type_avg)
gc()

tr_te = tr_te %>% left_join(filter(crd_type_mx,NAME_CONTRACT_STATUS=="1"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mx,NAME_CONTRACT_STATUS=="2"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mx,NAME_CONTRACT_STATUS=="3"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mx,NAME_CONTRACT_STATUS=="4"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mx,NAME_CONTRACT_STATUS=="5"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mx,NAME_CONTRACT_STATUS=="6"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mx,NAME_CONTRACT_STATUS=="7"),by="SK_ID_CURR") 
rm(crd_type_mx)
gc()

tr_te = tr_te %>% left_join(filter(crd_type_mn,NAME_CONTRACT_STATUS=="1"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mn,NAME_CONTRACT_STATUS=="2"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mn,NAME_CONTRACT_STATUS=="3"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mn,NAME_CONTRACT_STATUS=="4"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mn,NAME_CONTRACT_STATUS=="5"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mn,NAME_CONTRACT_STATUS=="6"),by="SK_ID_CURR") %>% 
  left_join(filter(crd_type_mn,NAME_CONTRACT_STATUS=="7"),by="SK_ID_CURR") 
rm(crd_type_mn)
gc()

tr_te = tr_te %>% mutate(x1= AMT_CREDIT.x/AMT_INCOME_TOTAL,
                         x2= AMT_CREDIT.y/AMT_INCOME_TOTAL,
                         x3=AMT_ANNUITY.x/AMT_INCOME_TOTAL,
                         x4=AMT_ANNUITY.x/AMT_INCOME_TOTAL,##
                         x5=AMT_ANNUITY.y/AMT_INCOME_TOTAL,
                         x6=AMT_CREDIT.x/AMT_ANNUITY.x, ##
                         x7=AMT_CREDIT.y/AMT_ANNUITY.x,
                         x8=AMT_CREDIT.x/AMT_ANNUITY.y,
                         x9=AMT_CREDIT.y/AMT_ANNUITY.y,
                         x10 = AMT_CREDIT.x/AMT_GOODS_PRICE.y,
                         x11 = AMT_CREDIT.x/AMT_GOODS_PRICE.x, ##
                         x12 = AMT_CREDIT.y/AMT_GOODS_PRICE.y,
                         x13 = AMT_CREDIT.y/AMT_GOODS_PRICE.x,
                         x14 = AMT_GOODS_PRICE.x/AMT_INCOME_TOTAL,
                         x15 = AMT_GOODS_PRICE.y/AMT_INCOME_TOTAL,
                         x16 = DAYS_EMPLOYED/DAYS_BIRTH, ##,
                         x17 = AMT_GOODS_PRICE.x/DAYS_BIRTH,
                         x18 = AMT_ANNUITY.x/DAYS_BIRTH,
                         x19 = AMT_ANNUITY.x/AMT_INCOME_TOTAL/DAYS_BIRTH,
                         x20 = AMT_GOODS_PRICE.x/DAYS_EMPLOYED,
                         x21 = AMT_ANNUITY.x/DAYS_EMPLOYED,
                         x22 = AMT_ANNUITY.x/AMT_INCOME_TOTAL/DAYS_EMPLOYED,
                         x23 = AMT_PAYMENT.x/DAYS_REGISTRATION,
                         x24 = DAYS_CREDIT.y/DAYS_BIRTH,
                         x25 = AMT_ANNUITY.x/AMT_INCOME_TOTAL/DAYS_BIRTH,
                         x26 = AMT_CREDIT.x/DAYS_BIRTH,
                         x27 = DAYS_EMPLOYED/DAYS_BIRTH*AMT_CREDIT.x,
                         x28 = DAYS_EMPLOYED/DAYS_BIRTH*AMT_ANNUITY.x,
                         x29 = AMT_ANNUITY.x/DAYS_CREDIT.y,
                         x30 = AMT_CREDIT.x/DAYS_CREDIT.y,
                         x32 = AMT_GOODS_PRICE.x/DAYS_CREDIT.y,
                         x33 = AMT_ANNUITY.x/AMT_INCOME_TOTAL/DAYS_CREDIT.y
) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))



bureau = read_csv("bureau.csv")

bureau_rec_1k = bureau %>% filter(DAYS_CREDIT>=-1000) %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

bureau_rec_0.5k = bureau %>% filter(DAYS_CREDIT>=-500) %>% mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .)))

avg_bureau_rec_1k = bureau_rec_1k %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(buro_count = bureau_rec_1k %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

mx_bureau_rec_1k = bureau_rec_1k %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) 

mn_bureau_rec_1k = bureau_rec_1k %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) 

avg_bureau_rec_0.5k = bureau_rec_0.5k %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(buro_count = bureau_rec_0.5k %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

mx_bureau_rec_0.5k = bureau_rec_0.5k %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) 

mn_bureau_rec_0.5k = bureau_rec_0.5k %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) 


rm(bureau)
gc()

tr_te = tr_te %>% left_join (avg_bureau_rec_1k, by = "SK_ID_CURR") %>% 
  left_join (mx_bureau_rec_1k, by = "SK_ID_CURR") %>%
  left_join (mn_bureau_rec_1k, by = "SK_ID_CURR") %>%
  left_join (avg_bureau_rec_0.5k, by = "SK_ID_CURR") %>%
  left_join (mn_bureau_rec_0.5k, by = "SK_ID_CURR") %>%
  left_join (mx_bureau_rec_0.5k, by = "SK_ID_CURR")

rm(avg_bureau_rec_1k,mx_bureau_rec_1k,mn_bureau_rec_1k,avg_bureau_rec_0.5k,mx_bureau_rec_0.5k,mn_bureau_rec_0.5k,bureau_rec_0.5k,bureau_rec_1k)
gc()
tr_te_df = tr_te
#write.csv(tr_te,"tr_te.csv",row.names=F)
dim(tr_te)
tr_te = tr_te %>% data.matrix()

y = tr$TARGET
tri = 1:nrow(tr)

dtest = tr_te[-tri,] %>% data.matrix()

tr_te = tr_te[tri, ]
tri = caret::createDataPartition(y, p = 0.9, list = F) %>% c()
dtrain = lgb.Dataset(data = tr_te[tri, ], label = y[tri], missing = -999)
dval = lgb.Dataset(data = tr_te[-tri, ], label = y[-tri], missing = -999)
rm(tr_te,tr)

params.lgb = list(
  objective = "binary",
  metric = "auc",
  nthread=4,
  n_estimators=10000,
  learning_rate=0.02,
  num_leaves=32,
  colsample_bytree=0.9497036,
  subsample=0.8715623,
  max_depth=8,
  reg_alpha=0.04,
  reg_lambda=0.073,
  min_split_gain=0.0222415,
  min_child_weight=40,
  silent=-1,
  verbose=-1
)

lgb.model <- lgb.train(
  params = params.lgb
  , data = dtrain
  , valids = list(val = dval)
  , learning_rate = 0.05
  , num_leaves = 7
  , num_threads = 2
  , nrounds = 3000
  , early_stopping_rounds = 200
  , eval_freq = 5
)
