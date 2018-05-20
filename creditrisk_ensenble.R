# With features form https://www.kaggle.com/ogrellier/good-fun-with-ligthgbm
library(tidyverse)
library(xgboost)
library(magrittr)
library("Ckmeans.1d.dp")
set.seed(0)

#---------------------------
cat("Loading data...\n")
tr <- read_csv("application_train.csv") 
te <- read_csv("application_test.csv")
bureau <- read_csv('bureau.csv')
prev <- read_csv('previous_application.csv')

#---------------------------
cat("Preprocessing...\n")

prev %<>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

avg_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev %>%  
           group_by(SK_ID_CURR, SK_ID_PREV) %>% 
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

bureau %<>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

avg_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(buro_count = bureau %>%  
           group_by(SK_ID_BUREAU, SK_ID_CURR) %>% 
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

tri <- 1:nrow(tr)
y <- tr$TARGET

tr_te <- tr %>% 
  select(-TARGET) %>% 
  bind_rows(te) %>%
  left_join(avg_prev, by = "SK_ID_CURR") %>% 
  left_join(avg_bureau, by = "SK_ID_CURR") %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), -999, .))) %>% 
  data.matrix()

rm(tr, te, prev, avg_prev, bureau, avg_bureau); gc()

#---------------------------
cat("Preparing data...\n")
dtest <- xgb.DMatrix(data = tr_te[-tri, ], missing = -999)
tr_te <- tr_te[tri, ]
tri <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()
dtrain <- xgb.DMatrix(data = tr_te[tri, ], label = y[tri], missing = -999)
dval <- xgb.DMatrix(data = tr_te[-tri, ], label = y[-tri], missing = -999)
cols <- colnames(tr_te)

rm(tr_te, y, tri); gc()

#---------------------------
cat("Training model...\n")
p1 <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 8,
          eta = 0.01,
          max_depth = 15,
          min_child_weight = 5,
          gamma = 0,
          subsample = 0.7,
          colsample_bytree = 0.8,
          alpha = 0,
          lambda = 0,
          nrounds = 4000)

m_xgb1 <- xgb.train(p1, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 500)

p2 <- list(objective = "binary:logistic",
           booster = "gbtree",
           eval_metric = "auc",
           nthread = 8,
           eta = 0.01,
           max_depth = 5,
           min_child_weight = 5,
           gamma = 0,
           subsample = 0.7,
           colsample_bytree = 0.8,
           alpha = 0,
           lambda = 0,
           nrounds = 4000)

m_xgb2 <- xgb.train(p2, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 500)

result1 = read_csv("../input/sample_submission.csv") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb1, dtest)) %>%

result2 = read_csv("../input/sample_submission.csv") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb2, dtest)) %>%
  
result_fin = result1 %>% left_join(result2,by="SK_ID_CURR") %>% mutate(TARGET_AVG = x.TARGET/2 + y.TARGET/2) %>% 
                select(SK_ID_CURR,TARGET = TARGET_AVG)

#---------------------------
read_csv("../input/sample_submission.csv") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb, dtest)) %>%
  write_csv(paste0("tidy_xgb_test", round(m_xgb$best_score, 5), ".csv"))


