
setwd("/Users/Sooyoung/kaggle/CreditRisk/")

# library(dplyr)
# library(data.table)

############# Load data
# 
# application_test = fread("application_test.csv")
# head(application_train)
# #This is the main table, broken into two files for Train (with TARGET) and Test (without TARGET).
# application_train = fread("application_train.csv")
# bureau_balnce = fread("bureau_balance.csv")
# bureau = fread("bureau.csv")
# credit_card_balance = fread("credit_card_balance.csv")
# installments_payments = fread("installments_payments.csv")
# POS_CASH_balance = fread("POS_CASH_balance.csv")
# previous_application = fread("previous_application.csv")
# sample_submission = fread("sample_submission.csv")


# With features form https://www.kaggle.com/ogrellier/good-fun-with-ligthgbm
library(tidyverse)
library(xgboost)
library(magrittr)
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

X = dtrain

searchGridSubCol <- expand.grid(subsample = c(0.5, 1.0), 
                                colsample_bytree = c(0.5, 1.0),
                                max_depth = c(3, 10),
                                min_child = seq(1), 
                                eta = c(0.01,0.2)
)

ntrees <- 5

system.time(
  loglossErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data =  X, nrounds = ntrees, nfold = 2, showsd = TRUE, 
                             metrics = "logloss", verbose = TRUE, "eval_metric" = "logloss",
                             "objective" = "reg:logistic", "max.depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    logloss <- tail(xvalidationScores$test_logloss_mean, 1)
    tlogloss <- tail(xvalidationScores$train_logloss_mean,1)
    output <- return(c(logloss, tlogloss, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))
    
  }))

output <- as.data.frame(t(loglossErrorsHyperparameters))
head(output)
varnames <- c("Testlogloss", "Trainlogloss", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")
names(output) <- varnames
head(output)


p <- list(objective = "reg:logistic",
          booster = "gbtree",
          eval_metric = "logloss",
          nthread = 8,
          eta = 0.20,
          max_depth = 10,
          min_child_weight = 1,
          gamma = 0,
          subsample = 1.0,
          colsample_bytree = 1.0,
          alpha = 0,
          lambda = 0,
          nrounds = 4000)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 100)

xgb.importance(cols, model=m_xgb) %>% 
  xgb.plot.importance(top_n = 30)

#---------------------------
read_csv("../input/sample_submission.csv") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb, dtest)) %>%
  write_csv(paste0("tidy_xgb_", round(m_xgb$best_score, 5), ".csv"))




