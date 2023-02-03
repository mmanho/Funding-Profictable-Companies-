library(tidyverse)
library(readxl)
library(dplyr)
library(stringi)
install.packages("stringdist")
library(stringdist)
library(glmnet)
library(randomForest)
library(mlbench)
library(caret)


train_1 <- read_excel("PitchBook_My_Layout_2.xlsx")
summary(train_1)
train_2 <- read_excel("PitchBook_My_Layout_3.xlsx")
summary(train_2)
train_3 <- read_excel("PitchBook_My_Layout_1.xlsx")
summary(train_3)

common <- intersect(names(train_1), names(train_2)) 
common_2 <- intersect(common, names(train_3))

train <- rbind(train_1[,common_2], train_2[,common_2], train_3[,common_2])# the common variables within three train data set
summary(train)
train_success = select(train, -c("Size Multiple", "First Financing Valuation", "Revenue", "Revenue Growth %", "Gross Profit", "Net Income", "Enterprise Value", "EBITDA", "EBIT", "Market Cap", "Net Debt", "Last Financing Size", "PitchBook Link", "Primary Contact", "Website", "Fiscal Period", "Active Investors", "Last Financing Date") )
summary(train_success)
sapply(train_success, function(x) sum(is.na(x)))


train_success <- train_success %>%   #the data you want to show
  mutate(`Ownership Status`= as.factor(`Ownership Status`), # the data want to modify
         success= ifelse(`Ownership Status` == "Out of Business", 0, 1))# the data want to add



train_success <- train_success %>%
  drop_na("Year Founded")

summary(train_success)
sapply(train_success, function(x) sum(is.na(x)))

view(train_success)

train_success <- train_success %>%
  group_by(`Primary Industry Code`, `Company Financing Status`) %>%
  mutate(total_raised_med = median(`Total Raised`, na.rm = TRUE)) %>% #group by() to calculate median
  ungroup() %>% #ungroup data
  mutate(`Total Raised` = ifelse(is.na(`Total Raised`), total_raised_med, `Total Raised`)) #subtitute NA with median


train_success <- train_success %>%
  drop_na("Total Raised")  # drop rows contain missing value
train_success <- train_success %>%
  drop_na("HQ Location")


train_success <- train_success %>%
  group_by(`Primary Industry Code`, `Company Financing Status`) %>%
  mutate(first_financing_med = median(`First Financing Size`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`First Financing Size` = ifelse(is.na(`First Financing Size`), first_financing_med, `First Financing Size`))

train_success <- train_success %>%
  drop_na("First Financing Size")

sapply(train_success, function(x) sum(is.na(x)))


#Binning Total Raised
train_success <- train_success %>%
  mutate( total_raised_factor = case_when(`Total Raised`>= 0 & `Total Raised`< 100 ~ "0-100",
                                          `Total Raised` >= 100 & `Total Raised`< 500 ~ "100-500", 
                                          `Total Raised`>= 500 & `Total Raised`< 900 ~ "500-900",
                                          `Total Raised`>= 900 & `Total Raised`< 5000 ~ "900-5000",
                                          `Total Raised` >= 5000 ~ ">5000", 
                                          TRUE ~ "unknown"  #value to output if condition is FALSE| formula to everything else
  ))  ## case when (condition ~ output)

#cosine similarity between company name and description 
train_success <- train_success %>%
  mutate(similarity_company_desc = stringdist(Companies, Description, method = "cosine"))


#extracting frequency of companies HQ location 
train_success <- train_success %>%
  mutate(state = stri_sub(train_success$`HQ Location`,-2)) ## Negative indices

train_success <- train_success %>%
  group_by(state) %>%
  mutate(location_freq = n()) %>%
  ungroup() %>%
  mutate(popular_loc = ifelse(location_freq > 500, 1, 0))

#Binning First financial Size
train_success <- train_success %>%
  mutate( first_financial_factor = case_when(`First Financing Size`>= 0 & `First Financing Size`< 25 ~ "0-25",
                                             `First Financing Size`>= 25 & `First Financing Size`< 50 ~ "25-50",
                                             `First Financing Size` >= 50 & `First Financing Size`< 100 ~ "50-100",
                                             `First Financing Size` >= 100 ~ ">100",
                                             TRUE ~ "unknown" ))




#Extracting top 2 Primary Industry Codes

Primary_industry_success <- train_success %>%
  group_by(`Primary Industry Code`) %>%
  summarise(Primary_industry_freq = sum(success)) %>%
  ungroup() %>%
  slice_max(Primary_industry_freq, n = 2)
train_success$top2_primary_industry <- as.integer(train_success$`Primary Industry Code` %in% Primary_industry_success$`Primary Industry Code`)
view(train_success)

#Extracting top 5 Last Financing Deal Type

Last_financing_success <- train_success %>%
  group_by(`Last Financing Deal Type`) %>%
  summarise(Last_financing_freq = sum(success)) %>%
  ungroup() %>%
  slice_max(Last_financing_freq, n = 5)
train_success$top5_financing_deal <- as.integer(train_success$`Last Financing Deal Type` %in% Last_financing_success$`Last Financing Deal Type`)# %in% return TRUE/FALSE, as.integer() --> 0/1


#Extracting top 2 Company Financing Status

Company_financing_success <- train_success %>%
  group_by(`Company Financing Status`) %>%
  summarise(Company_financing_freq = sum(success)) %>%
  ungroup() %>%
  slice_max(Company_financing_freq, n = 2)
train_success$top2_company_financing_status <- as.integer(train_success$`Company Financing Status` %in% Company_financing_success$`Company Financing Status`)



train_success <- train_success %>%
  mutate(total_raised_factor = as.factor(total_raised_factor),
         first_financial_factor=as.factor(first_financial_factor),
         `Primary Industry Code`=as.factor(`Primary Industry Code`),
         `Company Financing Status`=as.factor(`Company Financing Status`),
         `Last Financing Deal Type`=as.factor(`Last Financing Deal Type`),
         top2_primary_industry=as.factor(top2_primary_industry),
         top5_financing_deal=as.factor(top5_financing_deal),
         top2_company_financing_status=as.factor(top2_company_financing_status),
         success=as.factor(success))

summary(train_success)

train_success<-train_success[,-3]

view(train_success)

write.csv(train_success, "/Users/ruimin/Desktop/UMD MSMA/2022spring/summer program/FinalData_ddcf.csv", row.names=FALSE)


####################modeling###############################
library(caret)
clean_train <- train_success %>%
  select(`Total Raised`,
        # `First Financing Size`,
        # duration,
         similarity_company_desc,
         popular_loc,
         first_financial_factor,
         top2_primary_industry,
         top5_financing_deal,
         top2_company_financing_status,
         success
  )



###########LOGISTIC MODEL
dummy <- dummyVars(~., data = clean_train, fullRank = TRUE)
one_hot_lc <- data.frame(predict(dummy, newdata = clean_train))
one_hot_lc <- one_hot_lc %>% 
  mutate(success.1 = as.factor(success.1))

#Partitioning into train and test using indices 
set.seed(4)
tr_inst <- sample(nrow(train_success), .7*nrow(train_success))

va <- one_hot_lc[-tr_inst,]
tr <- one_hot_lc[tr_inst,]
va_y <- va$success.1
va_x <- va %>% select (-c(success.1))
tr_x <- tr%>% select(-c(success.1))
tr_y <- tr$success.1

logistic_success <-glm(success.1 ~.,family=binomial(link='logit'), data=tr)
summary(logistic_success)
probs_success <- predict(logistic_success, newdata = va_x, type = "response")

classifications_success <- ifelse(probs_success > .50, 1, 0)
#classifications_success <- ifelse(is.na(classifications_success), "NO", classifications_success)
summary(classifications_success)
accuracy <- function(classifications, actuals){
  correct_classifications <- ifelse(classifications == actuals, 1, 0)
  acc <- sum(correct_classifications)/length(classifications)
  return(acc)
}
accuracy(classifications_success,va_y)


#Random Forest Model
set.seed(1)

rf.mod <- randomForest(success.1~.,
                       data=tr,
                       mtry=3, ntree=10000,
                       importance=TRUE)

rf_preds <- predict(rf.mod, newdata=va)
rf_acc <- mean(ifelse(rf_preds==va$success.1,1,0))

rf.mod
rf_acc


#plot the variable importances (the average decrease in impurity when splitting across that variable)
importance(rf.mod)
varImpPlot(rf.mod)



#RF Crossvalidation
x <- tr_x
y <- tr_y
seed <- 7
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(success.1~., data=tr, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
