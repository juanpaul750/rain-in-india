library(Amelia)
library(tidyverse)
library(mice)
library(naniar)
library(broom)
library(ggplot2)
library(ggthemes)
library(survival)
library(corrplot)
library(ggThemeAssist)
library(esquisse)
library(tidymodels)
library(agricolae)
library(Stat2Data)
library(tree)
library(VIM)


#FOOD COST DATASET
food_price = read.csv("https://raw.githubusercontent.com/juanpaul750/indian-food-price/main/india.csv")

# WRANGLING
food_price = food_price %>% 
  rename( Mustard_oil = "Oil..mustard.", Potato = "Potatoes", Date = "date"  )
head(food_price)

# TABS AND PLOTS OF MISSING VALUES
md.pattern(food_price)
missing_dt_food = miss_var_summary(food_price)
missmap(food_price)

# RAIN DATASET
rain = read.csv("https://raw.githubusercontent.com/juanpaul750/rain-in-india/main/Sub_Division_IMD_2017.csv")

# WRANGLING
rain = rain%>%
  select(SUBDIVISION, YEAR, JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC) %>%
  rename(Region = "SUBDIVISION")

# ALL IMPLICIT MISSING DATA
rain = rain%>% 
  tidyr::complete(Region,YEAR)  # Note: found some implicit data

# TABS AND PLOTS OF MISSING VALUES
missing_dt_rain = miss_var_summary(rain)
missing_dt_rain = missing_dt_rain %>%
  rename(no_of_miss = "n_miss", percent_miss = "pct_miss")

gg_miss_var(rain) + labs(y = " number of missing values")+ 
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))
gg_miss_var(rain, show_pct = T) + theme(axis.title = element_text(size = 20),
    axis.text = element_text(size = 15))

# PLOTIN USING BIND SHADOW
food_price %>%
  bind_shadow()%>%
  ggplot(aes(x= Potato,
             colour = Onions_NA)) +
  geom_density()


# Data wrangling
rain = arrange(rain, YEAR)
rain = rain[rain$YEAR > 1998, ]

# CREATING MEAN RAIN FALL DATASET
df = group_by (rain,YEAR)
dtd = summarise(df, JAN = mean(JAN), FEB = mean(FEB), MAR = mean(MAR), APR = mean(APR), MAY = mean(MAY), JUN = mean(JUN),
                JUL = mean(JUL),AUG = mean(AUG), SEP = mean(SEP), OCT = mean(OCT), NOV = mean(NOV), DEC = mean(DEC) )

# USE GATHER FUNTION TO CREAT A LONG FORMAT DATASET
rain_column = dtd%>%
  gather(month,rain_mean,-1)%>%
  arrange(YEAR)

# CREATING NEW COLUMN WITH RAIN CATEGORY
rain_column = rain_column[-c(1:5),]
rain_column = mutate(rain_column, sl.no = c(1:223),rain_category =
                       case_when(rain_mean <= 15 ~ "Light Rain",
                                 rain_mean <= 50 ~ "Med Rain",
                                 rain_mean <= 120 ~ "Heavy Rain",
                                 rain_mean > 120 ~ "Very Heavy Rain",))
rain_column$rain_category = as.factor(rain_column$rain_category)
food_price = mutate(food_price, sl.no = c(9:248))

# MERGE THE TWO DATASET
food_rain = merge(food_price,rain_column, by = 'sl.no', all = TRUE)
food_rain = select(food_rain,Date,Chickpeas,Mustard_oil,Potato,Rice,Sugar,Wheat,Onions,rain_mean,rain_category )

# T-Test if the data set is MAR
food_rain_ttest = food_rain[-c(1:8),]
# T - test for mustard oil and rain test
food_rain_ttest = food_rain_ttest %>% 
  mutate(missing_oil = is.na(Mustard_oil))

# T test for combo
missing_oil_light = food_rain_ttest %>% 
  filter(rain_category == "Light Rain") %>% 
  pull(missing_oil)

missing_oil_med = food_rain_ttest %>% 
  filter(rain_category == "Med Rain") %>% 
  pull(missing_oil)

missing_oil_heavy = food_rain_ttest %>% 
  filter(rain_category == "Heavy Rain") %>% 
  pull(missing_oil)

missing_oil_vheavy = food_rain_ttest %>% 
  filter(rain_category == "Very Heavy Rain") %>% 
  pull(missing_oil)

t.test(missing_oil_light, missing_oil_med)
t.test(missing_oil_vheavy, missing_oil_med)
t.test(missing_oil_heavy, missing_oil_med)
t.test(missing_oil_light, missing_oil_heavy)
t.test(missing_oil_vheavy, missing_oil_heavy)

# VISUALISING TEST FOR MAR 

food_rain_ttest %>%
  select(rain_category,Onions)%>%
  spineMiss()

food_rain_ttest %>%
  select(rain_category, Mustard_oil)%>%
  spineMiss()


# CREATING THE LAG TO ACCOUNT FOR THE CROP HARVEST TIME
# LAG TO COMPARE ONION PRICE AND RAIN MEAN
f = 3
lag_onion = food_rain %>%
  mutate(rain_mean = lag(rain_mean, n = f))%>%
  mutate(rain_category = lag(rain_category, n = f))


# ANOVA HYPOTHESIS TESTING TEST WITH onion data
lag_on_grp = arrange(lag_onion, rain_mean)
lag_on_grp$rain_category = factor(lag_on_grp$rain_category , levels=c('Light Rain' , 'Med Rain', 'Heavy Rain','Very Heavy Rain'))
data_mean = aggregate(lag_on_grp$Onions,list(lag_on_grp$rain_category),mean,na.rm = T)

# BOX PLOT 
lag_on_grp %>%
  filter(!is.na(rain_category)) %>%
  ggplot()+
  aes(x = rain_category, y = Onions, fill = rain_category)+
  geom_boxplot()+
  theme(legend.position="none")+
  stat_summary(fun = 'mean', colour = 'black', geom = 'point' )+
  stat_summary(fun = 'mean', colour = 'black', geom = 'text',
               vjust = 1,aes(label = paste("Mean:",round(..y.., digits = 1))))+
  labs(
    x = "Rain Category",
    y = "Onion Price ( in RS)",
    title = "Mean and Meadian"
  )

# TABLE TO SHOW SD AND MEAN 
lag_on_grp %>%
  group_by(rain_category)%>%
  summarize(mean(Onions, na.rm = TRUE), sd(Onions, na.rm =TRUE) )

# ANOVA TEST SUMMARY
categories_of_rain = lag_onion$rain_category
out = aov(lag_onion$Onions ~ categories_of_rain)
summary(out)

#MULTIPLE COMPARISION
# PAIRWISE T TEST WITHOUT CORRECTION
with(lag_onion, pairwise.t.test(Onions, rain_category, p.adjust.method = "none"))

# BONFERRONI CORRECTION
with(lag_onion, pairwise.t.test(Onions, rain_category, p.adjust.method = "bonferroni"))

# TUKEYS CORRECTION FOR PLOTING CONFIDENCE INTERVAL 
TukeyHSD(out)
par(mar = c(4.5, 11.5, 4.5, 1.9))
plot(TukeyHSD(out),  las = 1)


# MISSING VALUE IMPUTAION USING AMELIA FOR ONION PRICE ANALYSIS
lag_onion_w = lag_onion[-c(1:8),]
head(lag_onion_w)
itr_food_rain = amelia(x = lag_onion_w, m=30, idvars = "Date", ords = 'rain_category')

# LINEAR MODEL
lm.out.food.rain = lapply(itr_food_rain $ imputation,function(i) lm(Onions ~ rain_mean , data = i))

coefs.food.rain = lm.out.food.rain %>%
  sapply(coef)%>%
  t()

coefs.food.rain 

my_stdE_extr = function(model){summary(model)$coef[,"Std. Error"]}

ses.food.rain = lm.out.food.rain %>%
  sapply(my_stdE_extr)%>%
  t()

mi.meld(coefs.food.rain, ses.food.rain)

# TIDY SUMMARY
al_imp_oni = bind_rows(unclass(itr_food_rain$imputations), .id = "m") %>%
  group_by(m) %>%
  nest()

mod_imputations = al_imp_oni %>%
  mutate(model = data %>% 
           map(~ lm(Onions ~ rain_mean , data = .)),tidied = model %>%
           map(~ tidy(., conf.int = TRUE)),glance = model %>% 
           map(~ glance(.)))

#models_imputations
mod_imputations %>%
  filter(m == "imp1") %>%
  unnest(tidied)

# FINDING COEFFICENT AND STD ERROR
param_coef_er = mod_imputations %>%
  unnest(tidied) %>%
  select(m, term, estimate, std.error) %>%
  gather(key, value, estimate, std.error) %>%
  spread(term, value)

# EXTRACTING COEFFICIENT
just_coefs = param_coef_er %>%
  filter(key == "estimate") %>%
  ungroup %>%
  select(-m, -key)

# EXTRACTING STD ERROR
just_ses = param_coef_er %>%
  filter(key == "std.error") %>%
  ungroup %>%
  select(-m, -key)

coefs_melded = mi.meld(just_coefs, just_ses)

# EXTRACTING DEGREE OF FREEDOM

model_degree_freedom = mod_imputations %>%
  unnest(glance) %>%
  filter(m == "imp1") %>%
  pull(df.residual)

# SUMMARY
mod_mel_summary = as.data.frame(cbind(t(coefs_melded$q.mi), t(coefs_melded$se.mi))) %>%
  magrittr::set_colnames(c("estimate", "std.error")) %>%
  mutate(term = rownames(.)) %>%
  select(term, everything()) %>%
  mutate(statistic = estimate / std.error,
         conf.low = estimate + std.error * qt(0.025, model_degree_freedom),
         conf.high = estimate + std.error * qt(0.975, model_degree_freedom),
         p.value = 2 * pt(abs(statistic), model_degree_freedom, lower.tail = FALSE))

mod_mel_summary
coefs_melded

# PLOTING 
# DENSITY PLOT FOR FEW IMPUTAIONS
ggplot()+
  geom_density(aes(x = rain_mean ), data = itr_food_rain$imputations$imp11)+
  geom_density(aes(x = rain_mean),  data = itr_food_rain$imputations$imp30)+
  geom_density(aes(x = rain_mean), data = itr_food_rain$imputations$imp6)+
  geom_density(aes(x = rain_mean), data = itr_food_rain$imputations$imp16)+
  geom_density(aes(x = rain_mean), data = itr_food_rain$imputations$imp21)+
  geom_density(aes(x = rain_mean), data = itr_food_rain$imputations$imp26)+
  geom_density(aes(x = rain_mean),colour = 'blue', data = itr_food_rain$imputations$imp2)+
  geom_density(aes(x = rain_mean, colour = 'red'),  data = lag_onion )+
  ggtitle("DENSITY PLOT FOR IMPUTED AND ORIGINAL DATA")+
  theme_fivethirtyeight()+theme(axis.title = element_text())+
  labs(x= "rain fall (in mm)",y="DENSITY") +labs(colour = "ORIGINAL")+theme_fivethirtyeight()+ 
  theme(panel.grid.major = element_line(colour = "mistyrose"),
    plot.title = element_text(colour = "red"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white",
        colour = "white"), legend.key = element_rect(fill = "red",
        colour = "red", linetype = "solid"),
    legend.background = element_rect(colour = "red")) +labs(x = "RAIN FALL (in mm)", colour = "ORIGINAL DATA IS RED",
    fill = NULL) 


ggplot(data = regr_model_1, aes(x = rain_mean, y = Onions))+
  geom_point(data = regr_model_1,color="green")+
  stat_smooth(method = 'lm', col = 'red')+
  stat_smooth(method = 'lm',aes(x = rain_mean),colour = 'blue', data = itr_food_rain$imputations$imp2)+
  stat_smooth(method = 'lm',aes(x = rain_mean),colour = 'violet', data = itr_food_rain$imputations$imp10)+
  labs(x= "rain fall",
       y="onion price")+
  ggtitle("comparing between rain mean and oninos",
          subtitle = "original and imputed data")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

# LINEAR MODEL FOR TESTING MUSTARD OIL PRICE

test_must = itr_food_rain$imputations$imp10

mustard_oil = lm(Mustard_oil ~ rain_mean , data = test_must)
summary(mustard_oil)


f = 1
test_must_1 = test_must %>%
  mutate(rain_mean = lag(rain_mean, n = f))%>%
  mutate(rain_category = lag(rain_category, n = f))

mustard_oil_1 = lm(Mustard_oil ~ rain_mean , data = test_must_1)
summary(mustard_oil_1)

f = 1
test_must_2 = test_must_1 %>%
  mutate(rain_mean = lag(rain_mean, n = f))%>%
  mutate(rain_category = lag(rain_category, n = f))

mustard_oil_2 = lm(Mustard_oil ~ rain_mean , data = test_must_2)
summary(mustard_oil_2)

f = 1
test_must_3 = test_must_2 %>%
  mutate(rain_mean = lag(rain_mean, n = f))%>%
  mutate(rain_category = lag(rain_category, n = f))

mustard_oil_3 = lm(Mustard_oil ~ rain_mean , data = test_must_3)
summary(mustard_oil_3)

# DECISION TREE FOR LAG ONION REGRESSION TEST
# Build the specification
#splitting data into train data test data
data_1 = itr_food_rain$imputations$imp2
head(data_1)

set.seed(1)
price_pred_split = initial_split(data_1, strata = Onions)

onion_train = training(price_pred_split)
onion_test  = testing(price_pred_split)
testing_Onion = onion_test$Onions

tree_model = tree(Onions ~ rain_mean, onion_train)
tree_model

# PLOT TREE MODEL
par(mar=c(1,1,1,1))
plot(tree_model)
text(tree_model, pretty = 0)

# Predict new data
price_pred_onion_num = predict(tree_model,onion_test)
price_pred_onion = predict(tree_model,onion_test)%>% 
  bind_cols(onion_test)

#checking the accuracy of the model using testing data
# MEAN SQUARE ERROR (MSE)
MSE = mean((price_pred_onion_num - testing_Onion)^2)
MSE
#ROOT MEAN SQUARE ERROR
RMSE = sqrt(MSE)
RMSE

# TO CHECK THE SUITABLE SIZE OF THE TREE 
#CROSS VALIDATION FOR PRUNING THE TREE

cv_tree = cv.tree(tree_model)
par(mar=c(4.5,4,3,2))
plot(cv_tree$size, cv_tree$dev,type = "b", xlab= "tree size",
     ylab ="MSE")

which.min(cv_tree$dev)
cv_tree$size[5]

# Checking to see if pruning helps

pruned_model = prune.tree(tree_model, best = 2)
plot(pruned_model)
text(pruned_model)

# check the accuracy
pruned_pred = predict(pruned_model, onion_test)
MSE_2 = mean((pruned_pred - testing_Onion)^2)

RMSE_2 = sqrt(MSE_2)
RMSE_2

