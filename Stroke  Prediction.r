# load library
library(dplyr) 
library(tidyverse)
 
library(corrplot) ## Correlation matrix plot library
library(PerformanceAnalytics)
library(corrplot)
library(Amelia)##check missing value libaray

# load Dataset
# read data from csv
df<-read.csv('C:/Users/natu/Downloads/healthcare-dataset-stroke-data.csv', header = TRUE)
df

dim(df)

summary(df)

missmap(df, main = "Missing values vs observed")


unique(df$stroke)
unique(df$gender)
unique(df$hypertension)
unique(df$heart_disease)
unique(df$ever_married)
unique(df$work_type)
unique(df$Residence_type)
unique(df$smoking_status)

table(df$gender)
table(df$smoking_status)
table(df$work_type)
length(df$bmi[df$bmi == "N/A"])

# remove outlier values
df_clean <- df %>% filter(df$gender!='Other')
#df_clean = df_clean %>% filter(df_clean$work_type!='Never_worked')
df_clean = df_clean %>% filter(df_clean$bmi!='N/A')

# convert chr to factors
df_clean$gender = as.factor(df_clean$gender)
df_clean$ever_married = as.factor(df_clean$ever_married)
df_clean$work_type = as.factor(df_clean$work_type)
df_clean$Residence_type = as.factor(df_clean$Residence_type)
df_clean$smoking_status = as.factor(df_clean$smoking_status)
df_clean$bmi = as.numeric(df_clean$bmi)

# check gender vs stroke

## check by graph
 ggplot(df_clean, aes(x=gender, fill=as.factor(stroke)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Gender Group, %') +
  scale_y_continuous(labels = scales::percent)


## check by statistical analysis chisq-test
table(data.frame(gender=df_clean$gender, stroke=df_clean$stroke))
chisq.test(df_clean$gender, df_clean$stroke)
        



# check hypertesion vs stroke
ggplot(df_clean, aes(x=as.factor(hypertension), fill=as.factor(stroke)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent)

table(data.frame(hypertension=df_clean$hypertension, stroke=df_clean$stroke))
chisq.test(df_clean$hypertension, df_clean$stroke)
# has correlation

# check heart_disease vs stroke
ggplot(df_clean, aes(x=as.factor(heart_disease), fill=as.factor(stroke)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(gender=df_clean$heart_disease, stroke=df_clean$stroke))
chisq.test(df_clean$heart_disease, df_clean$stroke)

# check ever_married vs stroke
ggplot(df_clean, aes(x=as.factor(ever_married), fill=as.factor(stroke)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent)
table(df_clean$ever_married, df_clean$stroke)
chisq.test(df_clean$ever_married, df_clean$stroke)
# has correlation

# check worktye vs stroke
ggplot(df_clean, aes(x=as.factor(work_type), fill=as.factor(stroke)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent)
table(df_clean$work_type, df_clean$stroke)
chisq.test(df_clean$work_type, df_clean$stroke)
# has correlation

# check Residence_type vs stroke
ggplot(df_clean, aes(x=as.factor(Residence_type), fill=as.factor(stroke)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent)
table(df_clean$Residence_type, df_clean$stroke)
chisq.test(df_clean$Residence_type, df_clean$stroke)

# check Residence_type vs stroke
ggplot(df_clean, aes(x=as.factor(smoking_status), fill=as.factor(stroke)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent)
table(df_clean$Residence_type, df_clean$stroke)
chisq.test(df_clean$smoking_status, df_clean$stroke)

#check avg_glucose_level vs stroke
ggplot(df_clean, aes(x=as.factor(stroke), y=avg_glucose_level, fill=as.factor(stroke))) + 
  geom_boxplot() + 
  labs(subtitle="avg_glucose_level by group stroke")
lm.stroke_glucose <- lm(df_clean$stroke ~ df_clean$avg_glucose_level)
summary(lm.stroke_glucose)

#check avg_glucose_level vs stroke
ggplot(df_clean, aes(x=as.factor(stroke), y=age, fill=as.factor(stroke))) + 
  geom_boxplot() + 
  labs(subtitle="age by group stroke")
lm.stroke_age <- lm(df_clean$stroke ~ df_clean$age)
summary(lm.stroke_age)

#check avg_glucose_level vs stroke
ggplot(df_clean, aes(x=as.factor(stroke), y=bmi, fill=as.factor(stroke))) + 
  geom_boxplot() + 
  labs(subtitle="bmi by group stroke")
lm.stroke_bmi <- lm(df_clean$stroke ~ df_clean$bmi)
summary(lm.stroke_bmi)

library(corrplot)

#Data preprocessing
df_num <- data.frame(df_clean)
df_num$ever_married = str_replace_all(df_num$ever_married, c("Yes"="1", "No"="0"))
df_num$ever_married = as.numeric(df_num$ever_married)

df_num$gender = str_replace_all(df_num$gender, c("Male"="1", "Female"="2"))
df_num$gender = as.numeric(df_num$gender)

df_num$work_type = str_replace_all(df_num$work_type, c("Never_worked"="0","children"="1", "Private"="2", "Self-employed"="3", "Govt_job"="4"))
df_num$work_type = as.numeric(df_num$work_type)

df_num$Residence_type = str_replace_all(df_num$Residence_type, c("Rural"="1", "Urban"="2"))
df_num$Residence_type = as.numeric(df_num$Residence_type)

df_num$stroke = as.numeric(as.character(df_num$stroke))

df_num$smoking_status = as.numeric(df_num$smoking_status)

drop_smoke<-c("smoking_status","id")
df_num <- df_num[,!(names(df_num) %in% drop_smoke)]


res <- cor(df_num)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# create training or test for neural network without missing features

# remove useless features
drop <- c("id","gender", "Residence_type")
df_num = df_num[,!(names(df_num) %in% drop)]

# shuffle index
set.seed(42)
rows<-sample(nrow(df_num))
df_shuffled<-df_num[rows,]

# split Train data and test data
train <- df_shuffled[1:4000,]
test <- df_shuffled[4001:4908,]

# apply model
model <- glm(stroke ~., family=binomial(link='logit'), data=train)
summary(model)
anova(model, test="Chisq")

# accuracy
fitted.results <- predict(model,newdata=subset(test,select=c(1,2,3,4,5,6,7)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$stroke)
print(paste('Accuracy',1-misClasificError))

96.475% of a patient is likely to get stroke  based on the categorical variables on the datasset


