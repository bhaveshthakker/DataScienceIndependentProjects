#Remove all env variables

rm(list = ls(all.names = T)) #Clear all variables



#Trying to recompile to remove error of package class was built before 3.0.0

install.packages('codetools')

update.packages(checkBuilt = TRUE, ask = FALSE)

install.packages("shiny")



#Install required packages

install.packages("fmsb") #VIF

install.packages("DMwR") #Data Mining

install.packages("outliers")

install.packages("pROC") #Receiver operating curve

install.packages("ROCR")

install.packages("aod") #Wald-test

install.packages("ResourceSelection") #hosmer-lemeshow test

install.packages("caret")

install.packages("XLConnect")

install.packages("pbkrtest")

install.packages('e1071', dependencies=TRUE) #Naive Bayes and Also for SVM

install.packages("pROC")

install.packages('car', dependencies = TRUE) 



#Load the data from packages

library(pROC)

library(outliers)

library(fmsb)

library(ResourceSelection)

#library(vif)

library(e1071)



#To solve the error of vif

setRepositories()

detach("package:usdm", unload=TRUE)

library(car)

#??vif



CTR_SD_Data <- read.csv("/home/raghunandangupta/Downloads/splits/sub-splitaa")

View(CTR_SD_Data)



#Vector to numeric converion

str(CTR_SD_Data)  #Gives the datatype of each column

CTR_SD_Data$site_id = as.numeric(CTR_SD_Data$site_id)

str(CTR_SD_Data)

CTR_SD_Data$site_domain = as.numeric(CTR_SD_Data$site_domain)

CTR_SD_Data$site_category = as.numeric(CTR_SD_Data$site_category)

CTR_SD_Data$app_id = as.numeric(CTR_SD_Data$app_id)

CTR_SD_Data$app_domain = as.numeric(CTR_SD_Data$app_domain)

CTR_SD_Data$app_category = as.numeric(CTR_SD_Data$app_category)

CTR_SD_Data$device_id = as.numeric(CTR_SD_Data$device_id)

CTR_SD_Data$device_ip = as.numeric(CTR_SD_Data$device_ip)

CTR_SD_Data$device_model = as.numeric(CTR_SD_Data$device_model)

CTR_SD_Data$site_id=as.numeric(CTR_SD_Data$site_id)

str(CTR_SD_Data)



#Split the data in training and test data

rows = seq(from=1,to=nrow(CTR_SD_Data), by = 1)

rows

train_rows = sample(x=rows, size=(0.7 * nrow(CTR_SD_Data))) #selecting 70% random sample no of row no as training data

train_rows

TrainData = CTR_SD_Data[train_rows,] #Getting training data i.e. selecting all rows that we had randomly selected from rows

TestData = CTR_SD_Data[-train_rows,] #Getting TEST data i.e. all rows not mentioned in train rows





str(TrainData)

#Check for ccorelation and multicollinearity

#multicollinearity_matrix = cor(TrainData[-1])

multicollinearity_matrix = cor(TrainData)#Discarding 1st column while checking ulticollinearitty

View(multicollinearity_matrix)

write.csv(multicollinearity_matrix, file="/home/raghunandangupta/Downloads/splits/sub-splitaaaa" )



vif = vif(TrainData[-2]) #calculates vif

vifcor(TrainData[-1], th=0.8) #Drops one of the column which is corelated



#Building Logistic Regression

log_reg_2 = lm(formula = click~TrainData$C1+TrainData$banner_pos+TrainData$app_domain+TrainData$app_id+TrainData$site_category+TrainData$C15+TrainData$C15+TrainData$C16+TrainData$C17+TrainData$C18+TrainData$C19+TrainData$C20+TrainData$C21, data=TrainData) #glm = generlaized linera model

log_reg_2

summary(log_reg_2) #AIC is error


