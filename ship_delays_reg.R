# Data Analysis

#library(openxlsx)

#set working directory (modify path as needed)
setwd("D:/Automation/Use Cases/Shipment Delays")
rm(list=ls())

# Load Data
X = read.csv(file='rcvd_data.csv')

# Variable transformations as required
X$SHIPMENT_AMOUNT = as.numeric(X$SHIPMENT_AMOUNT)

X$BOOKED_DATE =  as.POSIXct(X$BOOKED_DATE, format = "%m-%d-%Y %H:%M:%S",tz='EST') 
X$ROUTE_DERIVED =  as.POSIXct(X$ROUTE_DERIVED, format = "%m-%d-%Y %H:%M:%S",tz='EST')
X$MFG_DATE = as.POSIXct(X$MFG_DATE, format = "%m-%d-%Y %H:%M:%S",tz='EST')
X$CM_SSD = as.POSIXct(X$CM_SSD, format = "%m-%d-%Y %H:%M:%S",tz='EST')
X$PACKOUT_DATE  = as.POSIXct(X$PACKOUT_DATE, format = "%m-%d-%Y %H:%M:%S",tz='EST')
X$DELIVERY_CREATED  = as.POSIXct(X$DELIVERY_CREATED, format = "%m-%d-%Y %H:%M:%S",tz='EST')
X$PICK_RELEASED  = as.POSIXct(X$PICK_RELEASED, format = "%m-%d-%Y %H:%M:%S",tz='EST')
X$EXPECTED_SLC_SD  = as.POSIXct(X$EXPECTED_SLC_SD, format = "%m-%d-%Y %H:%M:%S",tz='EST')
X$ACTUAL_SLC_SD  = as.POSIXct(X$ACTUAL_SLC_SD, format = "%m-%d-%Y %H:%M:%S",tz='EST')
X$EXPECTED_DELIVERY_DATE=as.POSIXct(X$EXPECTED_DELIVERY_DATE,format="%m-%d-%Y %H:%M:%S",tz='EST')
X$DELIVERED_DATE  = as.POSIXct(X$DELIVERED_DATE, format = "%m-%d-%Y %H:%M:%S",tz='EST')

# Putting all data in X_all 
X_all=X


# Overall Summary of Data
names(X)
summary(X)

# Subsetting required columns
summary(X$ORDER_TYPE)
summary(X$SHIP_TO)
summary(X$SHIPPING_METHOD_CODE)
summary(X$OPT_IN_OUT)
summary(X$OTM_IB_SHIPPING_ROUTE_CODE)
summary(X$PICK_RELEASED)
summary(X$EXPECTED_SLC_SD)
summary(X$ACTUAL_SLC_SD)
reqd_list = c('ORDER_TYPE','SHIP_TO','SHIPPING_METHOD_CODE','OPT_IN_OUT',
              'OTM_IB_SHIPPING_ROUTE_CODE','PICK_RELEASED', 'EXPECTED_SLC_SD', 'ACTUAL_SLC_SD')
X_sub = X[ , which(names(X) %in% reqd_list)]
head(X_sub)
summary(X_sub)

# Some Data Cleaning

# Remove the first row with 2014 values
X_sub = X_sub[-1,]

# Remove actual SLC rows with blank(NA) values
idxna = which(is.na(X_sub$ACTUAL_SLC_SD)!=0)
X_sub = X_sub[-idxna,]

X_sub = X_sub[order(X_sub[,6]),] # order by Pick Release Date
head(X_sub)
tail(X_sub)

# Remove non Standard Order Types
summary(X_sub$ORDER_TYPE)
valid_ot = c('Standard Infosys - AUS', 'Standard Infosys - CAN',  'Standard Infosys - CN', 'Standard Infosys - IN', 
'Standard Infosys - ITL', 'Standard Infosys - JPN', 'Standard Infosys - NL',  
'Standard Infosys - PY', 'Standard Infosys - RUS',  'Standard Infosys - UKH',  
'Standard Infosys - US')
X_sub = X_sub[X_sub$ORDER_TYPE %in% valid_ot,]

# Remove low volume Shipping Method Codes
summary(X_sub$SHIPPING_METHOD_CODE)
X_sub = X_sub[!(as.numeric(X_sub$SHIPPING_METHOD_CODE) %in% which(table(X_sub$SHIPPING_METHOD_CODE)<5)),]

# Remove low volume Ship To values
summary(X_sub$SHIP_TO)
X_sub = X_sub[!(as.numeric(X_sub$SHIP_TO) %in% which(table(X_sub$SHIP_TO)<=5)),]


# Find number of minutes taken from Pick Release to SLC_SD
X_sub$EXP_PICKTOSLC= as.numeric(X_sub$EXPECTED_SLC_SD - X_sub$PICK_RELEASED)
X_sub$ACT_PICKTOSLC= as.numeric(X_sub$ACTUAL_SLC_SD - X_sub$PICK_RELEASED)


#A. Create Regression model to predict number of minutes to SLC_SD
# Create training and test data sets
summary(X_sub)
reqd_list2 = c('ORDER_TYPE','SHIP_TO','SHIPPING_METHOD_CODE','OPT_IN_OUT',
                 'OTM_IB_SHIPPING_ROUTE_CODE','ACT_PICKTOSLC')

X= X_sub[ , which(names(X_sub) %in% reqd_list2)]
Y= X$ACT_PICKTOSLC  # column for prediction
Y_EXP=X_sub$EXP_PICKTOSLC # To compare against expected
train=1:41000 # approx 80% of data

test.X=X[-train,]
test.Y=Y[-train]
test.Yexp = Y_EXP[-train]

# A1. Linear Regression

lm.fit=lm(ACT_PICKTOSLC~. , data=X, subset = train)
#lm.fit
#summary(lm.fit)

#Prediction
output.lm=predict( lm.fit, test.X )
#output.lm1 = cbind(test.X$ACT_PICKTOSLC,output.lm)

# Accuracy
# RMSE
cat( " MSE - Regression prediction vs actuals " ,mean((test.X$ACT_PICKTOSLC - output.lm)^2))
cat( " MSE - Expected SLC vs actuals " ,mean((test.X$ACT_PICKTOSLC - test.Yexp)^2))


#RMSPE
# Expected vs Actuals
e1 = (test.X$ACT_PICKTOSLC - test.Yexp)/test.X$ACT_PICKTOSLC
e2=sum(e1^2)/length(test.X$ACT_PICKTOSLC)
rmspe_exp=sqrt(e2)*100

r1 = (test.X$ACT_PICKTOSLC - output.lm)/test.X$ACT_PICKTOSLC
r2=sum(r1^2)/length(test.X$ACT_PICKTOSLC)
rmspe_reg=sqrt(r2)*100

rmspe_lm = rmspe_reg
cat( " RMSPE Linear Regression " , rmspe_lm)


mean(X_sub$EXP_PICKTOSLC / X_sub$ACT_PICKTOSLC)

# A2. MARS ( Regression Splines)

library(earth)

# fit model
mars.fit = earth(ACT_PICKTOSLC~. , data=X, subset = train)
# summarize the fit
summary(mars.fit)
# summarize the importance of input variables
evimp(mars.fit)
# make predictions
output.mars = predict(mars.fit, test.X)
#rbind(y$Prediction,t(output.mars))

# Accuracy
# RMSE
cat( " MSE - Regression prediction vs actuals " ,mean((test.X$ACT_PICKTOSLC - output.mars)^2))
cat( " MSE - Expected SLC vs actuals " ,mean((test.X$ACT_PICKTOSLC - test.Yexp)^2))

#RMSPE

r1 = (test.X$ACT_PICKTOSLC - output.mars)/test.X$ACT_PICKTOSLC
r2=sum(r1^2)/length(test.X$ACT_PICKTOSLC)
rmspe_reg=sqrt(r2)*100

rmspe_mars = rmspe_reg
cat( " RMSPE MARS " , rmspe_mars)

# A3. NEURAL NETWORK

library(nnet)
# fit model
nn.fit = nnet(ACT_PICKTOSLC~., data=X, subset = train, size=4, maxit=500000, linout= T, decay=0.01)
# make predictions
output.nn = predict(nn.fit, test.X, type="raw")# summarize the fit

# Accuracy
# RMSE
cat( " MSE - Regression prediction vs actuals " ,mean((test.X$ACT_PICKTOSLC - output.nn)^2))
cat( " MSE - Expected SLC vs actuals " ,mean((test.X$ACT_PICKTOSLC - test.Yexp)^2))

#RMSPE

r1 = (test.X$ACT_PICKTOSLC - output.nn)/test.X$ACT_PICKTOSLC
r2=sum(r1^2)/length(test.X$ACT_PICKTOSLC)
rmspe_reg=sqrt(r2)*100

rmspe_nn = rmspe_reg
cat( " RMSPE NN " , rmspe_nn)


# Handling level count problem for Decision Tree and Random Forest 
      #- identify columns with high factors
names(X)
X1=X
sort(table(X1$ORDER_TYPE))#ok 
sort(table(X1$OPT_IN_OUT))#ok
sort(table(X1$SHIPPING_METHOD_CODE))
sort(table(X1$SHIP_TO))
sort(table(X1$OTM_IB_SHIPPING_ROUTE_CODE))#ok

X1 = X1[!(as.numeric(X1$SHIPPING_METHOD_CODE) %in% which(table(X1$SHIPPING_METHOD_CODE)<40)),]
X1 = X1[!(as.numeric(X1$SHIP_TO) %in% which(table(X1$SHIP_TO)<70)),]
X1[] = lapply(X1, function(x) if(is.factor(x)) factor(x) else x) # remove redundant levels


Y1= X1$ACT_PICKTOSLC  # column for prediction
train1=1:41000 # approx 80% of data
test.X1=X1[-train,]
test.Y1=Y1[-train]

# Check that levels of the 2 columns are less than 30 for tree based algorithms
levels(X1$SHIPPING_METHOD_CODE)
levels(X1$SHIP_TO)

# A4. DECISION TREE

library(tree)

# fit model
tree.fit = tree(ACT_PICKTOSLC~., data=X1, subset = train1)
# make predictions
output.tree = predict(tree.fit, test.X1)# summarize the fit
plot(tree.fit)
text(tree.fit,pretty=0)


# Accuracy
# RMSE
cat( " MSE - Regression prediction vs actuals " ,mean((test.X1$ACT_PICKTOSLC - output.tree)^2))

#RMSPE

r1 = (test.X1$ACT_PICKTOSLC - output.tree)/test.X1$ACT_PICKTOSLC
r2=sum(r1^2)/length(test.X1$ACT_PICKTOSLC)
rmspe_reg=sqrt(r2)*100
rmspe_tree = rmspe_reg
cat( " RMSPE Decision Tree " , rmspe_tree)

# A5. RANDOM FOREST

library(randomForest)


# fit model
rf.fit=randomForest(ACT_PICKTOSLC~., data=X1, subset = train1, mtry=5, importance=TRUE)  
# make predictions
output.rf = predict(rf.fit, test.X1)# summarize the fit
plot(rf.fit)

# Accuracy
# RMSE
cat( " MSE - Regression prediction vs actuals " ,mean((test.X1$ACT_PICKTOSLC - output.rf)^2))

#RMSPE

r1 = (test.X1$ACT_PICKTOSLC - output.rf)/test.X1$ACT_PICKTOSLC
r2=sum(r1^2)/length(test.X1$ACT_PICKTOSLC)
rmspe_reg=sqrt(r2)*100
rmspe_rf = rmspe_reg
cat( " RMSPE Random Forest " , rmspe_rf)


# A6. MEAN BASED MODEL
mean_table = aggregate(X1$ACT_PICKTOSLC, FUN=mean, 
          by=list(ORDER_TYPE = X1$ORDER_TYPE, OPT_IN_OUT = X1$OPT_IN_OUT, SHIPPING_METHOD_CODE = X1$SHIPPING_METHOD_CODE , 
                  SHIP_TO = X1$SHIP_TO, 
                  OTM_IB_SHIPPING_ROUTE_CODE = X1$OTM_IB_SHIPPING_ROUTE_CODE))
class(mean_table)
names(mean_table)

sum(test.X1[1,1:5]==test.X1[3,1:5])

head(test.X1)
test_count = dim(test.X1)[1]
mean_table_count = dim(mean_table)[1]
output.mean=rep(0,test_count)

# Warning : This is a bit slow
for (rowt in 1:test_count){
  if (rowt%%100==0) {
  cat('\nRow',rowt)
    }
  
  for (rowm in 1:mean_table_count){
    #print(sum(test.X1[rowt,1:5]==mean_table[rowm,1:5])== 5)
    #cat('rowm',rowm)
    if (sum(test.X1[rowt,1:5]==mean_table[rowm,1:5])== 5){
      output.mean[rowt]=mean_table[rowm,6]
      break
    }
  }
}

r1 = (test.X1$ACT_PICKTOSLC - output.mean)/test.X1$ACT_PICKTOSLC
r2=sum(r1^2)/length(test.X1$ACT_PICKTOSLC)
rmspe_reg=sqrt(r2)*100
rmspe_mean = rmspe_reg
cat( " RMSPE Mean Based Model " , rmspe_mean)



