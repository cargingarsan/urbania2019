
library(mice)
data <- all |> select(-c(SalePrice))
head(data)

# Perform the mice imputation
init = mice(all, maxit=0) 
meth = init$method
predM = init$predictorMatrix

# You can set the method for the variables you want to impute
# For example, if you want to impute 'GarageYrBlt' with 'mean'
meth['GarageYrBlt'] = 'mean'

# Now perform the mice imputation with the specified methods
imp = mice(all, method=meth, predictorMatrix=predM, m=5)

# You can check the imputed data
completedData = complete(imp)



########################

library(VIM)

aggr(data)

nrow(na.omit(data))
data

data_imp <- mice(data)

attributes(data_imp)

#complete

data_complete <- complete(data_imp)

aggr(data_complete)

plot(density(data$LotFrontage, na.rm = TRUE)) 
lines(density(data_complete$LotFrontage), col="red", lty=3, lwd=2)

#############################
aggr(penguins)
library(missForest)

data_imp_peng <- missForest(as.data.frame(penguins))

table(penguins$sex)

table(data_imp_peng$ximp$sex)  
