# Load statistical packages
library(pls)

# Load data
data(longley)
# Examine structure and variable types
str(longley)

# View first few lines
head(longley)

# Summarize the data set
summary(longley)

# Visualize the data set 
x<-longley[ ,1:7]

par(mfrow=c(1,7))
for(i in 1:7) {
  boxplot(x[,i], main=names(longley)[i])
}


# Ordinary Least Squares Regression
#Fit model
olsFit<-lm(Employed ~.,longley)

# Summarize the fit
summary(olsFit)

# Make predictions
olsPredictions<-predict(olsFit, longley)

# Summarize accuracy
olsMSE<-mean((longley$Employed - olsPredictions)^2)
print(olsMSE)

# Stepwize Linear Regression
# Fit model
base<-lm(Employed ~., longley)

# Summarize the fit
summary(base)

# Perform step-wise feature selection
slrFit<-step(base)

# Summarize selected model
summary(slrFit)

# Make predictions
slrPredictions<-predict(slrFit, longley)

# Summarize accuracy
slrMSE<-mean((longley$Employed - slrPredictions)^2)
print(slrMSE)

#Principal Component Regression
# Fit model
pcrFit<-pcr(Employed ~ ., data = longley, valdiation = "cv")

# Summarize the fit
summary(pcrFit)
# Make predictions
pcrPredictions<-predict(pcrFit, longley, ncomp = 6)

# Summarize accuracy
pcrMSE<-mean((longley$Employed - pcrPredictions)^2)
print(pcrMSE)

#Partial Least Squares Regression
# Fit model
plsFit<-plsr(Employed ~., data = longley, validation = "CV")

# Summarize the fit
summary(plsFit)
# Make predictions
plsPredictions<-predict(plsFit, longley, ncomp = 6)

#Summarize acuracy
plsMSE<-mean((longley$Employed - plsPredictions)^2)
print(plsMSE)

