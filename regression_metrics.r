
#------------------------------------------------------------------------------------------------
# name: classification_metrics.r
# author: Chad Morgan
# purpose: basic model assessment metrics for regression algorithms
#
#------------------------------------------------------------------------------------------------


# test.response: Testing dataset response vector (i.e. the value we want to predict)
#       (this should NOT be the same data the regression was trained on)

# test.pred_response: Predicted testing dataset labels

# model.p: number of model parameters, including intercept



# ------ Error variance based metrics ------ #

# model errors or residuals
residuals = data.frame(test.response - test.pred_response)

# sum of squared errors
sse = sum(residuals^2)

# mean square error or error variance
mse = sse / dim(test.response)[1]
# NOTE: we use n as the variance denominator rather than n-1. It is critical to use cross-validation, becuase otherwise this measure of MSE will be biased to small

# residual standard error OR root mean square error
rmse = sqrt(mse)

# R-squared: proportion of variance explained by model
r.sqr = 1-(sse / sum((mean(test.response[,1])-test.response[,1])^2))



# ------ Absolute deviation based metrics ------ #

# mean absolute deviations
mad = sum(abs(test.response-test.pred_response)) / dim(test.response)[1]

# mean absolute percentage error
mape = sum(abs(test.response-test.pred_response) / test.response) / dim(test.response)[1]



# ------ Outlier and leverage detection ------ #

# standardized residuals
z.resid <-  (residuals-mean(residuals[,1]))/sd(residuals[,1])
colnames(z.resid) <- 'z.resid'

# mark outliers
z.resid$outlier <- factor(ifelse(abs(z.resid[,1])>3,1,0))

# plot outliers
library(ggplot2)
resid.dat <- data.frame(cbind(test.response,test.pred_response,residuals,z.resid))
ggplot(data=resid.dat,aes(x=test.pred_response,y=z.resid,color=outlier))+geom_point()

# mahalanobis distance (leverage measure) for training data train.x

mahal.dist <- data.frame(mahalanobis(train.x,colMeans(train.x),cov(train.x)))
colnames(mahal.dist)<-'mahal.dist'

qqplot(qchisq(ppoints(dim(train.x)[1]), df = dim(train.x)[2]),mahal.dist[,1])
qqline(mahal.dist[,1], distribution = function(p) qchisq(p, df = dim(train.x)[2]))






