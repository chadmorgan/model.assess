
#------------------------------------------------------------------------------------------------
# name: classification_metrics.r
# author: Chad Morgan
# purpose: basic model assessment metrics for classification algorithms
#
#   Note: these metrics are for binary classification only
#------------------------------------------------------------------------------------------------


# test.labels: Testing dataset label vector (0/1 encoding)
#       (this should NOT be the same data the classifier was trained on)

# test.pred_labels: Predicted testing dataset labels
# test.pred_prob: Predicted probabilities for testing data

# model.p: number of model parameters, including intercept



# ------ Likelihood-based measures ------ #

# Binomial log likelihood (larger is better)
log.like = sum( test.labels*log(test.pred_prob) + (1-test.labels)*log(1-test.pred_prob) )

# deviance or -2*log likelihood (smaller is better)
deviance = -2*log.like

# Aikake's information criterion (smaller is better)
aic = deviance + 2*model.p

# Bayesian information criterion (smaller is better)
bic = deviance + log(dim(test.labels)[1])*model.p

# log-loss / n (smaller is better)
log.loss = -log.like/dim(test.labels)[1]



# ------ Confusion matrix based measures ------ #

# confusion matrix (more obs on the diagonal is better)
conf.mat <- table(x=test.labels[,1],y=test.pred_labels[,1])

# classification accuracy (larger is better)
accuracy = (conf.mat[1,1]+conf.mat[2,2]) / sum(conf.mat)

# precision and recall (larger is better for both)
recall = conf.mat[2,2]/(conf.mat[2,2]+conf.mat[2,1])
precision = conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2])

# F1 score: harmonic mean of precision and recall
F1_score = 2*(recall*precision)/(recall+precision)



# ------ Area Under the Curve (AUC) ------ #

# i.e. the probability that a randomly chosen positive case has a larger score
#       than a randomly chosen negative case (larger is better)

AUC = mean(sample(test.pred_prob[test.labels==1],100000,replace=TRUE)>sample(test.pred_prob[test.labels==0],100000,replace=TRUE) )



# ------ Probability calibration ------ #

# get observed probability by predicted probability group
calibration.table <- data.frame(aggregate(x=test.labels,by=list(round(test.pred_prob,1)),FUN=mean))
colnames(calibration.table)<-c('predicted.prob','observed.prob')

# count number of observations by predicted probability group
count <- data.frame(aggregate(x=test.labels,by=list(round(test.pred_prob,1)),FUN=length))
colnames(count)<-c('predicted.prob','count')

# merge count onto observed rate
calibration.table <- merge(x=calibration.table,y=count,by='predicted.prob')

# calibration plot (points closer to diagonal are better)
library(ggplot2)
ggplot(data=calibration.table,aes(x=predicted.prob,y=observed.prob))+geom_point(aes(x=predicted.prob,y=observed.prob,size=count))+geom_line(aes(x=predicted.prob,y=predicted.prob))

# calibration score (smaller is better)
calibration.score = sum(calibration.table$count*(calibration.table$predicted.prob-calibration.table$observed.prob)^2) / sum(calibration.table$count)

# discrimination score (larger is better)
discrimination.score = sum(calibration.table$count*(mean(calibration.table$observed.prob)-calibration.table$observed.prob)^2) / sum(calibration.table$count)

