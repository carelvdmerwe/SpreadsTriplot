function(newconf)
{
  Metrics <- matrix(rep(0,dim(newconf)[2]*17),ncol=dim(newconf)[2],byrow=T)
  for (i in 1:dim(newconf)[2])
  {
    if(dim(newconf)[1]-1 == dim(newconf)[2])  confmat <- matrix(c(newconf[i,i],sum(newconf[i,-i]),sum(newconf[-i,i])-newconf[ncol(newconf)+1,i],sum(newconf[-i,-i])-sum(newconf[ncol(newconf)+1,-i]),newconf[ncol(newconf)+1,i],sum(newconf[ncol(newconf)+1,-i])),ncol=2,nrow=3,byrow=T)
    if(dim(newconf)[1] == dim(newconf)[2])  confmat <-   matrix(c(newconf[i,i],sum(newconf[i,-i]),sum(newconf[-i,i])                           ,sum(newconf[-i,-i])),ncol=2,nrow=2,byrow=T)
    if(abs(dim(newconf)[1] - dim(newconf)[2])>1) print("confusion matrix not correct!")
    #Prevalance = sum actual positive / total population
    Metrics[1,i] <- sum(confmat[,1])/sum(confmat)
    #Accuracy (ACC) = sum true positive + sum true negative / total population
    Metrics[2,i] <- (confmat[1,1] + confmat[2,2]) / sum(confmat)
    #Positive Predicted value / Precision = sum true positive / sum predicted positive
    Metrics[3,i] <- confmat[1,1] / sum(confmat[1,])
    #False discovery rate (FDR) = sum false positive / sum predicted positive 
    Metrics[4,i] <- confmat[1,2] / sum(confmat[1,])
    #False omission rate (FOR) = sum false negative / sum predicted negative
    Metrics[5,i] <- confmat[2,1] / sum(confmat[2,])
    #Negative predicted value (NPV) = sum true negative / sum predicted negative 
    Metrics[6,i] <- confmat[2,2] / sum(confmat[2,])
    #True positive rate (TPR) / Recall / Sensitivity / prob of detection = sum true positive / sum actual positive
    Metrics[7,i] <- confmat[1,1] / sum(confmat[,1])
    #False positive rate (FPR) / Fall-out / prob of false alarm = sum false positive / sum actual negative
    Metrics[8,i] <- confmat[1,2] / sum(confmat[,2])
    #False negative rate (FNR) / Miss-rate = sum false negative / sum actual positive
    Metrics[9,i] <- confmat[2,1] / sum(confmat[,1])
    #Specificity / selectivity / true negative rate (TNR) = sum true negative / sum actual negative 
    Metrics[10,i] <- confmat[2,2] / sum(confmat[,2])
    #Positive Likelihood ratio (LR+)= TPR / FPR
    Metrics[11,i] <-  Metrics[7,i] /  Metrics[8,i]
    #Negative likelihood ratio (LR-) = FNR / TNR
    Metrics[12,i] <- Metrics[9,i] /  Metrics[10,i] 
    #Diagnostic odds ratio (DOR) = LR+/LR-
    Metrics[13,i] <-  Metrics[11,i] /  Metrics[12,i]
    #F1 Score = 2 / (1/recall + 1/precision)
    Metrics[14,i] <- 2 / (1/Metrics[7,i] + 1/Metrics[3,i])
    }
  tempmet <- cbind(apply(Metrics, 1, function(x) mean(x,na.rm=TRUE)),Metrics) 
  tempmet[11,1] <-  tempmet[7,1] /  tempmet[8,1]
  tempmet[12,1] <- tempmet[9,1] /  tempmet[10,1] 
  tempmet[13,1] <-  tempmet[11,1] /  tempmet[12,1]
  tempmet[14,1] <- 2 / (1/tempmet[7,1] + 1/tempmet[3,1]) 
  tempmet[15,1] <- sum(diag(newconf))/sum(newconf)
  if(dim(newconf)[1]-1 == dim(newconf)[2]) tempmet[16,1] <- sum(newconf[dim(newconf)[1],]) / sum(newconf)
  tempmet[17,1] <- 1- tempmet[15,1]-tempmet[16,1]
  return(tempmet[,1])
}
