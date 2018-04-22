masomenos.train = function(X,   # pxn data frame of predictors
                           Y,   # nx1 vectors of labels or responses
                           P,   # number of predictors
                           training.criterion="AUC",
                           filtering.fraction=.5)
{
  # eliminate unclassified samples
  YY = Y[!is.na(Y)]
  XX = X[,!is.na(Y)]
  #
  
  Nvar = nrow(XX)
  crite = rep(NA,Nvar)
  if (training.criterion=="AUC"){
    for (pp in 1:Nvar){
      crite[pp] = as.numeric( wilcox.test(XX[pp,]~YY)$statistic / (sum(YY==0)*sum(YY==1)) )
    }
  }
  cutoff = sort(abs(crite),decreasing = TRUE)[P]
  variables = (1:Nvar)[abs(crite) >= cutoff]
  variables.signs = ( 2 * ( crite > 0 ) - 1 ) [variables]
  scores = apply ( XX[ variables, ] * variables.signs, 2, mean )
  
  if (training.criterion=="AUC"){ 
    crite.mom = as.numeric( wilcox.test(scores~YY)$statistic / (sum(YY==0)*sum(YY==1)) ) 
  }
  
  MoM = list(XX=XX,YY=YY,cutoff=cutoff,
             training.criterion=training.criterion,
             variables = variables,
             variables.signs=variables.signs,
             variables.criterion=crite[variables],
             scores=scores,
             criterion.mom=crite.mom)
  return(MoM)
}

masomenos.test = function(X,   # pxn data frame of predictors
                          Y,   # nx1 vectors of labels or responses
                          MoM.out # output form mas0menos.train
){
  # eliminate unclassified samples
  YY = Y[!is.na(Y)]
  XX = X[,!is.na(Y)]
  #

  Nvar = nrow(XX)
  scores = apply ( XX[ MoM.out$variables, ] * MoM.out$variables.signs, 2, mean )
  
  if (MoM.out$training.criterion=="AUC"){ 
    crite.mom = as.numeric( wilcox.test(scores~YY)$statistic / (sum(YY==0)*sum(YY==1)) ) 
  }
  
  MoM = list(XX=XX,YY=YY,
             scores=scores,
             criterion.mom=crite.mom)
  return(MoM)
}




#masomenos.test(matTesting, 1*as.vector(testingGroup=="Good"),MoM.t1)$criterion.mom+0.5




