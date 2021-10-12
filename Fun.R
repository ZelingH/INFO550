
#### Step I filtering ----
stepI_filtering = function(data , dict, missing_rate_control = 0.90, low_frequency_control = 0.05) {
  ## create filtering index
  filter_ind = sapply(1:ncol(data), function(idx) {
    var = data[,idx]
    if(dict$type[idx] == "continuous") {
      # continuous variables with acceptable missing rates 
      sum(is.na(var))/nrow(data) <= missing_rate_control
    }
    else if(dict$type[idx] %in% c("binary", "ordinal")) {
      # binary/ordinal variables with acceptable missing rates and frequency rates
      (sum(is.na(var))/nrow(data) <= missing_rate_control) & all(prop.table(table(var)) >= low_frequency_control) & (length(table(var))>=2)
    }
    else {
      # pre-specified "type" is needed
      stop("Please specify 'type' in dictionary among 'binary', 'ordinal', 'continuous'")
    }
  })
  dict$filtering = ifelse(filter_ind, "yes","no")
  return(list(
    "data_filtered" =data[, filter_ind],
    "dictionary" = dict)
  )
}

#### Summary of Missingness ----
summary_missingness = function(data, dict) {
  ## missing matrix
  mis.mat = cbind.data.frame(
    "rate" = apply(data, 2, function(var) sum(is.na(var)))/nrow(data),
    "type" = factor(dict$type[dict$name %in% colnames(data)])
  )
  ## plot
  p = ggplot(mis.mat, aes(rate, fill = type)) +
    geom_histogram(binwidth = 0.05) + 
    facet_wrap(~type) +
    xlab("Missing Rates") +
    scale_fill_brewer(palette="Dark2") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"))
  ## table
  tab = pander::pander(tapply(mis.mat$rate, mis.mat$type, summary))
  return(list(plot_missingness = p,tab_missingness = tab))
}

#### Step II screening ----
stepII_screening <- function(Ymat, Xmat, cor_test_type = "spearman", adjust_method = "none", p_value_threshold = 0.05) {
  if(is.vector(Xmat)) {
    message("For screening, please assign at least two potential predictors")
    break
  }
  ## If Ymat is a single variable
  else if(is.vector(Ymat) & ncol(Xmat) >= 2) {
    Ymat = list(Ymat)
  }
  ## save screening results as a list
  screening_list = lapply(as.list(Ymat), function(Y) stepII_screening_vecY(Y=Y, X = Xmat,
                                                                           cor_test_type = cor_test_type,
                                                                           adjust_method = adjust_method,
                                                                           p_value_threshold = p_value_threshold))
  return(screening_list)
}

stepII_screening_vecY = function(Y, X, cor_test_type, adjust_method, p_value_threshold ) {
  # predictors in X with the same missing patterns as Y
  missing_ind = apply(X, 2, function(x) {sum(is.na(Y) == is.na(x)) == length(Y)})
  # p values from correlation tests (treating error message as NA)
  cor_vec = apply(X, 2, function(x) tryCatch(cor.test(x, Y, method = cor_test_type, use = "complete.obs")$p.value,
                                             error = function(e) NA ))
  # adjusted p values
  if(adjust_method %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr")) {
    cor_vec = p.adjust(cor_vec, method = adjust_method)
  }
  # filter predictors
  screened_vars = colnames(X)[(!missing_ind) & (cor_vec <= p_value_threshold)]
  return(screened_vars[!is.na(screened_vars)])
}

#### Step III imputation ----

## scalable imputation model
impute_scale = function(Ymat, Xmat, dict, vars_list) {
  
  ## initialize parallel computing
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores, type = 'FORK')
  registerDoParallel(cl)
  
  ## imputation models
  # remove variables with <= 1 selected predictors
  var_rm = names(vars_list)[sapply(vars_list, length) <= 1]
  Ymat_v = Ymat[, !colnames(Ymat) %in% var_rm]
  vars_list_v = vars_list[!names(vars_list) %in% var_rm]
  
  ## start the imputation
  imputed_list = foreach(idx = 1:ncol(Ymat_v)) %dopar% {
    tryCatch({
      Y = Ymat_v[,idx]
      X =  Xmat[,vars_list_v[[idx]]]
      type = dict$type[dict$name == colnames(Ymat_v)[idx]]
      impute_main(Y, X, type)
    }, error = function(e) return(paste0("The ", idx, "th variable cause the error: ",e
    )))
  }
  
  
  ## stop the parallel computing
  stopImplicitCluster()
  
  ## check number of variables failed to be imputed
  nums = sum(sapply(imputed_list, function(x) length(x) != nrow(Xmat))) 
  
  if (nums != 0) {
    message(paste0(nums, " variables failed to be imputed!"))
    
    ## replace the error message with NA
    for(i in 1:length(imputed_list)) {
      if(length(imputed_list[[i]]) != nrow(Xmat)) {
        print(imputed_list[[i]])
        imputed_list[[i]] <- rep(NA, nrow(Xmat))
      }
    }
  }
  
  # convert list to a matrix
  imputed_mat = do.call(cbind, imputed_list)
  #replace with the observed data
  imputed_mat[!is.na(Ymat_v)] = Ymat_v[!is.na(Ymat_v)]
  colnames(imputed_mat) = colnames(Ymat_v)
  
  return(imputed_mat)
  
}
## main imputation models
impute_main = function(Y, X, type) {
  if(is.null(dim(X))) {
    stop("Not enough predictors to build an imputation model")
    
  }
  else if(ncol(X) == 0) {
    stop("Not enough predictors to build an imputation model")
  }
  else {
    X = apply(X, 2, function(x) {x[is.na(x)] = mean(x, na.rm = TRUE); return(x)})
    Dat = na.omit(cbind(Y, X))
    
    if (type == "binary") {
      bet = Est.ALASSO.GLM.Approx(Dat, fam0 = "binomial")
      pred = g.logit(pmin(as.matrix(cbind(rep(1, nrow(X)), X)) %*% bet,100))
    }
    
    else if (type == "ordinal") {
      y = as.factor(Dat[,1])
      K = length(unique(Dat[,1]))
      parameter = Est.ALASSO.POR.Approx(y, as.matrix(Dat[,-1]), K = K, BIC.factor = 0.1)
      intercepts = parameter[1:(K-1)]
      bet = parameter[-(1:(K-1))]
      eta = c(X %*% bet) + matrix(intercepts, nrow = nrow(X), ncol = K-1, byrow = TRUE)
      cumprob = t(apply(eta, 1, g.logit))
      prob = cbind(cumprob, 1) - cbind(0, cumprob)
      pred = prob %*% as.matrix(as.numeric(levels(y)))
    }
    else {
      bet = Est.ALASSO.GLM.Approx(Dat, fam0 = "gaussian")
      pred = as.matrix(cbind(rep(1, nrow(X)), X)) %*% bet
    }
    return(pred)
  }
}

## Imputation models for binary and continuous variables
Est.ALASSO.GLM.Approx = function(data,Wi=NULL,rtn="EST",adap=T,BIC.factor=0.1,fam0,offset=NULL){
  data = as.matrix(data); y = data[,1]; x = data[,-1,drop=F]; nn=length(y); pp = ncol(x); if(is.null(Wi)){Wi=rep(1,nn)};
  bini = glmnet(x,y,weights=Wi,family=fam0,alpha=0,offset=offset)
  lam.xx=svd(t(x)%*%x/nn)$d; tmpdf = apply(lam.xx/(lam.xx+VTM(bini$lambda,pp)),2,sum)
  tmpind = which.min(deviance(bini)+2*tmpdf); 
  lam.ridge = bini$lambda[tmpind]; c(bini$a0[tmpind], bini$beta[,tmpind]); 
  bini = c(bini$a0[tmpind], bini$beta[,tmpind]); w.b = 1/abs(bini[-1]); 
  Ahalf = svd(-A.fun(bini,data)+diag(c(0,rep(1,pp)))); Ahalf = Ahalf$u%*%diag(sqrt(Ahalf$d))%*%t(Ahalf$v)
  ynew = Ahalf%*%bini; xnew = Ahalf
  tmpfit = glmnet(x=xnew, y=ynew, family='gaussian',penalty.factor = c(0,w.b),
                  alpha=1, lambda = 10^seq(-4,3,0.01), intercept=F)
  BIC.lam = -2*logitlik.fun(tmpfit$beta,data)+min(nn^BIC.factor,log(nn))*tmpfit$df
  m.opt = which.min(BIC.lam); bhat.modBIC = tmpfit$beta[,m.opt]; lamhat = tmpfit$lambda[m.opt]
  bhat.modBIC
}

## Imputation models for ordinal variables
Est.ALASSO.POR.Approx = function(Y, X, K, BIC.factor=0.1){
  nn=length(Y);
  dat=cbind(Y, X)
  lam.ridge=dim(X)[2]/dim(X)[1]
  #lam.xx=svd(t(X)%*%X/nn)$d;  lam.99 = min(lam.xx[cumsum(lam.xx)/sum(lam.xx) < 0.99])
  fit.ini <- ordinalNet(X, Y, family="cumulative", link="logit",alpha=0,
                        parallelTerms=TRUE, nonparallelTerms=FALSE, lambdaVals=lam.ridge)
  bini=coef(fit.ini)
  w.eta = 1/abs(bini[K:length(bini)])
  Ahalf = svd(-A.ridge.fun(bini,dat, lam.ridge, K)+diag(c(rep(0,(K-1)), rep(1, length(w.eta)))))
  
  Ahalf = Ahalf$u%*%diag(sqrt(Ahalf$d))%*%t(Ahalf$v)
  ynew = Ahalf%*%bini; xnew = Ahalf
  tmpfit = glmnet(x=xnew, y=ynew, family='gaussian',penalty.factor = c(rep(0,(K-1)), w.eta),
                  alpha=1, lambda = 10^seq(-4,3,0.01), intercept=F)
  
  BIC.lam=-2*loglik.mat(tmpfit$beta,Y, X, K)+min(nn^BIC.factor,log(nn))*tmpfit$df
  m.opt = which.min(BIC.lam); bhat.modBIC = tmpfit$beta[,m.opt]; lamhat = tmpfit$lambda[m.opt]
  bhat.modBIC
}

## helper functions
g.logit = function(xx){exp(xx)/(exp(xx)+1)}
logit = function(xx){log(xx/(1-xx))}
dg.logit = function(xx){exp(xx)/(exp(xx)+1)^2}

logitlik.fun = function(bet.mat,dat){
  yi = dat[,1]; xi = dat[,-1]; pi.mat = g.logit(cbind(1,xi)%*%bet.mat) ## N x B
  apply(log(pi.mat)*yi + log(1-pi.mat)*(1-yi),2,sum)
}

A.fun = function(bet,dat){
  yy = dat[,1]; xx.vec = cbind(1,dat[,-1])
  -t(c(dg.logit(xx.vec%*%bet))*xx.vec)%*%xx.vec/length(yy)
}

###funtion used for ordinal approximate lasso
A.ridge.fun = function(par, dat,lambda, K){
  theta=par[1:(K-1)]
  eta=par[K:length(par)]
  eta=matrix(eta,nrow=length(eta),1)
  yy = dat[,1]; xx.vec = data.matrix(dat[,-1])
  n=length(yy)
  xeta=c(data.matrix(xx.vec)%*%eta)
  K=length(unique(yy))
  
  b.dd=matrix(0, nrow=length(c(theta,eta)), ncol=length(c(theta,eta)))
  b.dd[1,1]=sum(-(1*(yy==2)+1*(yy==1))*c(dg.logit(theta[1]+xeta))-(yy==2)*(exp(theta[1]+theta[2])/(exp(theta[2])-exp(theta[1]))^2))
  b.dd[1,2]=sum((yy==2)*(exp(theta[1]+theta[2])/(exp(theta[2])-exp(theta[1]))^2))
  b.dd[1,(K:dim(b.dd)[2])]=b.dd[(K:dim(b.dd)[2]),1]=colSums(-(1*(yy==2)+1*(yy==1))*diag(c(dg.logit(theta[1]+xeta)))%*%xx.vec)
  
  theta2.2=NULL
  for(k in 2:(K-2)){
    tmp=sum(-(1*(yy==(k+1))+1*(yy==(k)))*c(dg.logit(theta[k]+xeta))-
              (yy==k)*exp(theta[k]+theta[k-1])/(exp(theta[k])-exp(theta[k-1]))^2-
              (yy==(k+1))*exp(theta[k]+theta[k+1])/(exp(theta[k])-exp(theta[k+1]))^2)
    theta2.2=c(theta2.2, tmp)
  }
  diag(b.dd)[2:(K-2)]=theta2.2
  
  for(k in 2:(K-1)){
    tmp=sum((yy==(k))*exp(theta[k]+theta[k-1])/(exp(theta[k])-exp(theta[k-1]))^2)
    b.dd[k, k-1]=b.dd[k-1,k]=tmp
  }
  
  b.dd[K-1, K-1]=sum(-(yy==(K-1))*exp(theta[K-1]+theta[K-2])/(exp(theta[K-1])-exp(theta[K-2]))^2-
                       (1*(yy==K)+1*(yy==(K-1)))*c(dg.logit(theta[K-1]+xeta)))
  
  for(k in 2:(K-2)){
    tmp=colSums(-(1*(yy==(k+1))+1*(yy==k))*diag(c(dg.logit(theta[k]+xeta)))%*%xx.vec)
    b.dd[K:dim(b.dd)[1], k]=b.dd[k,K:dim(b.dd)[2]]=tmp
  } 
  
  b.dd[K:dim(b.dd)[1], K-1]=b.dd[K-1,K:dim(b.dd)[2]]=colSums((-(1*(yy==K)+1*(yy==(K-1)))*diag(c(dg.logit(theta[K-1]+xeta)))%*%xx.vec))
  
  for(i in 1:length(eta)){
    for(j in 1:i){
      tmp1=sum((yy==1)*(-xx.vec[,i]*xx.vec[,j]*c(dg.logit(theta[1]+xeta))))
      tmp2=0
      for(k in 2:(K-1)){
        tmp2=tmp2+sum((yy==k)*(-xx.vec[,i]*xx.vec[,j]*c(dg.logit(theta[k-1]+xeta))-xx.vec[,i]*xx.vec[,j]*c(dg.logit(theta[k]+xeta))))
      }
      tmp3=sum((yy==K)*(-xx.vec[,i]*xx.vec[,j]*c(dg.logit(theta[K-1]+xeta))))
      if(i==j){
        b.dd[K-1+i,K-1+j]=b.dd[K-1+j, K-1+i]=sum(tmp1, tmp2, tmp3)-n*lambda}
      if(i!=j){
        b.dd[K-1+i,K-1+j]=b.dd[K-1+j, K-1+i]=sum(tmp1, tmp2, tmp3)}
    }
  }
  b.dd
}

loglik.mat=function(par.mat, Y, X, K){
  logliki.mat=function(par.mat, Yi, Xi, K){
    theta.mat=par.mat[1:(K-1),]
    eta.mat=par.mat[K:dim(par.mat)[1],]
    L1=(Yi==1)*(theta.mat[1,]+Xi%*%eta.mat-log(1+exp(theta.mat[1,]+Xi%*%eta.mat)))
    L2=0
    for (k in 2:(K-1)){
      tmp=(Yi==k)*(Xi%*%eta.mat+log(exp(theta.mat[k,])-exp(theta.mat[k-1,]))-log(1+exp(theta.mat[k,]+Xi%*%eta.mat))-log(1+exp(theta.mat[k-1,]+Xi%*%eta.mat)))
      L2=L2+tmp
    }
    L3=-(Yi==K)*log(1+exp(theta.mat[K-1,]+Xi%*%eta.mat))
    loglik=as.vector((L1+L2+L3))
    loglik}
  colSums(matrix(unlist(lapply(1:length(Y), function(i) logliki.mat(par.mat, Y[i], X[i,], K))), ncol=dim(par.mat)[2], byrow=T))
}

VTM<-function(vc, dm){
  matrix(vc, ncol=length(vc), nrow=dm, byrow=T)
}




