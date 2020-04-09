k <- 2
prob <- matrix(rep(0, k*n), nrow = n)
weight <- matrix(rep(0, k*n), nrow = n)

# 初始权重alpha平均分配，高斯参数miu、sigma由均匀分布随机产生
alpha <- c(0.5, 0.5)
miu   <- runif(k)
sigma <- runif(k)
for(i in 1:200){
  for(j in 1:k){
   prob[,j]<-sapply(samp,dnorm,miu[j],sigma[j])
   weight[,j]<-alpha[j]*prob[,j]
  }
 row_sum=rowSums(weight)
 prob<-weight/row_sum
 olda<-alpha
 oldm<-miu
 olds<-sigma
 for(j in 1:k){
   sum1<-sum(prob[,j])
   sum2<-sum(samp*prob[,j])
   alpha[j]<-sum1/n
   miu[j]<- sum2/sum1
   sum3<-sum(prob[,j]*(samp-miu[j])^2)
   sigma[j]<-sqrt(sum3/sum1)
 }
   thres <- 1e-5
  if(sum(abs(alpha-olda))<thres & sum(abs(miu-oldm))<thres & sum(abs(sigma-olds))<thres){
     break
  }
 cat("step",i,'alpha',alpha,'miu',miu,'sigma',sigma,'\n')
 }

k <- 2
prob <- matrix(rep(0, k*n), nrow = n)
weight <- matrix(rep(0, k*n), nrow = n)

# 初始权重alpha平均分配，高斯参数miu、sigma由均匀分布随机产生
alpha <- c(0.5, 0.5)
miu   <- runif(k)
sigma <- runif(k)

# EM算法实现
for (step in 1:200) {
  # E-step：求第i个样本来自第j个高斯的概率
  for (j in 1:k) {
    prob[, j]   <- sapply(samp, dnorm, miu[j], sigma[j])
    weight[, j] <- alpha[j] * prob[, j]
  }
  row_sum <- rowSums(weight)
  prob    <- weight/row_sum

  # 记录上一次迭代的参数
  oldalpha <- alpha
  oldmiu   <- miu
  oldsigma <- sigma

  # M-step：最大化是通过求导令其为零的方法，这里直接给出参数优化结果
  for (j in 1:k) {
    sum1     <- sum(prob[, j])
    sum2     <- sum(samp*prob[, j])
    alpha[j] <- sum1/n
    miu[j]   <- sum2/sum1
    sum3     <- sum(prob[, j]*(samp-miu[j])^2)
    sigma[j] <- sqrt(sum3/sum1)
  }

  # 设阈值：当上一步迭代得到的参数与下一步迭代得到的参数变化很小，即认为收敛
  threshold <- 1e-5
  if (sum(abs(alpha - oldalpha)) < threshold & 
      sum(abs(miu - oldmiu))     < threshold & 
      sum(abs(sigma - oldsigma)) < threshold) break
  cat('step', step, 'alpha', alpha, 'miu', miu, 'sigma', sigma, '\n')
}
