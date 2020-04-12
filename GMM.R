# generate n=5000 samples
set.seed(637351)
n <- 5000

# first Gaussian 
alpha1 <- 0.4
miu1   <- 3
sigma1 <- 1

# second Gaussian
alpha2 <- 0.6
miu2   <- -2
sigma2 <- 2

n1 <- floor(n*alpha1)
n2 <- n-n1

samp <-numeric(n)
samp[1:n1] <- rnorm(n1, miu1, sigma1)
samp[(n1+1):n] <- rnorm(n2, miu2, sigma2)

# make a figure
hist(samp, freq = FALSE)
lines(density(samp), col = 'red')
###
k <- 2
prob <- matrix(rep(0, k*n), nrow = n)
weight <- matrix(rep(0, k*n), nrow = n)
# 初始权重alpha平均分配，高斯参数miu、sigma由均匀分布随机产生
###let's say P(a) and P(b)
###use Bayes rules
##P(b|Xi)=P(Xi|b)*P(b)/(P(Xi|b)*P(b)+P(Xi|a)*P(a))
##P(Xi|b)=dnorm(Xi,miu,sigma)
alpha <- c(0.5, 0.5)
miu   <- runif(k)
sigma <- runif(k)
for(i in 1:200){
    # E-step：求第i个样本来自第j个高斯的概率
    for(j in 1:k){
        ###Prior 
        prob[,j]<-sapply(samp,dnorm,miu[j],sigma[j])
        ###P(Xi|b)*P(b)
        weight[,j]<-alpha[j]*prob[,j]
    }
    ####(P(Xi|b)*P(b)+P(Xi|a)*P(a))
    row_sum=rowSums(weight)
    ###P(Xi|b)*P(b)/(P(Xi|b)*P(b)+P(Xi|a)*P(a)) == Posterior
    prob<-weight/row_sum
    olda<-alpha
    oldm<-miu
    olds<-sigma
    #M-step：最大化是通过求导令其为零的方法，这里直接给出参数优化结果
    for(j in 1:k){
        sum1<-sum(prob[,j])
        ###update xi
        sum2<-sum(samp*prob[,j])
        ##update the prior
        alpha[j]<-sum1/n
        #update miu
        miu[j]<- sum2/sum1
        ###update sigma
        sum3<-sum(prob[,j]*(samp-miu[j])^2)
        sigma[j]<-sqrt(sum3/sum1)
    }
    thres <- 1e-5
    if(sum(abs(alpha-olda))<thres & sum(abs(miu-oldm))<thres & sum(abs(sigma-olds))<thres){
        break
    }
    cat("step",i,'alpha',alpha,'miu',miu,'sigma',sigma,'\n')
}
