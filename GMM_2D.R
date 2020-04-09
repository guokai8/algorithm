library(mvtnorm)
n1=400
mu1=c(0.5,0.5)
var1=matrix(c(1,0,0,3),2,2,byrow = T)
n2=600
mu2=c(5.5,2.5)
var2=matrix(c(2,0,0,2),2,2,byrow = T)
n3=1000
mu3=c(1,7)
var3=matrix(c(6,0,0,2),2,2,byrow = T)
#####
x1=rmvnorm(n=n1,mean=mu1,sigma = var1)
x2=rmvnorm(n=n2,mean=mu2,sigma = var2)
x3=rmvnorm(n=n3,mean=mu3,sigma = var3)
#####
x<-rbind(x1,x2,x3)
xx<-as.data.frame(x)
xx$group<-rep(1:3,times=c(400,600,1000))
####
library(ggplot2)
theme_set(theme_light(base_size=15))
ggplot(xx,aes(V1,V2,color=group))+geom_point()
##########
n_clusters=3
n_points=nrow(x)
mu=rbind(c(0,-1),c(6,0),c(0,9))
var=matrix(rep(1,6),3,2)
Pi=rep(1/3,3)
W=matrix(1,nrow(x),n_clusters)/3
#########################
update_w<-function(x,mu,var,pi){
    n_points=nrow(x)
    n_clusters=length(pi)
    pdfs=matrix(0,n_points,n_clusters)
    for(i in 1:n_clusters){
        pdfs[,i]<-dmvnorm(x,mean=mu[i,],sigma=diag(var[i,]))*pi[i]
    }
    #w=sweep(pdfs,1,rowSums(pdfs),"/")
    w=pdfs/rowSums(pdfs)[row(pdfs)]
    return(w)
}
######
update_pi<-function(w){
    pi=colSums(w)/sum(w)
    return(pi)
}
###### ? some thing wrong here
logLH<-function(x,pi,mu,var){
    n_points=nrow(x)
    n_clusters=length(pi)
    pdfs=matrix(0,n_points,n_clusters)
    for(i in 1:n_clusters){
        pdfs[,i]<-dmvnorm(x,mean=mu[i,],sigma=diag(var[i,]))*pi[i]
    }
    return(mean(log(colSums(pdfs))))
}
####
update_mu<-function(x,w){
    n_clusters=ncol(w)
    mu=matrix(0,3,2)
    for(i in 1:n_clusters){
        mu[i,1]=weighted.mean(x[,1],w[,i])
        mu[i,2]=weighted.mean(x[,2],w[,i])
    }
    return(mu)
}
####
update_var<-function(x,mu,w){
    n_clusters=ncol(w)
    var=matrix(0,3,2)
    for(i in 1:n_clusters){
        var[i,1]<-weighted.mean((x[,1]-mu[i,1])^2,w[,i])
        var[i,2]<-weighted.mean((x[,2]-mu[i,2])^2,w[,i])
    }
    return(var)
}
######
logl<-numeric()
for(i in 1:5){
    W=update_w(x,mu,var,Pi)
    Pi=update_pi(W)
    mu=update_mu(x,W)
    var=update_var(x,mu,W)
    print(logLH(x,Pi,mu,var))
}
