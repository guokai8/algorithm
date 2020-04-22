## scale data and split to train and test data
train <- function(train,test,k=6,prob=FALSE,method="euclidean"){
    train <- as.data.frame(train)
    test <- as.data.frame(test)
    nc <- ncol(train)
    nrk <- nrow(train)
    nrt <- nrow(test)
    
    for(i in 1:nrt){
        dis <- data.frame(distance = rep(0,nrk))
        for(j in 1:nrk){
            ###calculate distance between test point with all known points
            dis[j,1]<-dist(rbind(test[i,1:(nc-1)],train[j,1:(nc-1)]),method = method)
            colnames(dis)[1]<-"distance"
            dis[j,2]<-train[j,nc]
            colnames(dis)[2]<-"known"
        }
        dis <-dis[order(dis$distance),]
        if(isTRUE(prob)){
            freq <- as.data.frame(table(dis[1:k,"known"])/k)
        }else{
            freq <- as.data.frame(table(dis[1:k,"known"]))
        }
        freq <- freq[order(-freq$Freq),]
        test[i,nc+1]<-as.character(freq[1,1])
        if(isTRUE(prob)){
            test[i,nc+2]<-as.numeric(freq[1,2])
        }
        
    }
    colnames(test)[nc+1]<-"predict"
    if(isTRUE(prob)){
        colnames(test)[nc+2]<-"prob"
    }
    test
}
iris_s <- data.frame(scale(iris[, 1:4]))
iris_s <- cbind(iris_s, iris[, 5])
names(iris_s)[5] = "Species"
sample_list <- sample(1:150, size = 100)
iris_know <- iris_s[sample_list, ]
iris_unknow <- iris_s[-sample_list, ]
###run the function
pre<-train(iris_know,iris_unknow,k=5)
