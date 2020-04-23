##calculate the class prob p(c)
preprob <- function(train){
    nc <- ncol(train)
    nr <- nrow(train)
    pre <- table(train[,nc])/nr
    pre <- as.data.frame(pre)
    return(pre)
}
###calculate the p(f|c)
prob <- function(train){
    nc <- ncol(train)
    nr <- nrow(train)
    class <- colnames(train)[nc]
    all <- gather(train,feature,value,-!!class)
    colnames(all)[1]<-"class"
    all <- all%>%group_by(class,feature)%>%count(value)%>%mutate(prob=n/sum(n))
    all[,c(1,2,3,5)]
}
### p(f|c)*p(c)
pred <- function(test,prob,pre){
    colnames(pre)<-c("class","prob")
    colnames(test)<-c("feature","value")
    pro <- inner_join(test,prob,by=c("feature","value"))%>%group_by(class)%>%summarise(p=prod(prob))
    res <- inner_join(pro,pre,by=c("class"="class"))%>%mutate(pred=p*prob)
    res[,c(1,4)]
}
###
train.apple <- data.frame(
    size = c("big", "small", "big", "big", "small", "small"),
    weight = c("light", "heavy", "light", "light", "heavy", "light"),
    color = c("red", "red", "red", "green", "red", "green"),
    taste = c("good", "good", "bad", "bad", "bad", "good")
)
test.apple <- data.frame(
    feature=c("size","weight","color"),
    value=c("big","light","red")
)
pc <- preprob(train.apple)
pfc <- prob(train.apple)
pred(test.apple,pfc,pc)
