
#特征A对训练数据集D的信息增益g(D|A)定义为集合D的经验熵H(D)与特征更A给定条件下D的经验条件熵H(D|A)之差，即：
#g(D,A)=H(D)−H(D|A)熵越大，说明系统越混乱，携带的信息就越少。熵越小，说明系统越有序，携带的信息就越多。信息的作用就是在于消除不确定性。
#H(D)=-sum(p*log2p)
#H(D|A)= 
#ID3划分特征使用的就是信息增益IG。一个属性的信息增益越大，表明属性对样本的熵减少的能力就更强，该属性使得数据所属类别的不确定性变为确定性的能力越强。
# 在决策树中，ID3属性划分标准使用的是信息增益，C4.5使用的是信息增益率。
# C4.5算法继承了ID3算法的优点，并在以下几方面对ID3算法进行了改进：能够对不完整数据进行处理。
# C4.5算法有如下优点：产生的分类规则易于理解，准确率较高。其缺点是：在构造树的过程中，需要对数据集进行多次的顺序扫描和排序，因而导致算法的低效。另外，C4.5只适合于能够驻留于内存的数据集，当训练集大得无法在内存容纳时程序无法运行。
# 另外，无论是ID3还是C4.5最好在小数据集上使用，决策树分类一般只试用于小数据。当属性取值很多时最好选择C4.5算法，ID3得出的效果会非常差，因为使用信息增益划分时它倾向于取值多的属性。

### calculate H(D) -(sample in each Class/total sample)*log2(sample in each Class/total sample)
hd<-function(train){
    nc <- ncol(train)
    colnames(train)[nc]<-"class"
    nr <- nrow(train)
    class <- unique(train$class)
    prob <- train %>%group_by(class)%>%summarise(prob = length(class)/nr)%>%
        as.data.frame()
    prob$logp <- log2(prob$prob)
    prob$plogp <- - prob$prob * prob$logp
    res <- sum(prob$plogp)
    res
}
### calculate H(D|A) A is one of the class category. 
hda <- function(train,select){
    nr <- nrow(train)
    nc <- ncol(train)
    colnames(train)[nc]<-"class"
    class <- unique(train$class)
    probc <- train%>%group_by(!!sym(select))%>%
        summarise(prob = length(!!sym(select))/nr)%>%
           as.data.frame()
    rownames(probc)<-probc[,select]
    prob <- unlist(lapply(split(train,train[select]),function(x)hd(x)))
    res<- sum(probc$prob*prob[rownames(probc)])
    #res <- sum(prob$pd)
    res
}
#ID3                         
info_gain<-function(train,select){
    info<-hd(train)-hda(train,select)
    info
}
####calculate ahd for info_gain_ratio
ahd <- function(train,select){
    idx <- which(colnames(train) == select)
    colnames(train)[idx]<-"class"
    traina <- train[,"class",drop=F]
    res <- hd(traina)
    res
}
###C4.5
info_gain_ratio <- function(train,select){
    nominator <- hd(train) - hda(train,select)
    denominator <- ahd(train,select)
    info_gain_ratio <- nominator / denominator 
    info_gain_ratio
}
returnlabel <- function(train) {
    nc <- ncol(train)
    tab <- train[, nc] %>% table(.)
    idx_max <- tab %>% which.max()
    label <- names(tab)[idx_max]
    label
}
##### 
#### step 1 
                          
create_tree <- function(train, func, err){
    nc <- ncol(train)
    labels <- colnames(train)
    lastcol <- colnames(train)[nc]
    lab_len <- unique(train[,lastcol])%>%length()
    if(lab_len == 1){
        lab <- unique(train[,lastcol])
        cat("leaf now is all ",lab,"\n")
    }else if(nc ==1) {
        lab <- returnlabel(train)
        cat("leaf should be ",lab, "since nc is 1")
    }else{
        info <- c()
        for(i in 1:(nc-1)){
            select <- colnames(train)[i]
            info_g <- func(train,select)
            info <- append(info,info_g)
        }
    names(info) <- colnames(train)[1:(nc-1)]
    maxn <- which.max(info) %>% names(info)[.]
    cat(maxn,"\n")
    cat(info[maxn],"\n")
    if (info[maxn] < err) {
        lab <- returnlabel(train)
        cat("leaf,  * ", lab, " *", " ----most of variables","\n")
    }else{
        subsets <- split(train,train[,maxn])
        labs <-setdiff(labels,maxn)
        for(i in names(subsets)){
            cat("Now is subtree ",i,"\n")
            create_tree(subsets[[i]][,labs],func,err)
        }
    }
    }
}
                         
