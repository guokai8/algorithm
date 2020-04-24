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
###
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
