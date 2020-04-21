# f(x) =sign(wx+b)
# choose xi,yi
# yi*(w*xi+b) <=0 
# w <- w+lr*yi*xi
# b <- b+ lr*y
mat <- matrix(c(3,3,1,4,3,1,1,1,-1),nrow=3,byrow = T)
train <- function(mat,lr = 0.5){
    nr <- nrow(mat)
    nc <- ncol(mat)
    w0 <- matrix(0,nc-1,1)
    b0 <- 0
    res <- NULL
    while(is.null(res)){
        print(list(w0=w0,b0=b0))
        for(i in seq_len(nr)){
            cat("choose the ",i, " sample\n")
            xi <- mat[i,1:(nc-1)]
            yi <- mat[i,nc]
            f <- (xi%*%w0+b0)*yi
            if(f <= 0){
                xi <- matrix(mat[i, 1:(nc - 1)], nc - 1, 1)
                w0 <- w0 +lr*yi*xi
                b0 <- b0 +lr*yi
                res <-NULL
                break
            }else{
                res <- c(res,f)
                cat("result = ",res,"\n")
            }
        }
    }
    cat("final w0",w0," final b0",b0,"\n")
}
