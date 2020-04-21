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
                #cat("result = ",res,"\n")
            }
        }
    }
    cat("final w0",w0," final b0",b0,"\n")
    return(c(b0,w0))
}

### another example use other guy's data to prove my code
##generate data ## https://github.com/billderose-zz/perceptron/blob/master/perceptron.R
Random.Unit <-function(n, dim, threshold) {
  points <- runif(n * dim)
  points <- matrix(points, ncol = dim)
  label <- ifelse(apply(points, 1, sum) < threshold, -1, 1)
  return(cbind(label, x0 = rep(1, n), points))
}
### generate figures
Plot2D <- function(points, a, b) {
  plot(points[, 3:4], xlab = "X", ylab = "Y",
       pch = ifelse(points[, 1] == 1, 2, 8),
       col = ifelse(points[, 1] == 1, "blue", "red"))
  abline(a, b)
}
####
THRESHOLD <- 0.75
pts <- Random.Unit(1000, 2, THRESHOLD)
mat <- pts[,c(3,4,1)]
w <- train(mat)
Plot2D(pts, -w[1]/w[3], -w[2]/ w[3])
####3D
Plot3D <- function(points, a, b, c, d) {
  plot3d(points[, 3:5], xlab = "X", ylab = "Y", zlab = "Z",
         pch = ifelse(points[, 1] == 1, 2, 8),
         col = ifelse(points[, 1] == 1, "blue", "red"))
  planes3d(a, b, c, d)
}
THRESHOLD <- 1.5
pts <- Random.Unit(1000, 3, THRESHOLD)
mat <- pts[],c(3,4,5,1)
w <- train(pts)
Plot3D(pts, w[4], w[3], w[2], w[1])
##### another example
irissub <- iris[1:100, c(1, 3, 5)]
names(irissub) <- c("sepal", "petal", "species")
head(irissub)
irissub[, 4] <- 1
irissub[irissub[, 3] == "setosa", 4] <- -1

x <- irissub[, c(1, 2)]
y <- irissub[, 4]
mat <- as.matrix(cbind(x,y))
w <-train(mat)
plot(x[,1],x[,2],col=y+2))
abline(-w[1]/w[3],-w[2]/w[3],col="red")
