forward <- function(O,A,B,PI){
    len <- length(O)
    nr <- nrow(B)
    alpha <- matrix(0,len,nr)
    for(t in 1:len){
        for(j in 1:nr){
            if(t==1){
                alpha[t,j] <- PI[j]*B[j,O[t]]
            }else{
                alpha[t,j] <- alpha[t-1,j]%*%A[,j]*B[j,O[t]]
            }
        }
    }
    p=sum(alpha[len,])
    p
}
