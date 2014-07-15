bertalanffy.comparison<-function(x,y,alpha)
{if (!is.list(y)) 
        y <- list(y)
 nI<-length(y)

for ( j in 1:(nI) ) bertalanffy(L=y[,j], t=x)

}
