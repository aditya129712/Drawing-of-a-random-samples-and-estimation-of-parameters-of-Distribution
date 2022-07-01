#The R-program for obtaining the random sample and estimating the parameters from the given BND is as follows-
  mx=12; my=15; sdx=8; sdy=5
  rho=40/(sdx*sdy)
  rho
  r<-runif(10,0,1)
  x<-mat.or.vec(10,1)
  for(i in 1:10){
    x[i]<-qnorm(r[i],mx,sdx)}
  x
  my_x<-my+((rho*sdy/sdx)*(x-mx))
  sdyx<-sqrt(sdy^2*(1-(rho^2)))
  r1<-runif(10,0,1)
  y<-mat.or.vec(10,1)
  for(i in 1:10){
    y[i]<-qnorm(r1[i],my_x[i],sdyx)}
  y
  mxhat<-mean(x)
  mxhat
  myhat<-mean(y)
  myhat
  sx_hat<-sqrt(var(x))
  sx_hat
  sy_hat<-sqrt(var(y))
  sy_hat
  rho_hat<-cov(x,y)/(sx_hat*sy_hat)
  rho_hat
  