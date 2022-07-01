#The R-program for obtaining a solution to the given problem is as follows-
  m1 = 1; m2 = 2; m3 = 3; s11 = 1; s12 = 0.8; s13 = -0.4; s22 = 1; s23 = -0.56; s33 = 2
  rho12 = s12/(s11*s22)
  rho12
  r1 = runif(15,0,1)
  x1 = mat.or.vec(15,1)
  for(i in 1:15){
    x1[i] = qnorm(r1[i],m1,s11)}
  x1
  m21 = m2+(rho12*(s22/s11)*(x1-m1))
  sd21 = sqrt((s22^2)*(1-(rho12^2)))
  sd21
  r2 = runif(15,0,1)
  x2 = mat.or.vec(15,1)
  for(i in 1:15){
    x2[i] = qnorm(r2[i],m21[i],sd21)}
  x2
  mu_2 = m3
  sig_21 = array(c(s13,s23),dim=c(1,2))
  sig_21
  sig_11 = array(c(s11^2,s12,s12,s22^2),dim = c(2,2))
  sig_11
  sig_11_inv = solve(sig_11)
  sig_11_inv
  m3_12 = mat.or.vec(15,1)
  for(i in 1:15){
    m3_12[i] = mu_2+(sig_21%*%sig_11_inv%*%array(c(x1[i]-m1,x2[i]-m2),dim = c(2,1)))}
  m3_12
  sig_22 = s33^2
  s3_12 = sqrt(sig_22-(sig_21%*%sig_11_inv%*%t(sig_21)))
  s3_12
  r3 = runif(15,0,1)
  x3 = mat.or.vec(15,1)
  for(i in 1:15){
    x3[i] = qnorm(r3[i],m3_12[i],s3_12)}
  x3
  