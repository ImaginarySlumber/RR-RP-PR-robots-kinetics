Inverse_RP = function(b,xq,yq,xa,ya,xad,yad,xadd,yadd,type){
  #compute phi and r
  x = xa-xq
  y = ya-yq
  
  if (type==1)
    r = sqrt((x)^2+(y)^2-b^2)
  else if (type==2)
    r = -sqrt((x)^2+(y)^2-b^2)
  
  trig = matrix(nrow=2,ncol = 1)
  trig = solve(matrix(c(-b,r,r,b),2,byrow=TRUE))%*%matrix(c(x,y),2,1)
  phi = atan2(trig[1,1],trig[2,1])
  
  #solve for v
  v = matrix(nrow=2,ncol = 1)
  v = solve(matrix(c(-r*sin(phi)-b*cos(phi),cos(phi),r*cos(phi)+b*sin(phi),sin(phi)),2,byrow = TRUE))%*%matrix(c(xa,ya),2,1)
  phid = v[1,1]
  rd = v[2,1]
  
  #solve for a
  inputa = matrix(c(xadd,yadd),2,1)
  a = matrix(nrow=2,ncol = 1)
  ddsolve = matrix(c(-b*cos(phi)-r*sin(phi),cos(phi),r*cos(phi)-b*sin(phi),sin(phi)),2,2,byrow = TRUE)
  dsolve = matrix(c(phid*(b*sin(phi)-r*cos(phi))-rd*sin(phi),-phid*sin(phi),-phid*(b*cos(phi)+r*sin(phi))+rd*cos(phi),phid*cos(phi)),2,2,byrow = TRUE)
  a = solve(ddsolve)%*%(-dsolve%*%v+inputa)
  phidd = a[1,1]
  rdd = a[2,1]
  
  ans = c(r,phi/pi*180,rd,phid, rdd,phidd)
  
  return(ans)
}