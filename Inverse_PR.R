Inverse_PR = function(c,b,xa,ya,xad,yad,xadd,yadd,type){
 
  if (type==1)
    psi = asin((ya-c)/b)
  else if (type==2)
    psi = pi-asin((ya-c)/b)
  
  s = xa-b*cos(psi)
  psid = yad/(b*cos(psi))
  sd = xad+b*psid*sin(psi)
  
  psidd = (yadd+b*(psid^2)*sin(psi))/(b*cos(psi))
  sdd = xadd+b*psidd*sin(psi)+b*(psid^2)*cos(psi)
  psi = psi/pi*180
  ans = c(s,psi,sd,psid,sdd,psidd)
  return(ans)
}