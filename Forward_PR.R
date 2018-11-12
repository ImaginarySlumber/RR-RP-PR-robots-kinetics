#tested without v and a
Forward_PR = function(c,b,s,psi,sd,psid,sdd,psidd){
  psi = psi/180*pi
  #end effector
  xa = s+b*cos(psi)
  ya = c+b*sin(psi)
  
  #velocity
  xad = sd-b*psid*sin(psi)
  yad = b*psid*cos(psi)
  
  #acceleration
  xadd = sdd-b*psidd*sin(psi)-b*(psid^2)*cos(psi)
  yadd = b*psidd*cos(psi)-b*(psid^2)*sin(psi)
  
  ans = c(xa,ya,xad,yad,xadd,yadd)
  
  return(ans)
}
  