Forward_RP = function(b, Xq, Yq, r, PHI, rd,PHI_d, rdd,PHI_dd){
  #the position of end effector
  Xa = Xq+r*cos(PHI)-b*sin(PHI)
  Ya = Yq+r*sin(PHI)+b*cos(PHI)
  
  #the velocity
  Xad = -r*PHI_d*sin(PHI)+rd*cos(PHI)-b*PHI_d*cos(PHI)
  Yad = r*PHI_d*cos(PHI)+rd*sin(PHI)+b*PHI_d*cos(PHI)
  
  #acceleration
  Xadd = -rd*PHI_d*sin(PHI)+rdd*cos(PHI)-PHI_dd*(r*sin(PHI)+b*cos(PHI))+(PHI^2)*(b*sin(PHI)-r*cos(PHI))
  Yadd = -rd*PHI_d*cos(PHI)+rdd*sin(PHI)-PHI_dd*(r*cos(PHI)-b*cos(PHI))-(PHI^2)*(b*cos(PHI)-r*sin(PHI))
  
  ans = c(Xa, Ya, Xad,Yad,Xadd,Yadd)
  
  return(ans)
}