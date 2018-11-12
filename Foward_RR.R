##A function for forward RR mechaninism, tested###
#function(c, b, Xa, Ya,Xq, Yq, Xa_d,Ya_d, Xa_dd,Ya_dd)Forward_RR
Forward_RR = function(c, b, Xq, Yq, PHI,  PSI,PHI_d, PSI_d, PHI_dd,PSI_dd){
#defined in radian  
PHI = (PHI/180)*3.141592658
PSI = (PSI/180)*3.141592658
#print(c(PHI, PSI))

Xa = c*cos(PHI)+b*cos(PSI)+Xq
Ya = c*sin(PHI)+b*sin(PSI)+Yq

Xa_d = -c*PHI_d*sin(PHI)-b*PSI_d*sin(PSI)
Ya_d = c*PHI_d*cos(PHI)+b*PSI_d*cos(PSI)

Xa_dd = -c*PHI_dd*sin(PHI)-c*(PHI_d^2)*cos(PHI)-b*PSI_dd*sin(PSI)-b*(PSI_d^2)*cos(PSI)
Ya_dd = c*PHI_dd*cos(PHI)-c*(PHI_d^2)*sin(PHI)+b*PSI_dd*cos(PSI)-b*(PSI_d^2)*sin(PSI)
#turn all the angles into degree

ans = c(Xa,Ya,Xa_d,Ya_d,Xa_dd,Ya_dd)

return(ans)
}
#return(forward)
###The forward RR function ends here.###