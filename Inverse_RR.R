#tested
Inverse_RR = function(c, b, Xq, Yq, Xa, Ya, Xa_d, Ya_d,Xa_dd, Ya_dd,type){
  
  X = Xa-Xq
  Y = Ya-Yq
  
  #define all the required matrices
  PHI_d = matrix(nrow=2,ncol=1)
  PSI_d = matrix(nrow = 2, ncol =  1)
  PHI_dd = matrix(nrow = 2, ncol =  1)
  PSI_dd = matrix(nrow = 2, ncol =  1)
  
  #print(c(X,Y))
  xydot = matrix(c(Xa_d,Ya_d),2,1)
  xydotdot = matrix(c(Xa_dd,Ya_dd),2,1)
  
  #define the answer function
  ans = matrix(nrow = 7, ncol = 3)
  
  #define A, B, C for the calc
  A = 2*c*(Y)
  B = 2*c*(X)
  C = c^2 + X^2 + Y^2 - b^2
  
  #define t1 and t2 for the calc for PHI
  t1 = (A + sqrt(A^2+B^2-C^2))/(C+B)
  t2 = (A - sqrt(A^2+B^2-C^2))/(C+B)
  PHI = matrix(c(2*atan(t1),2*atan(t2)),2,1)
  ans[2,1] = 'PHI'
  ans[2,2] = PHI[1,1]/pi*180
  ans[2,3] = PHI[2,1]/pi*180
  
  #define u1, u2, v1, v2 for the calc for PSI
  u1 = (X-c*cos(PHI[1,1]))/b
  v1 = (Y-c*sin(PHI[1,1]))/b
  u2 = (X-c*cos(PHI[2,1]))/b
  v2 = (Y-c*sin(PHI[2,1]))/b
  PSI = matrix(c(atan2(v1,u1),atan2(v2,u2)),2,1)
  ans[5,1] = 'PSI'
  ans[5,2] = PSI[1,1]/pi*180
  ans[5,3] = PSI[2,1]/pi*180
  
  for (i in c(1, 2)) {
    
    #find the dot ones
    calcmat = matrix(c(-c*sin(PHI[i,1]), -b*sin(PSI[i,1]), c*cos(PHI[i,1]), b*cos(PSI[i,1])),2,byrow=TRUE)
    dot = solve(calcmat)%*%xydot
    PHI_d[i,1] = dot[1, 1]
    PSI_d[i,1] = dot[2, 1]
    
    #fine the dotdot ones
    mat1 = xydotdot[1,1]+c*((PHI_d[i,1])^2)*cos(PHI[i,1])+b*cos(PSI[i,1])*(PSI_d[i,1])^2
    mat2 = xydotdot[2,1]+c*((PHI_d[i,1])^2)*sin(PHI[i,1])+b*sin(PSI[i,1])*(PSI_d[i,1])^2
    calcmat2 = matrix(c(mat1, mat2),2,1)
    dotdot = solve(calcmat)%*%calcmat2
    PHI_dd[i,1] = dotdot[1,1]
    PSI_dd[i,1] = dotdot[2,1]
    ans[1,i+1] = i
    
    ans[3,1] = 'PHI_d'
    ans[3,i+1] = PHI_d[i,1]
    ans[6,1] = 'PSI_d'
    ans[6,i+1] = PSI_d[i,1]
    ans[4,1] = 'PHI_dd'
    ans[4,i+1] = PHI_dd[i,1]
    ans[7,1] = 'PSI_dd'
    ans[7,i+1] = PSI_dd[i,1]   
  }

  if (type == 1) anssheet = c(PHI[1,1]/pi*180,PSI[1,1]/pi*180,PHI_d[1,1],PSI_d[1,1],PHI_dd[1,1],PSI_dd[1,1])
  else if (type == 2) anssheet = c(PHI[2,1]/pi*180,PSI[2,1]/pi*180,PHI_d[2,1],PSI_d[2,1],PHI_dd[2,1],PSI_dd[2,1])
  
  return(anssheet)
}