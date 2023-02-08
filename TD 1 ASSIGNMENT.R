#Used libraries:
library(readxl)

setwd("C:/Users/abhil/Downloads")
df=read_excel('Pure species.xlsx')
t=data.frame(df)
sub=subset(t)
print(sub)

#Here we use values of n-butane

#Used functions:
#For saturated vapor
z1=function (b,q,z=1){
  n=20
  
  for (i in seq(1,n,1)){
    Z=1+b-(q*b*(z-b)/((z)*(z+b)))
    z=Z
    
  }
  
  return (Z)
  
}
#for saturated liquid
z2=function (b,q,z=b){
  n=20
  
  for (i in seq(1,n,1)){
    Z=b+(z*(z+b)*((1+b-z)/(q*b)))
    z=Z
  }
  
  return (Z)
  
}

#Main:
Tr=0.8233 #reduced temperature given in question for butane #tr=t/tc
pr=0.2491#reduced pressure given in question for butane #pr=p/pc
w=0.2 #for butane
srk=(1+(0.480+1.574*w-0.176*w^2)*(1-Tr^0.5))^2 #soave redlich eqaution form #w is acentric factor
q=(0.42748*srk)/(0.08664*Tr)
b=0.08664*(pr/Tr)
message("q is ::",q)
message("b is ::",b)
print(paste("Final Z for saturated vapout is::",z1(b,q)))
a=z1(b,q)
v=(a*83.14*350)/9.4573
print(paste("Volume of saturated vapour is::",v))
##
print(paste("FINAL Z for saturated liq is:",z2(b,q)))
a2=z2(b,q)
v2=(a2*82.14*350)/(9.4573)
print(paste("Volume for the saturated liquid is: ",v2))

