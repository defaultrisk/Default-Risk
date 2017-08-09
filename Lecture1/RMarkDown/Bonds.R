# la courbe zero coupon


# les cash flows de mon obligation
# bond 2 ans, 2 paiements dans l'année, taux de 6 %, remboursement bullet, nominal de 1 MEUR
N<-10^6
T<-2
eta<-2
r<-0.06
# donner des vraies dates
dates<-seq(from=as.Date("01/01/2018", "%d/%m/%y"),to=as.Date("01/01/2020", "%d/%m/%y"),length.out=5)
cash_flows<-rep((-N*r),T*eta+1)
cash_flows[1]<-N
cash_flows[T*eta+1]<-cash_flows[T*eta+1]-N
barplot(cash_flows)

compound<-function(t){return(1/(1+r)^t)}

compoundings<-compound(dates)
priceofthebound<-(-compoundings[2:(T*eta+1)]%*%cash_flows[2:(T*eta+1)])

# bootstraping
# 

# version discontinue

# version continue



# calcul du spread de mon bond

# the implied default propability