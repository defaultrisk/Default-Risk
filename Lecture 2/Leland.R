####### simulation d'une diffusion #######

### Paramètre de la diffusion
# On veut simuler : dVt/Vt=(r-delta)dt + sigma d Wt
delta=0.06
r=0.06
sigma=0.4

### Paramètre de précision de la simulation

n = 100 # nombre de pas
T = 1 # horizon temporel
V0= 100 # valeur initiale

V=rep(0,n+1)
V[1]=V0
tick=T/n
for (i in (2:(n+1)))
{
  V[i]=V[i-1]*(1+(r-delta)*(tick)+sigma*rnorm(1,0,tick))
}

plot(seq(from=0,to=T,by=T/n), V, type="l", lwd=2)

