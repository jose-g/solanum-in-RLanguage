source("thermalTime.R")
# levantamos toda la base de datos de clima en un data frame
#climate <- read.csv("climate.csv", header = TRUE)   # abrir un archivo csv
climate <- read.table("climate.prn", header = TRUE)   # abrir un archivo ascii
# juntamos la fecha y se agrega como una nueva columna
Date<-as.Date(paste(climate$Year,"-",climate$Month,"-",climate$Day,sep=""))
climate<-data.frame(Date,climate)
# definimos periodo de simulacion
sowing<-"1995-07-17" # formato por defecto con el que se trabaja en R (Año-Mes-Dia)
harvest<-"1995-12-7"
# seleccionamos datos de clima correspondiente al periodo de simulacion
climate <- climate[climate[, 1] >= as.Date(sowing) & climate[, 1] <= as.Date(harvest), ]
# retiro las columnas Date, Tmin y Tmax del dataframe y los guardo en vectores por separado
DATE<-as.Date(climate$Date)
TMIN<-as.numeric(climate$Tmin)
TMAX<-as.numeric(climate$Tmax)
EDay=13
TT<-thermalTime(DATE,TMIN,TMAX,sowing,harvest,EDay,c(0, 12, 24, 35)) # se calcula el TT
climate<-data.frame(climate,TT=TT$tt) # agrego la columna TT calculada al df climate

# INPUTS
DMcont=0.21
A_0=0.77
Tu_0=880
b=175
RUE=2.8
EDay=13
v=0.075
wmax=0.89
tm=536
te=1045

Pc = 18
TDM = 1.0E-37
cTII = 0
Tac = 1.0E-05
bsearch_tt=TRUE
tt=0.0
w = 0.2
Tb_0 = 4
Tu_1 = 28
To_0 = 15
a = (log(2))/log((Tu_1-Tb_0)/(To_0-Tb_0))
PDay=-1
rnd=runif(1)
rdm = (log((1+rnd)/(1-rnd)))/1.82 ## log: es logaritmo natural

verificar_densidad = 4.17
timeDuration<-as.numeric(as.Date(harvest)-as.Date(sowing) +1)
# OUTPUTS
tdm <- vector(mode="numeric", length=timeDuration)
dty <- vector(mode="numeric", length=timeDuration)
fty <- vector(mode="numeric", length=timeDuration)
cc <- vector(mode="numeric", length=timeDuration)

#############################################################################################
#                                                                                           #
#                                 Inicio de la simulacion                                   #
#                                                                                           #
#############################################################################################
#timeDuration=1
for(i in 1:timeDuration)
{
  canopy = wmax*exp(-1*(tm/(Tac*verificar_densidad)))*(1+(te-Tac)/(te-tm))*((Tac/te)^(te/(te-tm)))
  canopy1 = rdm*v*canopy+canopy
  Part = A_0*exp(-1*(exp((-1*(Tac-Tu_0))/b)))
  DTY1 = TDM*Part
  tt = climate$TT[i];
  
  if(cTII>20.0 & bsearch_tt)
  {
    tt_fixed=tt
    bsearch_tt=FALSE
  }
  else
  {
    tt_fixed=0
  }
  if(bsearch_tt)
  {
    Tu_cTII=1.0
  }
  else
  {
    Tu_cTII=(tt_fixed+b)/Tu_0
  }
  Part_cTII = A_0*exp(-1*(exp((-1*(Tac-Tu_0*Tu_cTII))/b)))
  HI_cTII = ifelse(cTII<=20,0,Part_cTII)
  DTY2 = TDM*HI_cTII
  DTY = ifelse(Tu_cTII<=1,DTY1,DTY2)
  FTY = DTY/DMcont
  Tmax = climate$Tmax[i]
  Tmin = climate$Tmin[i]
  Tav = (Tmax+Tmin)/2
  Tac = tt
  Tindex = ifelse(Tav<Tb_0,0,ifelse(Tav>Tu_1,0,(2*((Tav-Tb_0)^a)*((To_0-Tb_0)^a)-((Tav-Tb_0)^(2*a)))/((To_0-Tb_0)^(2*a))))
  PAR = climate$Rad[i]*0.5
  N = climate$sunsh[i]
  Pindex = ifelse(N>Pc,exp(-1*(w*(N-Pc))),1)
  TII = Tindex*Pindex
   
  PDay=PDay+1
  DAE = ifelse(PDay>=EDay,PDay-EDay,0)
  CC = ifelse(DAE<=0,0,ifelse(canopy1>0,canopy1,10^(-6)))
  dW = (RUE*CC*PAR)/100
#   /*    outputs    */
  tdm[i]=TDM;
  dty[i]=DTY;
  fty[i]=FTY;
  cc[i]=CC;
#   /*    CONTAINERS    */
   TDM = TDM+dW;
   cTII = cTII+TII;
}
climate<-data.frame(climate,tdm,dty,fty,cc)






