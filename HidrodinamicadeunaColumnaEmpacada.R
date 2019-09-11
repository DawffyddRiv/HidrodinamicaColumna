# El siguiente Script se elabor? para la pr?ctica de "Hidrodin?mica de una columna empacada"
#como una gu?a inicial en el manejo de datos para los estudiantes de la asignatura de LIQ 3

#Primera parte: Estimación de las carácteristicas geometricas del equipo
Dext=Hidrodi_P1$`Do [cm]`
Do=mean(Dext)
Di=mean(Hidrodi_P1$`Di [cm]`)
L=mean(Hidrodi_P1$`L [cm]`)

Ro_cm=Do/2
Ri_cm=Di/2
A_bases=2*pi*((Ro_cm^2)-(Ri_cm^2))
A_int=pi*Di*L       #[cm^2]          
A_ext=pi*Do*L       #[cm^2]
Atot_pza=A_bases+A_int+A_ext #cm^2
Peso_empaques=190.9    #[g]
#Fracc_huecos=0.41   #e
Fracc_huecos=0.69
Dens_aparente=0.38  # [g/cm^3]
Dens_real=0.65      # [g/cm^3]
Empaqporvol=1.93    #Empaques por cm^3

av=Atot_pza*Empaqporvol
LTorre=106  #[cm]
Atransflujo=pi*((5.08/2)^2)   #[cm^2]
Vol_columna=Atransflujo*LTorre   #[cm^3]
VolHuecosCol=(206/300)*Vol_columna  #[cm^3]
A_transv_entre_empaques=(Atransflujo/Vol_columna)*VolHuecosCol #[cm^2]

#Segunda Parte: Hidrodinámica

flujos<-data.frame('PPM 25'=c(152,154),'PPM 50'=c(80.92,80.3),'PPM 75'=c(61,58),'PPM 100'=c(46.23,46.05))
#View(flujos)
promedio<-c(apply(flujos, 2, mean))
prom_horas=promedio/3600
flujo_Lxs=0.2/prom_horas #[L/s]
flujovsPPM<-data.frame(PPM=c(25,50,75,100),flujos_Lh=flujo_Lxs)
#View(flujovsPPM)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(viridis)
ggplot(data=flujovsPPM, aes(x=PPM, y=flujos_Lh))+geom_point()
regress<-lm(flujos_Lh ~ PPM, data = flujovsPPM)
summary(regress)
plot(flujovsPPM$PPM, flujovsPPM$flujos_Lh, xlab='PPM', ylab='Flujo L/h')
abline(regress)
ggplot(data=flujovsPPM, aes(x=PPM, y=flujos_Lh))+geom_point(color='green')+ geom_smooth(method="lm", se=TRUE, color="red")+ stat_regline_equation()





Taire_liq=23+273.15  #[K]
Taire_std=25+273.15  #[K]
P_atmosferica=0.77 #[atm]
densidad_aire=(-0.0035*(Taire_liq-273.15))+1.2791 #[Kg/m^3]
gravedad=127098720 #m/h^2
TH2O=21+273.15  #K


viscosidad_agua=-10.2158+((1.7925 * 10^3)/Taire_liq)+((1.773 * 10^-2)*Taire_liq)+((-1.2631 * 10^-5)*Taire_liq)

densi_agua_gcm3=(0.3471)*(0.274^-((1-((TH2O)/647.13))^0.28571)) #[g/cm^3]
densidad_agua=densi_agua_gcm3*(100^3/1000)#Kg/m^3 

HidrodinamicaP2$`Q_[L/h]`=HidrodinamicaP2$`%Aire`*0.072  
HidrodinamicaP2$`Q_[m3/h]`=HidrodinamicaP2$`Q_[L/h]`* (Taire_liq/Taire_std)*(1/P_atmosferica) 
HidrodinamicaP2$`G [Kg/h]`=HidrodinamicaP2$`Q_[m3/h]`*densidad_aire    
HidrodinamicaP2$`G_prima[kg/h*m2]`=HidrodinamicaP2$`G [Kg/h]`/(A_transv_entre_empaques/10000)    
HidrodinamicaP2$`L_[kg/h]`=HidrodinamicaP2$`Litros/hora`*(densidad_agua*0.001)
HidrodinamicaP2$`L_prima[kg/h*m2]`=HidrodinamicaP2$`L_[kg/h]`/(A_transv_entre_empaques*0.0001)
HidrodinamicaP2$`G2(aV/E3)muL0.2/g*roG/roL)`=((HidrodinamicaP2$`G_prima[kg/h*m2]`^2)*((av/(Fracc_huecos^3))*100)*(viscosidad_agua^0.2))/(gravedad*densidad_agua*densidad_aire)

#HidrodinamicaP2$`G2(aV/E3)muL0.2/g*roG/roL)`<-NULL  #Para borrar columna
HidrodinamicaP2$`(Lprima/Gprima)`=(HidrodinamicaP2$`L_prima[kg/h*m2]`/HidrodinamicaP2$`G_prima[kg/h*m2]`)
ratio_densidades<-(densidad_aire/densidad_agua)^0.5
HidrodinamicaP2$`(Lprima/Gprima)*(roG/roL)^1/2`=HidrodinamicaP2$`(Lprima/Gprima)`*ratio_densidades
HidrodinamicaP2$`DeltaP/L [Kgf/m^2 m]`=((HidrodinamicaP2$`PF [cm H2O]`-HidrodinamicaP2$`PD [cm H2O]`)*9.9997)/(LTorre/100) 


seco_empaque=HidrodinamicaP2[0:8,c("DeltaP/L [Kgf/m^2 m]","G [Kg/h]")]
ggplot(data=seco_empaque , aes(x=`G [Kg/h]`, y=`DeltaP/L [Kgf/m^2 m]`,colour="red"))+geom_point() +scale_x_log10()+scale_y_log10()

ggplot(data=HidrodinamicaP2 , aes(x=`G [Kg/h]`, y=`DeltaP/L [Kgf/m^2 m]`,colour=`Litros/hora`,group = as.factor(`Litros/hora`)))+
  geom_point() +scale_x_log10()+scale_y_log10()# + geom_smooth(method="lm",formula= (y ~ exp(x)), se=FALSE,aes(fill=`Litros/hora`,colour = `Litros/hora`))+scale_x_log10()+scale_y_log10()

#--------------------Hasta qui vamos
#ggplot(data=HidrodinamicaP2 , aes(x=`Log_G`, y=`Log_DeltaP/L`,colour=`Litros/hora`,group = as.factor(`Litros/hora`)))+geom_point() + geom_smooth(method="lm",formula= (y ~ exp(x)), se=FALSE,aes(fill=`Litros/hora`,colour = `Litros/hora`))

#Elaboraci?n de la gr?fica de Lobo
Lobo<-data.frame(`Litros/hora`=c(18,18,18,18,18,18,18,18,18),Gprima2=c(2.6,2.2,1.2,0.44,0.06,0.01,0.0045,0.0008,0.00001),`L/Gxrog/rol`=c(0.01, 0.02, 0.05,  0.1, 0.3,  0.7,  1 ,   2 ,  7.8))
dt<-data.frame(`Litros/hora`=c(18,18,18,18,18,18,18,18,18),Gprima2=c(2.6,2.2,1.2,0.44,0.06,0.01,0.00045,0.0008,0.00001),`L/Gxrog/rol`=c(0.005, 0.018, 0.04, 0.09, 0.27, 0.6,  0.9 , 1.9 ,7.2))

ggplot(data=Lobo , aes(x=Gprima2, y=L.Gxrog.rol))+geom_line()+scale_x_log10()+scale_y_log10()+geom_line(data = dt,aes(x=Gprima2, y=L.Gxrog.rol))+#geom_line()+scale_x_log10()+scale_y_log10()
  
  
  ggplot(data=HidrodinamicaP2 , aes(x=`(Lprima/Gprima)*(roG/roL)^1/2`,y=`G2(aV/E3)muL0.2/g*roG/roL)`,color=`Litros/hora`,group = as.factor(`Litros/hora`)))+ 
  geom_point() + geom_smooth(method="lm",formula= (y ~ exp(x)),se=FALSE,aes(fill=`Litros/hora`,colour = `Litros/hora`))+
  scale_x_log10()+scale_y_log10()#+geom_line(data = Lobo,aes(x=Gprima2, y=L.Gxrog.rol))

coloreando<-c("#FFDB6D", "#C4961A", "#F4EDCA", 
              "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")

ggplot(data=HidrodinamicaP2 , aes(x=`(Lprima/Gprima)*(roG/roL)^1/2`,y=`G2(aV/E3)muL0.2/g*roG/roL)`))+
  geom_point()+
  geom_line(aes(group=`Litros/hora`,colour=`Litros/hora`))+
  scale_x_log10()+scale_y_log10() +
  scale_color_viridis_c(option = "D")+
  geom_line(data = Lobo,aes(x=Gprima2, y=L.Gxrog.rol), col="red")

hidroP2_B=HidrodinamicaP2[HidrodinamicaP2>0,]

ggplot(data=hidroP2_B , aes(x=`(Lprima/Gprima)*(roG/roL)^1/2`,y=`G2(aV/E3)muL0.2/g*roG/roL)`))+
  geom_line(aes(group=`Litros/hora`,colour=`Litros/hora`))+
  geom_point(aes(colour=`Litros/hora`))+
  scale_color_viridis(option = "D")+
  geom_line(data = Lobo,aes(x=Gprima2, y=L.Gxrog.rol), col="red")+
  scale_x_log10()+scale_y_log10() 
