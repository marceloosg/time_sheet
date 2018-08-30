library(data.table)

ponto=fread("GLogData_001.csv")
colnames(ponto)[9]="Tipo"
helo=ponto[Name=="Heloisa"]
feriados=fread("feriados.csv",sep= ";")
feriadosSP=fread("feriados-sp.csv",sep= "-")

helo$Data=as.Date(helo$Data)
helo$h=as.integer(sapply(helo$Hora,function(x) strsplit(x,":")[[1]][1]))
helo$m=as.integer(sapply(helo$Hora,function(x) strsplit(x,":")[[1]][2]))

umaEntrada=merge(helo,helo[ ,length(unique(No)),Data][V1==1][,-c("V1")],by="Data")

saida=helo[No %in% umaEntrada[h < 12]$No]
saida$No=saida$No+1000
saida$Tipo="Saída"
saida$Hora="17:30:00"
saida$h=17
saida$m=30
saida


duasEntradas=merge(helo,helo[ ,length(unique(No)),Data][V1==2][,-c("V1")],by="Data")
tresEntradas=merge(helo,helo[ ,length(unique(No)),Data][V1==3][,-c("V1")],by="Data")

almoco=tresEntradas[Tipo=="Retorno"]
almoco$No=almoco$No+1000
almoco$Tipo = "Almoço"
almoco$h = as.integer((almoco$h*60+almoco$m-30)/60)
almoco$m = (almoco$m+30) %% 60

m10=function(i){
  ifelse(i<10,paste("0",i,sep=""),as.character(i))
}
almoco[,Hora:=paste(m10(h),":",m10(m),":00",sep = "")]

final=rbind(helo,saida,almoco)
horas=final[,.(Turno=(.SD[Tipo=="Saída"]$h*60-.SD[Tipo=="Entrada"]$h*60+
                        .SD[Tipo=="Saída"]$m-.SD[Tipo=="Entrada"]$m)/60, 
         Almoco=(.SD[Tipo=="Retorno"]$h*60-.SD[Tipo=="Almoço"]$h*60+
           .SD[Tipo=="Retorno"]$m-.SD[Tipo=="Almoço"]$m)/60),Data]
horas[is.na(Almoco)]$Almoco=-0.5
Dias=data.table(Data=seq(min(final$Data),Sys.Date(),1))
#Dias=merge(Dias,feriados[,.(Data=as.Date(Data,format="%d/%m/%y"),nacional=1)],by="Data",all.x=T)
Dias=merge(Dias,feriadosSP[,.(Data=as.Date(Data,format="%d/%m/%Y"),feriado=1)],by="Data",all.x=T)
Dias[,dia_semana:=weekdays(Data)]

Dias=merge(Dias,horas,by="Data",all.x=T)
Dias[is.na(feriado),feriado:=0]
Dias[is.na(Turno),Turno:=0]
Dias[is.na(Almoco),Almoco:=0]
Dias$carga=0
Dias[feriado==0 & !dia_semana %in% c("sábado","domingo"), carga:=8]
Dias[,Horas_Realizadas:=Turno-Almoco]
Dias[,Horas_Extras:=ifelse(Horas_Realizadas > carga +0.16,Horas_Realizadas - carga,0)]
Dias[,Horas_Devidas:=ifelse(Horas_Realizadas < carga -0.16,-Horas_Realizadas + carga,0)]



sumario=Dias[,.(saldo=sum(Horas_Extras-Horas_Devidas),Horas_Realizadas=sum(carga +Horas_Extras-Horas_Devidas),carga_Turno=sum(carga),feriados=sum(feriado),dias_uteis=sum(carga==8)),month(Data)]

sumario
sum(sumario$saldo)

output=merge(Dias,final[,.(Entrada_1=Hora[Tipo=="Entrada"],Saída_1=Hora[Tipo=="Almoço"],Entrada_2=Hora[Tipo=="Retorno"],Saída_2=Hora[Tipo=="Saída"]),Data],by="Data",all.x=T)
output=output[,.(Mes=month(Data),Data,feriado=ifelse(feriado==1,"feriado",""),dia_semana,Entrada_1,Saída_1,Entrada_2,Saída_2,saldo=Horas_Extras-Horas_Devidas,Assinatura="_______________________________")]

hf=function(h){
  s=sapply(h,function(j){ifelse(j>0,"+","-")})
  h=abs(h)
  paste(s,paste(m10(as.integer(h)),
            m10(as.integer(60*(h-as.integer(h)))),sep=":"),sep="")
}


for(m in unique(output$Mes)){
  nome=paste("output",m10(m),"2018.csv",sep="_")
  print(nome)
  om=output[Mes==m][,-c("Mes")]
  om$Data=format(om$Data,format="%d/%m/%y")
  subtotal=om[,lapply(.SD,function(x){ifelse(class(x)==class(1),sum(x),NA)})]
  subtotal$Data="Total"
  om$saldo=hf(om$saldo)
  subtotal$saldo=hf(subtotal$saldo)
  fwrite(om,nome)
  fwrite(subtotal,nome,append = T)
}
