library(readxl) 
library(gplots) 
library(agricolae)
library(sciplot)
library(PMCMRplus)
library(corrplot)
library(psych)
library(scales)
library(visreg)
citation()
dir()
setwd("C:/Users/Adamastor/Documents/Biologia/3Estatística no R")
tcc=read.csv("Dados_TCC.csv", h=T, stringsAsFactors = T, dec=",") 
summary(tcc)
str(tcc) 
plot(tcc)
md=colMeans(tcc[grep("inside",x=tcc$Local),c(3:6)],na.rm = T)
mf=colMeans(tcc[grep("outside",x=tcc$Local),c(3:6)],na.rm = T)
(médias=matrix(c(md,mf),byrow=T,nrow = 2,ncol=4,dimnames=list(c("inside","outside"),c("GMC","MMC","TP","MXR"))))
(prop=proportions(médias,2))
par(mfrow=c(1,1),mar=c(3,4,1,1),lwd=1)
barplot(height = prop, horiz = T, las=1, col = c("darkgreen", "darkred"), legend.text = T)

#####verificar se médias dos dados bióticos diferem entre os locais (2 locais = teste T ou kruskall + boxplot)
#TH_B = GMC
hist(tcc$GMC, probability = T)
lines(x=density(x=tcc$GMC, na.rm=T, adjust=2)) #quase
shapiro.test(tcc$GMC) #0,06 = normal
bartlett.test(tcc$GMC~tcc$Local) #0,7455 = homo ###teste t
t.test(tcc$GMC~tcc$Local) #0,002728 = diferem

par(mfrow=c(2,1),mar=c(2,5,1,1),lwd=1)
boxplot(tcc$GMC~tcc$Local)
boxplot(tcc$GMC~tcc$Local,axes=F, varwidth=T, xlab="",ylab="", border="black", col=c("darkgreen","darkred"), ylim=c(70,103), outline = T, main="", horizontal = F)
box(bty="o", lwd=2)
axis(side=2,las=1,font=2)
nomes=c("INSIDE","OUTSIDE")
pos=c(1,2)
axis(side = 1,at = pos, labels = nomes, las=1,font=9)
mtext(text="Gill Moisture", side=2,line=3.5,cex=0.9,font=7)
mtext(text="Content (%)", side=2,line=2.7,cex=0.9,font=7)
legend("top", legend=c("p-value=0,002728"), text.font = 2, bty="n", cex=0.55, text.col="gray25")

#MMC
hist(tcc$MMC, probability = T)
lines(x=density(x=tcc$MMC, na.rm=T, adjust=2)) #parece
shapiro.test(tcc$MMC) #0,002 = não normal
bartlett.test(tcc$MMC~tcc$Local) #0,047 = não homo ###kruskal
kruskal.test(tcc$MMC~tcc$Local) #0,000004813 = diferem

par(mfrow=c(1,1),mar=c(1,5,5,1),lwd=1)
boxplot(tcc$MMC~tcc$Local)
boxplot(tcc$MMC~tcc$Local,axes=F, varwidth=T, xlab="",ylab="", border="black", col=c("darkgreen","darkred"), ylim=c(70,100), outline = T, main="", horizontal = F)
box(bty="o", lwd=2)
axis(side=2,las=1,font=2)
nomes=c("INSIDE","OUTSIDE")
pos=c(1,2)
axis(side = 1,at = pos, labels= nomes, las=1,font=9)
mtext(text="Muscle Moisture", side=2,line=3.5,cex=0.9,font=7)
mtext(text="Content (%)", side=2,line=2.7,cex=0.9,font=7)
legend("top", legend=c("p-value=0,000004813"), text.font = 2, bty="n", cex=0.55, text.col="gray25")

#TP
hist(tcc$TP, probability = T)
lines(x=density(x=tcc$TP, na.rm=T, adjust=2)) #parece um pouco
shapiro.test(tcc$TP) #0,001 = não normal
bartlett.test(tcc$TP~tcc$Local) #0,000001765 = não homo ###kruskal
kruskal.test(tcc$TP~tcc$Local) #0,00000002328 = diferem

par(mfrow=c(1,1),mar=c(3,5,1,1),lwd=1)
boxplot(tcc$TP~tcc$Local)
boxplot(tcc$TP~tcc$Local,axes=F, varwidth=T, xlab="",ylab="", border="black", col=c("darkgreen","darkred"), ylim=c(15,140), outline = T, main="", horizontal = F)
box(bty="o", lwd=2)
axis(side=2,las=1,font=2)
nomes=c("INSIDE","OUTSIDE")
pos=c(1,2)
axis(side = 1,at = pos, labels = nomes, las=1,font=9)
mtext(text="Total Plasmatic Proteins (mg)", side=2,line=3,cex=1,font=7)
legend("topleft", legend=c("Kruskal-Wallis test demonstrated", "statistical difference", "(p-value=0,00000002328)"), text.font = 2, bty="n", cex=0.6, text.col="gray25")

#MXR
hist(tcc$MXR, probability = T)
lines(x=density(x=tcc$MXR, na.rm=T, adjust=2)) #não parece
shapiro.test(tcc$MXR) #0,0000038 = não normal
bartlett.test(tcc$MXR~tcc$Local) #0,000264 = não homo ##kruskal
kruskal.test(tcc$MXR~tcc$Local) #0,0000000000005607 = diferem

par(mfrow=c(1,1),mar=c(3,6,1,1),lwd=1)
boxplot(tcc$MXR~tcc$Local)
boxplot(tcc$MXR~tcc$Local,axes=F, varwidth=T, xlab="",ylab="", border="black", col=c("darkgreen","darkred"), ylim=c(6400,151000), outline = T, main="", horizontal = F)
box(bty="o", lwd=2)
axis(side=2,las=1,font=2)
nomes=c("INSIDE","OUTSIDE")
pos=c(1,2)
axis(side = 1,at = pos, labels = nomes, las=1,font=9)
mtext(text="Fluorescence/Mass of Tissue (rfu/mg)", side=2,line=4.5,cex=1,font=7)
legend("topleft", legend=c("Kruskal-Wallis test demonstrated", "statistical difference", "(p-value=0,0000000000005607)"), text.font = 2, bty="n", cex=0.6, text.col="gray25")

#####verificar se médias dos dados bióticos diferem entre os pontos (6 pontos = anova ou kruskall + boxplot)
#GMC
hist(tcc$GMC, probability = T)
lines(x=density(x=tcc$GMC, na.rm=T, adjust=2))
shapiro.test(tcc$GMC) #normal
bartlett.test(tcc$GMC~tcc$Site) #0,02 = não homo
anova(lm(formula=tcc$GMC~tcc$Site)) #0,0000004971 = diferença
(tkTHB=HSD.test(y=lm(formula=tcc$GMC~tcc$Site),trt="tcc$Site",group=T)) 
boxplot(tcc$GMC~tcc$Site) #a,a,ab,a,bc,c ##P8 próximo aos de dentro, p9 e p10 mais isolados

par(mfrow=c(2,1),mar=c(2,5,1,1),lwd=0.75)
boxplot(tcc$GMC~tcc$Site,axes=F, varwidth=T, xlab="",ylab="", border="black", col=c("darkgreen","darkgreen","darkgreen","darkred","darkred","darkred"), ylim=c(70,103), outline = T, main="", horizontal = F)
box(bty="o", lwd=2)
axis(side=2,las=1,font=2)
nomesP=c("P5","P6","P7","P8","P9","P10")
posP=c(1,2,3,4,5,6)
axis(side = 1,at = posP, labels = nomesP, las=1,font=9)
mtext(text="Gill Moisture", side=2,line=3.5,cex=0.9,font=7)
mtext(text="Content (%)", side=2,line=2.7,cex=0.9,font=7)
tapply(tcc$GMC,tcc$Site,max, na.rm=T)
text(x=c(1,2,3,4,5,6),y=c(91.3,94.3,98.1,89.1,97.1,97.9)+3,labels=c("a","a","ab","a","bc","c"),cex=1)

#THM
hist(tcc$MMC, probability = T)
lines(x=density(x=tcc$MMC, na.rm=T, adjust=2))
shapiro.test(tcc$MMC) #0,002 = não normal
bartlett.test(tcc$MMC~tcc$Site) #0,001 = não homo
anova(lm(formula=tcc$MMC~tcc$Site)) #0,000181 = diferença
(tkTHM=HSD.test(y=lm(formula=tcc$MMC~tcc$Site),trt="tcc$Site",group=T)) 
boxplot(tcc$MMC~tcc$Site) #ab,a,ab,ab,bc,c

#par(mfrow=c(1,1),mar=c(3,5,5,1),lwd=0.75)
boxplot(tcc$MMC~tcc$Site,axes=F, varwidth=T, xlab="",ylab="", border="black", col=c("darkgreen","darkgreen","darkgreen","darkred","darkred","darkred"), ylim=c(70,100), outline = T, main="", horizontal = F)
box(bty="o", lwd=2)
axis(side=2,las=1,font=2)
nomesP=c("P5","P6","P7","P8","P9","P10")
posP=c(1,2,3,4,5,6)
axis(side = 1,at = posP, labels = nomesP, las=1,font=9)
mtext(text="Muscle Moisture", side=2,line=3.5,cex=0.9,font=7)
mtext(text="Content (%)", side=2,line=2.7,cex=0.9,font=7)
tapply(tcc$MMC,tcc$Site,max, na.rm=T)
text(x=c(1,2,3,4,5,6),y=c(81,85.3,85,88.2,88.2,93.5)+2.7,labels=c("ab","a","ab","ab","bc","c"),cex=1)

#TP
hist(tcc$TP, probability = T)
lines(x=density(x=tcc$TP, na.rm=T, adjust=2))
shapiro.test(tcc$TP) #0,001 = não normal
bartlett.test(tcc$TP~tcc$Site) #0,00001 = não homo
anova(lm(formula=tcc$TP~tcc$Site)) #0,00000000000002783 = diferença
(tkPT=HSD.test(y=lm(formula=tcc$TP~tcc$Site),trt="tcc$Site",group=T)) 
boxplot(tcc$TP~tcc$Site) #a,a,a,c,b,bc #dentro e fora muito mais isolados
 
par(mfrow=c(1,1),mar=c(3,5,1,1),lwd=0.75)
boxplot(tcc$TP~tcc$Site,axes=F, varwidth=T, xlab="",ylab="", border="black", col=c("darkgreen","darkgreen","darkgreen","darkred","darkred","darkred"), ylim=c(20,140), outline = T, main="", horizontal = F)
box(bty="o", lwd=2)
axis(side=2,las=1,font=2)
nomesP=c("P5","P6","P7","P8","P9","P10")
posP=c(1,2,3,4,5,6)
axis(side = 1,at = posP, labels = nomesP, las=1,font=9)
mtext(text="Total Plasmatic Protein (mg)", side=2,line=3,cex=1,font=7)
tapply(tcc$TP,tcc$Site,max, na.rm=T)
text(x=c(1,2,3,4,5,6),y=c(34,35.2,35.6,98,98,87.7)+7,labels=c("a","a","a","c","b","bc"),cex=1)

#MXR
hist(tcc$MXR, probability = T)
lines(x=density(x=tcc$MXR, na.rm=T, adjust=2))
shapiro.test(tcc$MXR) #0,000003 = não normal
bartlett.test(tcc$MXR~tcc$Site) #0,00000008 = não homo
anova(lm(formula=tcc$MXR~tcc$Site)) #0,0000000000005 = diferença
(tkMXR=HSD.test(y=lm(formula=tcc$MXR~tcc$Site),trt="tcc$Site",group=T)) 
boxplot(tcc$MXR~tcc$Site) #a,a,a,b,b,b #dentro e fora 100% isolados

dunnMXR=kwAllPairsDunnTest(formula=tcc$MXR~tcc$Site)
summary(dunnMXR) #ad,b,ab,c,c,cd

par(mfrow=c(1,1),mar=c(3,6,1,1),lwd=0.75)
boxplot(tcc$MXR~tcc$Site,axes=F, varwidth=T, xlab="",ylab="", border="black", col=c("darkgreen","darkgreen","darkgreen","darkred","darkred","darkred"), ylim=c(4000,165000), outline = T, main="", horizontal = F)
box(bty="o", lwd=2)
axis(side=2,las=1,font=2)
nomesP=c("P5","P6","P7","P8","P9","P10")
posP=c(1,2,3,4,5,6)
axis(side = 1,at = posP, labels = nomesP, las=1,font=9)
mtext(text="Fluorescence/Mass of Tissue (rfu/mg)", side=2,line=4.5,cex=1,font=7)
tapply(tcc$MXR,tcc$Site,max, na.rm=T)
text(x=c(1,2,3,4,5,6),y=c(68400,31371,44811,88000,150658,121050)+10000,labels=c("a","a","a","b","b","b"),cex=1)

#####correlação dados bióticos e abióticos
corBIO=tcc[,3:6] 
cor(na.omit(corBIO)) 
corrplot(cor(na.omit(corBIO))) #alta correlação entre PT e MXR
cor.test(tcc$MXR,tcc$TP) #0,6 = moderada e positiva
par(mfrow=c(1,1),mar=c(3,6,5,1),lwd=0.75)
corrplot(cor(na.omit(corBIO)), type="lower", order = "original", method="circle", bg= "ghostwhite", tl.pos = "lt", add=F) 
corrplot(cor(na.omit(corBIO)), type="upper", order = "original", method="number", bg= "ghostwhite", tl.pos= "n", add=T)
mtext(text="Correlação Dados Bióticos", side=2,line=1,cex=1.5,font=7)

pairs.panels(x=tcc[,3:12]) #MXR-Nitrato = -0,62/ THB-Amônia = 0,55/ THB-OD = -0,62

corABIO=tcc[,7:12] 
cor(na.omit(corABIO)) 
corrplot(cor(na.omit(corABIO))) #alta correlação entre Amon-OD e fósf-nit
cor.test(tcc$Oxig_dissolv,tcc$Ammonia) #-0,82 = forte e negativa
cor.test(tcc$Nitrate,tcc$Phosphorus) #0,69 = forte e positiva
corrplot(cor(na.omit(corABIO)), type="lower", order = "original", method="circle", bg= "ghostwhite", tl.pos = "lt", title = "", add=F, tl.text=1) 
corrplot(cor(na.omit(corABIO)), type="upper", order = "original", method="number", bg= "ghostwhite", tl.pos= "n", add=T)
mtext(text="Correlação Dados Abióticos", side=2,line=1,cex=1.5,font=7)

#regressão
mlMXRTP=lm(tcc$MXR~tcc$TP)
summary(mlMXRTP) #674.7*x+4050 e R²=0,3661
par(mfrow=c(1,1),mar=c(5,5,5,1),lwd=1, bg="white", col="black")
plot(tcc$MXR~tcc$TP)
plot(tcc$MXR~tcc$TP, ylim=c(0,151000), pch=25,cex=2,bg="yellow",col="forestgreen", ylab = "", xlab="")
curve(647.7*x+4050, add = T, col="darkcyan", lwd=2)
legend("topleft", legend= c("Y=674.7x+4050","R²=0,3661"), bty="n",text.font = 6, cex = 0.9, text.col = "darkcyan")
mtext(text = "Fluorescência/massa (rfu/mg)",side = 2,line = 3,font = 7,cex=1,las=0)
mtext(text = "Proteínas Totais (mg)",side = 1,line = 3,font = 7,cex=1.2,las=0)
mtext(text="Relação entre Proteínas Plasmáticas", side=3,line=3,cex=2,font=9)
mtext(text="e atividade do fenótipo MXR", side=3,line=1,cex=2,font=9)

mlOA=lm(tcc$Oxig_dissolv~tcc$Ammonia)
summary(mlOA) #-0,012025*x+8.569657 e R²=0,6656
par(mfrow=c(1,1),mar=c(5,5,5,1),lwd=1, bg="white", col="black")
plot(tcc$Oxig_dissolv~tcc$Ammonia)
plot(tcc$Oxig_dissolv~tcc$Ammonia, ylim=c(0,9), pch=25,cex=2,bg="yellow",col="forestgreen", ylab = "", xlab="")
curve(-0.012025*x+8.569657, add = T, col="darkcyan", lwd=2)
legend("bottomleft", legend= c("Y=-0,012025x+8.569657","R²=0,6656"), bty="n",text.font = 6, cex = 0.9, text.col = "darkcyan")
mtext(text = "Oxigênio Dissolvido (mg/L)",side = 2,line = 3,font = 7,cex=1.2,las=0)
mtext(text = "Amônia (ug/L)",side = 1,line = 3,font = 7,cex=1.2,las=0)
mtext(text="Relação entre Oxigênio Dissolvido", side=3,line=3,cex=2,font=9)
mtext(text="e Amônia", side=3,line=1,cex=2,font=9)

mlNP=lm(tcc$Nitrate~tcc$Phosphorus)
summary(mlNP) #0,002885*x+45400 e R²=0,4706
par(mfrow=c(1,1),mar=c(5,5,5,1),lwd=1, bg="white", col="black")
plot(tcc$Nitrate~tcc$Phosphorus)
plot(tcc$Nitrate~tcc$Phosphorus, ylim=c(45300,45600), pch=25,cex=2,bg="yellow",col="forestgreen", ylab = "", xlab="")
curve(0.002885*x+45400, add = T, col="darkcyan", lwd=2)
legend("bottomright", legend= c("Y=0,002885x+45400","R²=0,4706"), bty="n",text.font = 6, cex = 0.9, text.col = "darkcyan")
mtext(text = "Nitrato (ug/L)",side = 2,line = 3,font = 7,cex=1.2,las=0)
mtext(text = "Fósforo (ug/L)",side = 1,line = 3,font = 7,cex=1.2,las=0)
mtext(text="Relação entre Nitrato", side=3,line=3,cex=2,font=9)
mtext(text="e Fósforo", side=3,line=1,cex=2,font=9)

mlNMXR=lm(tcc$Nitrate~tcc$MXR)
summary(mlNMXR) #-0,001527*x+45520 e R²=0,3945
par(mfrow=c(1,1),mar=c(5,5,5,1),lwd=1, bg="white", col="black")
plot(tcc$Nitrate~tcc$MXR)
plot(tcc$Nitrate~tcc$MXR, ylim=c(45200,45600), pch=25,cex=2,bg="yellow",col="forestgreen", ylab = "", xlab="")
curve(-0.001527*x+45520, add = T, col="darkcyan", lwd=2)
legend("topright", legend= c("Y=-0,001527x+45520","R²=0,3945"), bty="n",text.font = 6, cex = 0.9, text.col = "darkcyan")
mtext(text = "Nitrato (ug/L)",side = 2,line = 3,font = 7,cex=1.2,las=0)
mtext(text = "Atividade do fenótipo MXR (rfu/mg)",side = 1,line = 3,font = 7,cex=1.2,las=0)
mtext(text="Relação entre Nitrato", side=3,line=3,cex=2,font=9)
mtext(text="e atividade do fenótipo MXR", side=3,line=1,cex=2,font=9)

mlGMCA=lm(tcc$GMC~tcc$Ammonia)
summary(mlGMCA) #0,05242*x+81,50447 e R²=0,306
par(mfrow=c(1,1),mar=c(5,5,5,1),lwd=1, bg="white", col="black")
plot(tcc$GMC~tcc$Ammonia)
plot(tcc$GMC~tcc$Ammonia, ylim=c(75,100), pch=25,cex=2,bg="yellow",col="forestgreen", ylab = "", xlab="")
curve(0.05242*x+81.50447, add = T, col="darkcyan", lwd=2)
legend("bottomright", legend= c("Y=0,05242x+81,50447","R²=0,306"), bty="n",text.font = 6, cex = 0.9, text.col = "darkcyan")
mtext(text = "Teor Hídrico da Brânquia (%)",side = 2,line = 3,font = 7,cex=1,las=0)
mtext(text = "Amônia (ug/L)",side = 1,line = 3,font = 7,cex=1.2,las=0)
mtext(text="Relação entre Amônia", side=3,line=3,cex=2,font=9)
mtext(text="e Teor Hídrico da Brânquia", side=3,line=1,cex=2,font=9)

mlGMCOD=lm(tcc$GMC~tcc$Oxig_dissolv)
summary(mlGMCOD) #-3,911*x+115,680 e R²=0,3884
par(mfrow=c(1,1),mar=c(5,5,5,1),lwd=1, bg="white", col="black")
plot(tcc$GMC~tcc$Oxig_dissolv)
plot(tcc$GMC~tcc$Oxig_dissolv, ylim=c(75,100), pch=25,cex=2,bg="yellow",col="forestgreen", ylab = "", xlab="")
curve(-3.911*x+115.680, add = T, col="darkcyan", lwd=2)
legend("bottomleft", legend= c("Y=-3,911x+115,680","R²=0,3884"), bty="n",text.font = 6, cex = 0.9, text.col = "darkcyan")
mtext(text = "Teor Hídrico da Brânquia (%)",side = 2,line = 3,font = 7,cex=1,las=0)
mtext(text = "Oxigênio Dissolvido (mg/L)",side = 1,line = 3,font = 7,cex=1.2,las=0)
mtext(text="Relação entre Oxigênio Dissolvido", side=3,line=3,cex=2,font=9)
mtext(text="e Teor Hídrico da Brânquia", side=3,line=1,cex=2,font=9)

