library("openxlsx")
library(dplyr)
library(corrplot)
library(Metrics)
library(ggplot2)

dane <- read.xlsx('kolokwium_gr3.xlsx')
View(dane)

#wyœwietlam podstawowe statystyki:
summary(dane)
dim(dane) #wymiar
colnames(dane)

sapply(dane,class) #nie wszystkie dane sa numeryczne - Date i Precip.Type nie mog¹ zostaæ u¿yte w modelu, 

dane<-select(dane,-Date,-Precip.Type)

cor(dane)
corrplot(cor(dane), method = "number", type = "lower")
#Po wywolaniu macierzy korelacji mozna zauwazyc ze korelacja zmiennych: 
#temperature, humidity i visibility s¹ najwiêksze, wiêc tylko ich potrzebujemy

#skracam nazwy kolumn
colnames(dane) <-c("Temperature","Apparent.Temperature","Humidity","Wind.Speed","Visibility")

#zostawiam 3 zmienne z najwiêksz¹ korelacj¹ i na ich podstawie bede tworzy³a modele
dane<-select(dane,-Wind.Speed)

#Usuwam wszystkie wiersze w których znajduje siê jedna lub wiêcej brakuj¹ca wartoœæ
dane<-dane[complete.cases(dane),]


#Dziele dane na zbior testowy i treningowy
library(caTools)
#prawda - treningowy
#falsz - testowy
samp<-sample.split(dane, SplitRatio = 0.8, group = NULL )
train<-dane[samp==TRUE, ]
test<-dane[samp==FALSE, ]

#---------------------------------------------
#model predykcji odczuwalnej temperatury (Apparent Temperature)
model <- lm(Apparent.Temperature ~ 1, train)
m<-rmse(train$Apparent.Temperature,predict(model))
m #10.78252
p<-predict(model,test)
pm<-rmse(test$Apparent.Temperature,p)
pm #10.72695
summary(model)

#------------------------------------------------------------
model1 <- lm(Apparent.Temperature ~ Temperature, train)
m1<-rmse(train$Apparent.Temperature,predict(model1))
m1 #1.337151
p1<-predict(model1,test)
pm1<-rmse(test$Apparent.Temperature,p1)
pm1 #1.339487
summary(model1) #R-squared:  0.9846

windows()
par(mfrow=c(1,2))
plot(train$Temperature,train$Apparent.Temperature)
abline(model2,col="red")

plot(test$Temperature,test$Apparent.Temperature)
abline(model2,col="blue")

#Blad sredniokwadratowy jest bardzo niski (1,3) co oznacza ze otrzymane wyniki niewiele roznia sie od wynikow z danych
#Wspolczynnik R^2 jest bliski 1 (0.9846) wiêc model jest bardzo dobry
#z wykresów widaæ ¿e zmienne sa bardzo silnie skorelowane, poniewaz ukladaja sie w prawie idealnej linii

#-----------------------------------------------
model2 <- lm(Apparent.Temperature ~ Visibility, train)
m2<-rmse(train$Apparent.Temperature,predict(model2))
m2 #10.14036
p2<-predict(model2,test)
pm2<-rmse(test$Apparent.Temperature,p2)
pm2 #10.12273
summary(model2) #R-squared: 0.1156

windows()
par(mfrow=c(1,2))
plot(train$Visibility,train$Apparent.Temperature)
abline(model2,col="red")

plot(test$Visibility,test$Apparent.Temperature)
abline(model2,col="blue")

#rmse jest du¿o wy¿szy ni¿ w modelu 1, wspó³czynnik R^2 jest bliski 0 wiêc model 
#jest du¿o gorszy i mniej wiarygodny ni¿ poprzedni,oznacza to ¿e widocznoœæ ma ma³o istotny wp³yw na 
#odczuwaln¹ temperaturê

#-------------------------------------------------------------
model3 <- lm(Apparent.Temperature ~ Temperature+Visibility, train)
m3<-rmse(train$Apparent.Temperature,predict(model3))
m3 #1.332421
p3<-predict(model3,test)
pm3<-rmse(test$Apparent.Temperature,p3)
pm3 #1.334695
summary(model3) #R-squared:  0.9847

#b³¹d œredniokwadratowy jest zbli¿ony, minimalnie mniejszy od tego z modelu 1
#wspó³czynnik r^2 jest bliski 1, wiêc ten model jest bardzo wiarygodny
#--------------------------------------------------------------------
model4 <- lm(Apparent.Temperature ~ Humidity, train)
m4<-rmse(train$Apparent.Temperature,predict(model4))
m4 #8.656444
p4<-predict(model4,test)
pm4<-rmse(test$Apparent.Temperature,p4)
pm4 #8.623955
summary(model4) #R-squared:  0.3555

windows()
par(mfrow=c(1,2))
plot(train$Humidity,train$Apparent.Temperature)
abline(model4,col="red")

plot(test$Humidity,test$Apparent.Temperature)
abline(model4,col="blue")

#rmse jest bardzo wysokie(mniejsze od modelu 2), wspó³czynnik R^2 jest niski, wiêc model 
#lepszy ni¿ model 2, co oznacza ¿e wilgotnoœæ ma wiêkszy wp³yw ni¿ widocznoœæ na odczuwaln¹ temperaturê
#ze wzrostem wilgotnoœci maleje odczuwalna temperatura
#------------------------------------------------------------
model5 <- lm(Apparent.Temperature ~ Temperature+Visibility+(Temperature*Humidity), train)
m5<-rmse(train$Apparent.Temperature,predict(model5))
m5 #1.261827
p5<-predict(model5,test)
pm5<-rmse(test$Apparent.Temperature,p5)
pm5 #1.263636
summary(model5) #R-squared:  0.9863 

#jak na razie najlepszy model- najmniejsza z dotychczasowych wartoœæ rmse i r^2

#-----------------------------------------
model6 <- lm(Apparent.Temperature ~ Temperature+Humidity, train)
m6<-rmse(train$Apparent.Temperature,predict(model6))
m6 #1.283516
p6<-predict(model6,test)
pm6<-rmse(test$Apparent.Temperature,p6)
pm6 #1.287803
summary(model6) #R-squared:  0.9858
#model równie dobry,porównywalnie ma³a wartoœæ rmse, r^2 bliski 1, a model mniej skomplikowany ni¿ poprzedni 
#---------------------------------------
RMSE_vec<-c(m1,pm1,m2,pm2,m3,pm3,m4,pm4,m5,pm5,m6,pm6)
df<-data.frame(RMSE=RMSE_vec,model=rep(c("1","2","3","4","5","6"),each=2),zbiór=c("train","test"))
windows()
ggplot(df,aes(x=model,y=RMSE,fill=zbiór))+geom_bar(stat="identity",position="dodge")

#we wszystkich modelach wartoœæ p-value jest <5%, wiêc w ka¿dym zmodeli odrzucam
#tezê o jego losowoœci.zarówno model 1,3,5 i 6 s¹ bardzo dobre.Najni¿szy b³¹d 
#œredniokwadratowy ze wszystkich modeli ma model 5(jest to model regresji 
#wielorakiej z interakcjê pomiêdzy temperatur¹ a wilgotnoœci¹) r^2 jest bliski 1
#wiêc wg wyników jest to najlepszy i najbardziej wiarygodny model ze wzglêdu na 
#najmniejszy rmseja natomiast jako najlepszy model wybra³abym model 6, jest to model
#regresji wielorakiej i jest mniej skomplikowany od modelu 5, a wartoœæ rmse jest
#druga najni¿sza spoœród wszystkich otrzymanych