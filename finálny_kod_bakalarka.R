################ Potrebované packages #################

install.packages('fpp2', dependencies = TRUE)
library(fpp2)

################### Načítanie všetkých dát ###########################

data <- read.table(file="C:\\bakalarka\\data_bakalarka.txt" , header=TRUE, dec = ",")

data_graf <- read.table(file="C:\\bakalarka\\data_bakalarka_graf.txt" , header=FALSE, dec = ",")

attach(data)
head(data)

################### hodnoty pre grafy analýzy dát ###########################


X <- as.Date(format(as.Date(data$Datum, format = "%d.%m.%Y"), "%Y-%m-%d"))
data$year <- as.numeric(format(X, '%Y'))
data$X <- as.Date(format(as.Date(data$Datum, format = "%d.%m.%Y"), "%Y-%m-%d"))

max(data[,5])
min(data[,5])
max(data[,2])
min(data[,2])
max(data[,3])
min(data[,3])

data1920 = subset(data, subset = data$year >= '1920' & data$year <='1929')
data1930 = subset(data, subset = data$year >= '1930' & data$year <='1939')
data1940 = subset(data, subset = data$year >= '1940' & data$year <='1949')
data1950 = subset(data, subset = data$year >= '1950' & data$year <='1959')
data1960 = subset(data, subset = data$year >= '1960' & data$year <='1969')
data1970 = subset(data, subset = data$year >= '1970' & data$year <='1979')
data1980 = subset(data, subset = data$year >= '1980' & data$year <='1989')
data1990 = subset(data, subset = data$year >= '1990' & data$year <='1999')
data2000 = subset(data, subset = data$year >= '2000' & data$year <='2009')
data2010 = subset(data, subset = data$year >= '2010' & data$year <='2020')


################### Grafy analýzy časových radov ###########################

#################### Liptovský Mikuláš #############################

# Priebeh priemerných mesačných prietokov z vodomernej stanice Liptovský Mikuláš na rieke Váh za obdobie 1991-2022

Y1 <- ts(data_graf[,2], start = c(1991,1), frequency = 12)

autoplot(Y1) +
  ylab("Prietoky") +
  xlab("t") +
  theme(axis.title.x = element_text(size = 26), 
        axis.title.y = element_text(size = 26),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# Kĺzavé priemery priemerných mesačných prietokov z vodomernej stanice Liptovský Mikuláš na rieke Váh za obdobie 1991-2022

test_24mo = zoo::rollmean(data$LMikulas, k= 25)
m24 <- c(0,0,0,0,0,0,0,0,0,0,0,0,test_24mo,0,0,0,0,0,0,0,0,0,0,0,0)
data5_1 <- cbind(data, m24)
data5 = subset(data5_1, subset = data5_1$X >= '1990-01-01' & data5_1$X <='2020-12-01')

LM_regression <- lm(data5_1$m24 ~ data5_1[,5], data = data5_1)
summary(LM_regression)
reg = function(x){20.12 + -6.986e-05 * x}
x = 1990:2020
y = reg(x)
plot(y ~ x,type ="l", lwd = 2.5, ylim=c(0,70))

plot <- plot(data5[,5], data5[,2] ,xlab="t" ,ylab= "Prietoky",
             cex.lab = 1.5, cex.axis = 1.4, type="l", lwd=2, ylim=c(0,70), xlim= c(8500,17500))+
  lines(data5[,5], data5$m24, xlim= c(1990,2019), lwd = 2.5, col = "red")

# Boxploty priemerných mesačných prietokov z vodomernej stanice Liptovský Mikuláš na rieke Váh za obdobie 1921-2022 rozdelené podľa desaťročí


boxplot(data1920$LMikulas, data1930$LMikulas, data1940$LMikulas, data1950$LMikulas, data1960$LMikulas, data1970$LMikulas, data1980$LMikulas, data1990$LMikulas, data2000$LMikulas, data2010$LMikulas,names = c("1920s", "1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s"),xlab="Desaťročia" ,ylab= "Prietoky",
        cex.lab = 1.5, cex.axis = 1.4)

# Priebeh diferenovaných priemerných mesačných prietokov z vodomernej stanice Liptovský Mikuláš na rieke Váh za obdobie 1921-2022 rozdelené podľa mesiacov

DY1 <- diff(Y1)

ggsubseriesplot(DY1) +
  ylab("Zmeny prietokov") +
  xlab("Mesiace") +
  theme(axis.title.x = element_text(size = 24), 
        axis.title.y = element_text(size = 24),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))


##################### Šala ####################


# Priebeh priemerných mesačných prietokov z vodomernej stanice Šala na rieke Váh za obdobie 1991-2022

Y2 <- ts(data_graf[,3], start = c(1991,1), frequency = 12)

autoplot(Y2) +
  ylab("Prietoky") +
  xlab("t") +
  theme(axis.title.x = element_text(size = 26), 
        axis.title.y = element_text(size = 26),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# Kĺzavé priemery priemerných mesačných prietokov z vodomernej stanice Šala na rieke Váh za obdobie 1991-2022

test_24mos = zoo::rollmean(data$Sala, k= 25)
m24s <- c(0,0,0,0,0,0,0,0,0,0,0,0,test_24mos,0,0,0,0,0,0,0,0,0,0,0,0)
data5_1s <- cbind(data, m24s)
data5s = subset(data5_1s, subset = data5_1$X >= '1990-01-01' & data5_1$X <='2020-12-01')

LM_regressions <- lm(data5_1s$m24 ~ data5_1s[,5], data = data5_1s)
summary(LM_regressions)
regs = function(x){141.7 + -5.654e-04 * x}
xs = 1:2020
ys = reg(xs)
plot(ys ~ xs,type ="l", lwd = 2.5, ylim=c(0,70))

plot <- plot(data5s[,5], data5s[,3] ,xlab="t" ,ylab= "Prietoky",
             cex.lab = 1.5, cex.axis = 1.4, type="l", lwd=2, ylim=c(0,500), xlim= c(8500,17500))+
  lines(data5s[,5], data5s$m24, xlim= c(1990,2019), lwd = 2.5, col = "red")


# Boxploty priemerných mesačných prietokov z vodomernej stanice Šala na rieke Váh za obdobie 1921-2022 rozdelené podľa desaťročí
boxplot(data1920$Sala, data1930$Sala, data1940$Sala, data1950$Sala, data1960$Sala, data1970$Sala, data1980$Sala, data1990$Sala, data2000$Sala, data2010$Sala,names = c("1920s", "1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s"),xlab="Desaťročia" ,ylab= "Prietoky",
        cex.lab = 1.5, cex.axis = 1.4)

# Priebeh diferenovaných priemerných mesačných prietokov z vodomernej stanice Šala na rieke Váh za obdobie 1921-2022 rozdelené podľa mesiacov

DY2 <- diff(Y2)

ggsubseriesplot(DY2) +
  ylab("Zmeny prietokov") +
  xlab("Mesiace") +
  theme(axis.title.x = element_text(size = 24), 
        axis.title.y = element_text(size = 24),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))

################### Zadefinovanie Sample a Test intervalov ###########################

# Liptovský Mikuláš

S1 <- ts(data[,2], start = c(1921,1),end = c(2005,1), frequency = 12)


# Sala

S2 <- ts(data[,3], start = c(1921,1),end = c(2005,1), frequency = 12)


################### ETS model ###########################

# Liptovský Mikuláš

#ETS(M,A,M)
fit_ets12 <- ets(S1)
print(summary(fit_ets12))
checkresiduals(fit_ets12)

# Predpoveď ETS(M,A,M) modelu pre priebeh priemerných mesačných prietokov zo stanice Liptovský Mikuláš na rieke Váh za obdobie 2000-2022 porovnaná s reálnymi dátami
fcst_ets12 <- forecast(fit_ets12, h=204)
plot(fcst_ets12, include = 60, ylim=c(0,400)) +
  lines(Y1, lwd=2, col="red" )

#ETS(M,N,A) 
fit_ets13 <- ets(S1, model = "MZA")
print(summary(fit_ets13))
checkresiduals(fit_ets13)

#ETS(A,N,N)
fit_ets15 <- ets(S1, model = "ANN")
print(summary(fit_ets15))
checkresiduals(fit_ets15)

#ETS(A,A,N) 
fit_ets15 <- ets(S1, model = "AAN")
print(summary(fit_ets15))
checkresiduals(fit_ets15)

#ETS(A,N,A) 
fit_ets14 <- ets(S1, model = "ANA")
print(summary(fit_ets14))
checkresiduals(fit_ets14)

# Predpoveď ETS(A,N,A) modelu pre priebeh priemerných mesačných prietokov zo stanice Liptovský Mikuláš na rieke Váh za obdobie 2000-2022 porovnaná s reálnymi dátami
fcst_ets14 <- forecast(fit_ets14, h=204)
plot(fcst_ets14, lwd=2.5, include = 70, ylim=c(0,120)) +
  lines(Y1, lwd=2, col="red" )

#ETS(A,Ad,A) 
fit_ets11 <- ets(S1, model = "AAA")
print(summary(fit_ets11))
checkresiduals(fit_ets11)

###################### Šala ######################

#ETS(A,N,N)
fit_ets25 <- ets(S2, model = "ANN")
print(summary(fit_ets25))
checkresiduals(fit_ets25)

#ETS(A,Ad,N) 
fit_ets25 <- ets(S2, model = "AAN")
print(summary(fit_ets25))
checkresiduals(fit_ets25)

#ETS(A,N,A) 
fit_ets24 <- ets(S2, model = "ANA")
print(summary(fit_ets24))
checkresiduals(fit_ets24)

# Predpoveď ETS(A,N,A) modelu pre priebeh priemerných mesačných prietokov zo stanice Šala na rieke Váh za obdobie 2000-2022 porovnaná s reálnymi dátami
fcst_ets24 <- forecast(fit_ets24, h=204)
plot(fcst_ets24, lwd=2.5, include = 70, ylim=c(0,600)) +
  lines(Y2, lwd=2, col="red" )

#ETS(A,Ad,A) 
fit_ets21 <- ets(S2, model = "AAA")
print(summary(fit_ets21))
checkresiduals(fit_ets21)

#ETS(M,N,A) 
fit_ets23 <- ets(S2, model = "MZA")
print(summary(fit_ets23))
checkresiduals(fit_ets23)


################### ARIMA model ###########################

# Liptovský Mikuláš

lnS1 <- log(S1)

# hladanie počtu parametrov
fit_arimaln11 <- auto.arima(lnS1, D=1, max.p = 4, max.q = 8, max.P = 8, max.Q = 4, max.order = 20, approximation = FALSE, stepwise =FALSE, trace = TRUE, allowdrift = FALSE,) 

# ARIMA(1,0,1)(8,1,0)[12] : 914.3451
fit_arima11 <- arima(lnS1, order = c(1,0,1), seasonal = list(order = c(8,1,0))) 
print(summary(fit_arima11))
checkresiduals(fit_arima11)

# Predpoveď ARIMA(1,0,1)(8,1,0)[12] modelu pre priebeh priemerných mesačných prietokov zo stanice Liptovský Mikuláš na rieke Váh za obdobie 2000-2022 porovnaná s reálnymi dátami
fcst_arima11 <- forecast(fit_arima11, h=204)
plot(fcst_arima11, cex.lab = 1.5, cex.axis = 1.4, include = 60, ylim=c(1,5)) +
  lines(log(Y1), lwd=2, col="red" )

# ARIMA(1,0,3)(8,1,0)[12] : 917.8202
fit_arima12 <- arima(lnS1, order = c(1,0,3), seasonal = list(order = c(8,1,0))) 
print(summary(fit_arima12))
checkresiduals(fit_arima12)

# ARIMA(0,0,6)(8,1,0)[12] : 920.2022
fit_arima13 <- arima(lnS1, order = c(0,0,6), seasonal = list(order = c(8,1,0))) 
print(summary(fit_arima13))
checkresiduals(fit_arima13)

# ARIMA(2,0,4)(8,1,0)[12] : 921.0585
fit_arima14 <- arima(lnS1, order = c(2,0,4), seasonal = list(order = c(8,1,0))) 
print(summary(fit_arima14))
checkresiduals(fit_arima14)


########################################################################

# Šala

lnS2 <- log(S2)

# hladanie počtu parametrov
fit_arimaln11 <- auto.arima(lnS2, D=1, max.p = 4, max.q = 6, max.P = 8, max.Q = 4, max.order = 20, approximation = FALSE, stepwise =FALSE, trace = TRUE, allowdrift = FALSE,) 

# ARIMA(2,0,1)(8,1,0)[12] : 1174.767
fit_arima21 <- arima(lnS2, order = c(2,0,1), seasonal = list(order = c(8,1,0))) 
print(summary(fit_arima21))
checkresiduals(fit_arima21)

# Predpoveď ARIMA(2,0,1)(8,1,0)[12] modelu pre priebeh priemerných mesačných prietokov zo stanice Šala na rieke Váh za obdobie 2000-2022 porovnaná s reálnymi dátami
fcst_arima21 <- forecast(fit_arima21, h=208)
plot(fcst_arima21, cex.lab = 1.5, cex.axis = 1.4, include = 60, ylim=c(3,7)) +
  lines(log(Y2), lwd=2, col="red" )

#ARIMA(1,0,3)(8,1,0)[12]                    : 1176.007
fit_arima22 <- arima(lnS2, order = c(1,0,3), seasonal = list(order = c(8,1,0))) 
print(summary(fit_arima22))
checkresiduals(fit_arima22)

# ARIMA(1,0,4)(8,1,0)[12]                    : 1178.057
fit_arima23 <- arima(lnS2, order = c(1,0,4), seasonal = list(order = c(8,1,0))) 
print(summary(fit_arima23))
checkresiduals(fit_arima23)

# ARIMA(1,0,0)(8,1,0)[12]                    : 1175.097
fit_arima24 <- arima(lnS2, order = c(1,0,0), seasonal = list(order = c(8,1,0))) 
print(summary(fit_arima24))
checkresiduals(fit_arima24)

########################################################################
# Porovnávanie predpovedí

# Porovnanie strednej hodnoty predpovede vybratého ARIMA a ETS modelu pre priebeh priemerných mesačných prietokov zo stanice Liptovský Mikuláš na rieke Váh za obdobie 2005-2021 s reálnymi dátami
plot(Y1, type = "l", col = "blue", xlab="t" ,ylab= "Prietoky", cex.lab = 1.9, cex.axis = 1.7, xlim=c(2005,2021))
lines(exp(fcst_arima11$mean), col = "red")
lines(fcst_ets14$mean, col = "green")
legend("topright", legend = c("Reálne hodnoty", "Predpoveď pomocou Arima", "Predpoveď pomocou ETS"), col = c("blue", "red", "green"), lty = 1, cex=1.2)

# Porovnanie strednej hodnoty predpovede vybratého ARIMA a ETS modelu pre priebeh priemerných mesačných prietokov zo stanice Šala na rieke Váh za obdobie 2005-2021 s reálnymi dátami
plot(Y2, type = "l", col = "blue", xlab="t" ,ylab= "Prietoky", cex.lab = 1.9, cex.axis = 1.7, xlim=c(2005,2021))
lines(exp(fcst_arima21$mean), col = "red")
lines(fcst_ets24$mean, col = "green")
legend("topright", legend = c("Reálne hodnoty", "Predpoveď pomocou Arima", "Predpoveď pomocou ETS"), col = c("blue", "red", "green"), lty = 1, cex=1.2)

