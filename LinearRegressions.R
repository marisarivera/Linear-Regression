#Universidad Francisco Marroquin
#Facultad de Ciencias Economicas
#Metodos Numericos
#Catedratica: Ing. Silvia Ruiz
#Auxiliar: Ing. Pablo Diaz

#Nombre: Maria Isabel Rivera Portillo
#Carnet: 20150204
#PROJECT: LINEAR REGRESSIONS

##Set Directory
getwd()
setwd("...Set Working Directory...") 

##Packages
library(dplyr)
library(readr)
library(caTools)
library(ggplot2)

#LEAST SQUARE METHOD
lsq_m <- function(x,y){
  if (length(x) == length(y)) {
    n <- length(x)
    s_x <- sum(x)
    s_y <- sum(y)
    s_xy <- sum(x*y)
    s_xx <- sum(x*x)
    a1 = (n*s_xy - s_x*s_y)/(n*s_xx - (s_x)**2)
    return(a1)
  }
  else {
    print("Arreglos no son de la misma dimension")
  }
}

lsq_c <- function(x,y){
  if (length(x) == length(y)) {
    n <- length(x)
    s_x <- sum(x)
    s_y <- sum(y)
    s_xy <- sum(x*y)
    s_xx <- sum(x*x)
    a0 = (s_xx*s_y - s_xy*s_x)/(n*s_xx - (s_x)**2)
    return(a0)
  }
  else {
    print("Arreglos no son de la misma dimension")
  }
}


#EXAMPLE 1: LINEAR vs. LOGARITHMIC REGRESSIONS
##Declaration of Data
x <- c(0.2,0.5,1.0,2.0,3.0)
x2 <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,
        1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,
        2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0)
y <- c(3.0,2.0,1.4,1.0,0.6)
y2 <- 1/y

##Dispersion of Data
ggplot() +
  geom_point(aes(x = x, y = y2), colour = 'red') +
  ggtitle('Example 1: Dispersion') + xlab('x values') + ylab('y values')

m <- lsq_m(x,y2)
c <- lsq_c(x,y2)

##Linear Regression
ggplot() +
  geom_point(aes(x = x, y = y2), colour = 'red') +
  geom_line(aes(x = x2, y = m*x2 + c), colour = 'blue') +
  ggtitle('Example 1: Linear Regression') + xlab('x values') + ylab('y values')

##Logarithmic Regression
ggplot() +
  geom_point(aes(x = x, y = y), colour = 'red') +
  geom_line(aes(x = x2, y = 1/(m*x2 + c)), colour = 'blue') +
  ggtitle('Example 1: Logarithmic Regression') + xlab('x values') + ylab('y values')


#EXAMPLE 2: LINEAR vs. EXPONENTIAL REGRESSIONS
##Declaration of Data
years <- c(1900,1950,1970,1980,1990,2000,2010)
years2 <- seq(1900,2010,1)
people <- c(400,557,825,981,1135,1266,1370)
people_ln <- log(people)

##Dispersion of Data
ggplot() +
  geom_point(aes(x = years, y = people), colour = 'red') +
  ggtitle('Population by Year Dispersion') + xlab('Year') + ylab('Population')

m2 <- lsq_m(years, people_ln)
c2 <- lsq_c(years, people_ln)

people2<-c()
i=1
while(i<(length(years2)+1)) {
  value<-exp(c2) * exp(m2*years2[i])
  people2<-c(people2,value)
  i=i+1
}

##Linear Regression
ggplot() +
  geom_point(aes(x = years, y = people_ln), colour = 'red') +
  geom_line(aes(x = years, y = m2*years + c2), colour = 'blue') +
  ggtitle('Example 2: Linear Regression') + xlab('Years') + ylab('Population')

##Exponential Regression
ggplot() +
  geom_point(aes(x = years, y = people), colour = 'red') +
  geom_line(aes(x = years2, y = people2), colour = 'blue') +
  ggtitle('Example 2: Exponential Regression') + xlab('Years') + ylab('Population')


#EXAMPLES WITH REAL DATASETS
##Example 1: Salaries by Year in US Dollars (Linear Regression vs. Logarithmic Regression)
salaries <- read.csv("Salary_Data.csv")
dim(salaries) #30 registers & 2 variables
summary(salaries)
head(salaries, head=5)

x_s <- salaries$YearsExperience
x2_s <- seq(1,12,1)
y_s <- salaries$Salary
y2_s <- 1/y_s

###Dispersion of Data
ggplot(salaries, aes(x = x_s, y = y_s, color = 'red')) + 
  geom_point() + 
  ggtitle('Salary vs Years of Experience Dispersion') +
  xlab('Years of Experience') +
  ylab('Salary')

m_s <- lsq_m(x_s,y_s)
c_s <- lsq_c(x_s,y_s)

###Linear Regression
ggplot(salaries, aes(x = x_s, y = y_s, color = 'red')) +
  geom_point() +
  geom_line(aes(x = x_s, y = m_s*x_s + c_s), colour = 'blue') +
  ggtitle('Salary vs Years of Experience') +
  xlab('Years of Experience') +
  ylab('Salary')

###Linear Regression Using R function
fit_salaries <- lm(Salary ~ YearsExperience, data = salaries)
summary(fit_salaries)
fit_salaries$coefficients
interval <- c(1:25)
df <- data.frame(interval, predict(fit_salaries, data.frame(YearsExperience=interval)))
colnames(df) <- c("Years of Experience","Expected Salary")
df

###Logarithmic Regression
ggplot(salaries, aes(x = x_s, y = y2_s, color = 'red')) +
  geom_point() +
  geom_line(aes(x = x_s, y = 1/(m_s*x_s + c_s)), colour = 'blue') +
  ggtitle('Salary vs Years of Experience') +
  xlab('Years of Experience') +
  ylab('Salary')


##Example 2: Deaths by Year in Guatemala Country (Linear Regression vs. Exponential Regression)
deaths <- read.csv("muertes.csv")
dim(deaths) #154 registers & 4 variables
summary(deaths)
head(deaths, head=5)
Grouped_Deaths <- group_by(deaths,Ano) %>% summarise(DeathsByYear=sum(Muertes))

years_d <- Grouped_Deaths$Ano
years2_d <- seq(2009,2015,1)
deaths_d <- Grouped_Deaths$DeathsByYear
deaths_dln <- log(deaths_d)

##Dispersion of Data
ggplot(Grouped_Deaths, aes(x = years_d, y = deaths_d, color = 'red')) +
  geom_point() +
  ggtitle('Deaths vs Year') +
  xlab('Year') +
  ylab('Number of Deaths')

m_d <- lsq_m(years_d, deaths_dln)
c_d <- lsq_c(years_d, deaths_dln)

deaths2_d <-c()
i=1
while(i<(length(years2_d)+1)) {
  v<-exp(c_d) * exp(m_d*years2_d[i])
  deaths2_d<-c(deaths2_d,v)
  i=i+1
}

##Linear Regression
ggplot(Grouped_Deaths, aes(x = years_d, y = deaths_dln, color = 'red')) +
  geom_point() +
  geom_line(aes(x = years_d, y = m_d*years_d + c_d), colour = 'blue') +
  ggtitle('Deaths vs Year') +
  xlab('Year') +
  ylab('Number of Deaths')

###Linear Regression Using R function
fit_deaths <- lm(DeathsByYear ~ Ano, data = Grouped_Deaths)
summary(fit_deaths)
fit_deaths$coefficients
interval2 <- c(2015:2025)
df2 <- data.frame(interval2, predict(fit_deaths, data.frame(Ano=interval2)))
colnames(df2) <- c("Year","Expected Number of Deaths")
df2

##Exponential Regression
ggplot(Grouped_Deaths, aes(x = years_d, y = deaths_d, color = 'red')) +
  geom_point() +
  geom_line(aes(x = years2_d, y = deaths2_d), colour = 'blue') +
  ggtitle('Deaths vs Year') +
  xlab('Year') +
  ylab('Number of Deaths')
