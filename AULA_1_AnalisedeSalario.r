
install.packages("Ecdat")

# criando o objeto salario no R:
salario <-c (3.67,5.31,3.74,8.57,4.11,6.96,6.13,3.22,4.60,7.66,
           6.01,4.70,5.69,4.35,2.84,1.99,4.77,10.31,4.17,1.71,
           4.07,8.35,7.47,3.16,3.27,4.41,0.92,11.13,6.15,1.07,
           2.14,8.23,3.20,4.41,12.10,4.37,6.79,7.04,7.46,4.90)

salario # visualizando o objeto salario

media <- mean(salario) # descobrindo a média sálarial

média # visualizando a média

mediana <- median(salario) # descobrindo a mediana 

mediana #visualizando a mediana

# descobrindo a moda
y <- table(salario)
moda <- names(y)[which(y==max(y))]

moda #visualizando a moda

table(salario) #para mostrar a frequência observada

variância <- var (salario) # descobrindo variância

variância #visualizando a variância

desvio_padrão <- sd(salario) #descobrindo o desvio padrão

desvio_padrão #visualizando o desvio padrão

coef_variação <- (desvio_padrão/média)*100 # descobrindo o coeficiente de variação

coef_variação #visualizando o coeficiente de variação

boxplot(salario,main=" ", cex.main=0.9,col="lightgreen") # criando diagrama de caixa

k <- ceiling((1+3.3*log10(length(salario)))) # cálculando o número de intervalos de classe

k # visualizando o intervalo de classe

h <- (diff(range(salario))/k) # cálculando o intervalo de classe

h # visualizando o intervalo

h <- 1.6 # definindo o intervalo

minimo <- min(salario);minimo # visualizando o valor minimo

limites <- c(minimo,minimo+h,minimo+2*h,minimo+3*h,minimo+4*h,
           minimo+5*h,minimo+6*h,minimo+7*h) # definindo os limites 

limites # visualizando os limites dos intervalos de classes

classes <- c("0.92|-- 2.52","2.52|-- 4.12","4.12|-- 5.72",
             "5.72|-- 7.32","7.32|-- 8.92","8.92|-- 10.52",
             "10.52|-- 12.12") # calculando as frequências de cada intervalo de classe           

table(cut(salario,breaks=limites,right=FALSE,labels=classes))

histo <- hist(salario,breaks = 
c(minimo,minimo+h,minimo+2*h,minimo+3*h,minimo+4*h,
minimo+5*h,minimo+6*h,minimo+7*h),
include.lowest = TRUE,col="grey",freq=T,
main=" ",ylab="Freq.",
xlab="Intervalos de salário") # construindo o histograma de frequências

histo <- hist(salario,breaks = 
c(minimo,minimo+h,minimo+2*h,minimo+3*h,minimo+4*h,
minimo+5*h,minimo+6*h,minimo+7*h),
include.lowest = TRUE,col="grey",freq=T,
main=" ",cex.main=0.8,
ylab="Freq.",xlab="Intervalos de salário",xlim=c(0,14))
lines(c(0.12, histo$mids, 12.92), c(0,histo$counts, 0),
type = "l",col="red",lwd=2) # construindo o histograma de frequências e o polígono de frequências
