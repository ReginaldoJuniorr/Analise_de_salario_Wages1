# Análise descritiva dos salários (U$/hora) de 40 pessoas, obtidos do arquivo Wages1

A análise consistirá no cálculo das medidas de tendência central, 
variabilidade, elaboração de tabela de distribuição de frequências, diagrama de 
caixa e histograma de frequências.

## Começamos instalando a biblioteca "Ecdat":

```{r}
install.packages("Ecdat")
```
## Vamos criar um objeto com os nossos dados:

```{r}
salario<-c
(3.67,5.31,3.74,8.57,4.11,6.96,6.13,3.22,4.60,
7.66,6.01,4.70,5.69,4.35,2.84,1.99,4.77,10.31,
4.17,1.71,4.07,8.35,7.47,3.16,3.27,4.41,0.92,
11.13,6.15,1.07,2.14,8.23,3.20,4.41,12.10,
4.37,6.79,7.04,7.46,4.90)
```

## Para visualizar o objeto salario, basta digitar:

```{r}
salario
```

o R mostrará os dados, dessa forma:
```
salario
 [1]  3.67  5.31  3.74  8.57  4.11  6.96  6.13  3.22  4.60  7.66  6.01  4.70
[13]  5.69  4.35  2.84  1.99  4.77 10.31  4.17  1.71  4.07  8.35  7.47  3.16
[25]  3.27  4.41  0.92 11.13  6.15  1.07  2.14  8.23  3.20  4.41 12.10  4.37
[37]  6.79  7.04  7.46  4.90
```


## Para descubir a média salarial, faremos o seguinte comando:

```{r}
média<-mean(salario)
```

e para consultá-la: 

```{r}
média
```

O salário médio por hora é U$ 5,28.

## Da mesma forma faremos com a mediana:

```{r}
mediana<-median(salario)
mediana
```

O salário mediano por hora é de U$ 4,65, indicando que 50% das pessoas têm 
salário por hora menor ou igual a U$ 4,65 e 50%, maior do que este valor.

## A moda:

```{r}
y <- table(salario)
moda <- names(y)[which(y==max(y))]
moda
```

O salário por hora mais frequente é U$ 4,41.

Para saber qual é a frequência observada basta digitar:

```{r}
table(salario)
```

o R mostrará a frequência, dessa forma:
```
salario
[1]  3.67  5.31  3.74  8.57  4.11  6.96  6.13  3.22  4.60  7.66  6.01  4.70
[13]  5.69  4.35  2.84  1.99  4.77 10.31  4.17  1.71  4.07  8.35  7.47  3.16
[25]  3.27  4.41  0.92 11.13  6.15  1.07  2.14  8.23  3.20  4.41 12.10  4.37
[37]  6.79  7.04  7.46  4.90
```
## A variância:

```{r}
variância<-var(salario)
variância
```
>[1] 6.852329

Aprendemos em aula que variância é calculada elevando-se o desvio dos valores observados em 
relação à média ao quadrado, o que significa que a unidade dessa medida é
também elevada ao quadrado, portanto, não pode ser comparada com a média.

Para que seja possível a comparação, deve-se usar o desvio padrão.

```{r}
desvio_padrão<-sd(salario)
desvio_padrão
```
>[1] 2.617695

O desvio padrão do salário por hora é U$ 2,62.

## O coeficiente de variação:

```{r}
coef_variação<-(desvio_padrão/média)*100
coef_variação
```
>[1] 49.5893

A variação em torno do salário médio por hora é de 49,59%, indicando uma 
grande variabilidade dos salários.

## Diagrama de caixa:

```{r}
boxplot(salario,main=" ", cex.main=0.9,col="lightgreen")
```
![plot](https://github.com/user-attachments/assets/5a2b6eb2-d3f6-4a67-be35-3ceab35edcf6)


o diagrama de caixa indica que não existe nenhum valor atípico ou outlier.

## Distribuição de frequências e histograma de frequências

Para a elaboração da distribuição de frequências, deve-se inicialmente 
calcular o número de intervalos de classes (𝑘). Nesse caso, o número de 
observações é igual a 40, ou seja, 𝑛 = 40. Utilizando a fórmula de Sturges para 
obter o número de intervalos, tem-se:

𝑘 = 1 + 3,3 × log(𝑛) = 1 + 3,3 × log(40) = 6,29 ≅ 7

Serão utilizados sete intervalos de classes. Para definir os limites de 
classes:

𝐴𝑡 = 𝑚𝑎𝑖𝑜𝑟 𝑣𝑎𝑙𝑜𝑟 − 𝑚𝑒𝑛𝑜𝑟 𝑣𝑎𝑙𝑜𝑟
𝐴𝑡 = 12,10 − 0,92 = 11,18
ℎ = 11,18 / 7 = 1,5971 ≅ 1,6

Recomenda-se arredondar o valor de ℎ sempre para um valor maior, nesse 
caso, será arredondado para 1,6. O limite inferior do primeiro intervalo será o 
menor valor do conjunto de dados, nesse caso, 0,92. O limite superior é obtido 
somando o valor de ℎ; tem-se, então, 2,52, e assim por diante. A notação utilizada 
( |--- ) indica que inclui o limite inferior e não inclui o limite superior.
Para obter a distribuição de frequências e o histograma de frequências no 
R, procedemos da seguinte forma:

```{r}
k<-ceiling((1+3.3*log10(length(salario)))) # cálculo do número de intervalos de classe
k
```
>[1] 7

```{r}
h<-(diff(range(salario))/k) # cálculo do intervalo de classe
h
```
>[1] 1.597143

```{r}
h<-1.6 # definindo o intervalo
minimo<-min(salario);minimo
```
> [1] 0.92

```{r}
limites<-c(minimo,minimo+h,minimo+2*h,minimo+3*h,minimo+4*h,
           minimo+5*h,minimo+6*h,minimo+7*h)
limites # apresenta os limites dos intervalos de classes
```
>[1]  0.92  2.52  4.12  5.72  7.32  8.92 10.52 12.12

Os limites de classes são: 0,92 |--- 2,52, 2,52 |--- 4,12, até 10,52 |--- 12,12.

Em seguida, calcula-se as frequências de cada intervalo de classe.

```{r}
classes<-c("0.92|-- 2.52","2.52|-- 4.12","4.12|-- 5.72",
             "5.72|-- 7.32","7.32|-- 8.92","8.92|-- 10.52",
             "10.52|-- 12.12")
table(cut(salario,breaks=limites,right=FALSE,labels=classes))
```
```
0.92|-- 2.52   2.52|-- 4.12   4.12|-- 5.72   5.72|-- 7.32   7.32|-- 8.92
         5              9             11              6              6 
8.92|-- 10.52 10.52|-- 12.12
         1              2
```

Para construir o histograma de frequências:

```{r}
histo<-hist(salario,breaks = 
c(minimo,minimo+h,minimo+2*h,minimo+3*h,minimo+4*h,
minimo+5*h,minimo+6*h,minimo+7*h),
include.lowest = TRUE,col="grey",freq=T,
main=" ",ylab="Freq.",
xlab="Intervalos de salário")
```
![histograma](https://github.com/user-attachments/assets/7496c8ec-49cd-482f-92e2-b6639f1bb975)


Em que:

• hist: função para construir o histograma;

• breaks: limites de classes;

• include.lowest = TRUE: inclui o limite inferior da classe;

• col: cor das colunas;

• freq=T: utiliza as frequências;

• main=" ": título do gráfico em branco;

• ylab="Freq.": título do eixo Y;

• xlab="Intervalos de ICEI": título do eixo X.

Para a construção do histograma de frequências e o polígono de 
frequências no R:

```{r}
histo<-hist(salario,breaks = 
c(minimo,minimo+h,minimo+2*h,minimo+3*h,minimo+4*h,
minimo+5*h,minimo+6*h,minimo+7*h),
include.lowest = TRUE,col="grey",freq=T,
main=" ",cex.main=0.8,
ylab="Freq.",xlab="Intervalos de salário",xlim=c(0,14))
lines(c(0.12, histo$mids, 12.92), c(0,histo$counts, 0),
type = "l",col="red",lwd=2)
```
![histo+poligono](https://github.com/user-attachments/assets/eb2f078c-7819-4b12-a39c-dfb99979df58)

Em que:

• lines: desenha os segmentos de reta;

• histo$mids: pontos médios das classes.

Os valores definidos na função lines são:

• 0,12: é limite inferior do primeiro intervalo de classe (0,92) menos a metade do valor de h (ℎ/2 = 0,80);

• 12,92: é limite superior do último intervalo de classe (12,12) mais a metade do valor de h (ℎ/2 = 0,80).
