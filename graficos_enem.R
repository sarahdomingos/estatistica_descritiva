
#Coletar dados
url <- "C:path do arquivo/ENEM_AL_R_2022ok.csv"
milsa <- read.csv(url, header = TRUE, sep = ";", dec = ",")

#a) Gerar um gráfico boxplot com todas as notas das provas, inclusive com a
#média final obtida. Analisar os resultados obtidos.
notas <- milsa[c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO", "NU_NOTA_ENEM")]
boxplot(notas)

#b) Trabalhando com dados agrupados, com 10 classes, usando 100 pontos
#como intervalo de classe, construir tabela com as frequências absolutas e
#relativas e suas respectivas frequências acumuladas para as notas de
#Matemática (NU_NOTA_MT) e Redação (NU_NOTA_REDACAO). Gerar
#Histogramas com os resultados obtidos e comentar os resultados obtidos.
limites <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
classes <- c("0 - 100", "100 - 200", "200 - 300", "300 - 400", "400 - 500", "500 - 600", "600 - 700", "700 - 800", "800 - 900", "900 - 1000")

freq_MA <- table(cut(milsa$NU_NOTA_MT, breaks=limites, right=FALSE, labels=classes))
freq_ac_MA <- cumsum(freq_MA)
freq_rel_MA <- prop.table(freq_MA)
freq_rel_ac_MA <- cumsum(freq_rel_MA)

freq_REDACAO <- table(cut(milsa$NU_NOTA_REDACAO, breaks=limites, right=FALSE, labels=classes))
freq_ac_REDACAO <- cumsum(freq_REDACAO)
freq_rel_REDACAO <- prop.table(freq_REDACAO)
freq_rel_ac_REDACAO <- cumsum(freq_rel_REDACAO)

tab_MA <- cbind(freq_MA, freq_ac_MA, freq_rel_MA=round (freq_rel_MA*100, digits=2), freq_rel_ac_MA=round (freq_rel_ac_MA*100, digits=2))
print(tab_MA)
hist_MA <- hist(tab_MA)

tab_RE <- cbind(freq_REDACAO, freq_ac_REDACAO, freq_rel_REDACAO=round (freq_rel_REDACAO*100, digits=2), freq_rel_ac_REDACAO=round (freq_rel_ac_REDACAO*100, digits=2))
print(tab_RE)
hist_RE <- hist(tab_RE)

#c) Com a média final (NU_NOTA_ENEM), agrupe os dados em 6 classes (0-300; 300-400, 400-500, 500-600, 700-1000) construir tabela com as
#frequências absolutas e relativas e suas respectivas acumuladas. Gerar o
#Histogramas e comentar o resultado obtido.
limites <- c(0, 300, 400, 500, 600, 700, 1000)
classes <- c("0 - 300", "300 - 400", "400 - 500", "500 - 600", "600 - 700", "700 - 1000")

freq_FINAL <- table(cut(milsa$NU_NOTA_ENEM, breaks=limites, right=FALSE, labels=classes))
freq_ac_FINAL <- cumsum(freq_FINAL)
freq_rel_FINAL <- prop.table(freq_FINAL)
freq_rel_ac_FINAL <- cumsum(freq_rel_FINAL)

tab_FINAL <- cbind(freq_FINAL, freq_ac_FINAL, freq_rel_FINAL=round (freq_rel_FINAL*100, digits=2), freq_rel_ac_FINAL=round (freq_rel_ac_FINAL*100, digits=2))
print(tab_FINAL)
hist_MA <- hist(tab_FINAL)

#d) Escolher duas variáveis (colunas) qualitativas, gerar gráficos adequados e
#interpretar os resultados.

tabela <- table(milsa$TP_SEXO, milsa$TP_LINGUA)
linhas <- c("Feminino", "Masculino")
colunas <- c("Inglês", "Espanhol")
rownames(tabela, do.NULL = TRUE)
colnames(tabela, do.NULL = TRUE)
colnames(tabela) <- colunas
rownames(tabela) <- linhas
fourfoldplot(tabela, main="Relação entre sexo e idioma")

#e) Escolher duas variáveis (colunas), sendo uma qualitativa e outra
#quantitativa, gera gráficos de barras de pizza e interpretar os resultados.
library(ggplot2)

limites <- c(0, 300, 600, 800, 1000)
classes <- c("0 - 300", "300 - 600", "600 - 800", "800 - 1000")
tabela2 <- table(cut(milsa$NU_NOTA_ENEM, breaks=limites, right=FALSE, labels=classes), milsa$Q025)
print(tabela2)

df = data.frame(subject <- c('0 - 300','0 - 300','300 - 600','300 - 600','600 - 800','600 - 800'),
                internet <- c('Com acesso','Sem acesso','Com acesso','Sem acesso',
                            'Com acesso','Sem acesso'),
                value <- c(12,11,8208,3719,1683,153))

df$subject <- factor(df$subject)
df$internet <- factor(df$internet) 

ggplot(data=df, aes(x=" ", y=value, group=internet, colour=internet, fill=internet)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ subject) +theme_void() + ggtitle("Notas ENEM") + theme(plot.title = element_text(hjust=0.5, face='bold'))

