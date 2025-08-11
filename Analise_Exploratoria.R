

setwd("C:/Users/Jean/Desktop/Trabalho_Probabilidade")



obesidade <- read.csv("Obesidade_clean.csv")


summary(obesidade)


obesidade<- within(obesidade, {
  Genero                      <- factor(Genero)
  Historico_Familiar_Obesidade<- factor(Historico_Familiar_Obesidade)
  Comida_Alta_Caloria         <- factor(Comida_Alta_Caloria)
  Consumo_Vegetais            <- factor(Consumo_Vegetais)
  Comer_Entre_Refeicoes       <- factor(Comer_Entre_Refeicoes)
  Fumante                     <- factor(Fumante)
  Monitora_Calorias           <- factor(Monitora_Calorias)
  Uso_Tecnologia              <- factor(Uso_Tecnologia)
  Consumo_Alcool              <- factor(Consumo_Alcool)
  Meio_Transporte             <- factor(Meio_Transporte)
  Nivel_Obesidade             <- factor(Nivel_Obesidade)
})




maximo_obesidade <- max(obesidade$Peso)

minimo_obesidade <- min(obesidade$Peso)


media_obesidade <- mean(obesidade$Peso)

mediana_obesidade <- median(obesidade$Peso)

desvio_padr_obesidade <- sd(obesidade$Peso)

quartis_obesidade <- quantile(obesidade$Peso)

Amplitude_obesidade <- maximo_obesidade - minimo_obesidade

Nclasses <- nclass.Sturges(obesidade$Peso); Nclasses

amplitude_classes <- ceiling(Amplitude_obesidade / Nclasses)

limiteclas <- seq(minimo_obesidade,maximo_obesidade + amplitude_classes,by = amplitude_classes)

limiteclas

classes <- c(
  "39 |- 50",
  "50 |- 61",
  "61 |- 72",
  "72 |- 83",
  "83 |- 94",
  "94 |- 105",
  "105 |- 116",
  "116 |- 127",
  "127 |- 138",
  "138 |- 149",
  "149 |- 160",
  "160 |- 171",
  "171 |- 182"
)

Freq = table(cut(obesidade$Peso, breaks = limiteclas, right=FALSE, 
                 labels=classes))

Freq

FreqAc <- cumsum(Freq); 
FreqRel <- prop.table(Freq); 
FreqRelAc <- cumsum(FreqRel)

TabResul = cbind(Freq,
                 FreqAc, 
                 FreqRel = round(FreqRel*100,digits = 2),
                 FreqRelAc= round(FreqRelAc*100,digits = 2))
TabResul

Pesos <- obesidade$Peso

h <- hist(Pesos, breaks=limiteclas,
         ylab="Frequencias absolutas",  labels=TRUE,cex.lab = 0.8,main="Histograma Obesidade", 
         xlim=c(39,182), ylim = c (0,400), col="blue",right=FALSE,border="white",lwd=2)


lines(c(min(h$breaks), h$mids, max(h$breaks)), 
      c(0,h$counts, 0), type = "l", col="orange",lwd=3)


boxplot(obesidade$Peso,  col = "magenta",main="Boxplot Peso Corporal",ylab="Peso (Kg)",outcol="red",outpch = 16)

obesidade$Nivel_Obesidade <- factor(
  obesidade$Nivel_Obesidade,
  levels = c(
    "Abaixo do Peso",
    "Peso Normal",
    "Sobrepeso Nível 1",
    "Sobrepeso Nível 2",
    "Obesidade Nível 1",
    "Obesidade Nível 2",
    "Obesidade Nível 3"
  )
)

str(obesidade)


meio_transporte_obesidade_tb = table(obesidade$Meio_Transporte,obesidade$Nivel_Obesidade);meio_transporte_obesidade_tb

meio_transporte_obesidade_tb <- with(obesidade,table(obesidade$Meio_Transporte,obesidade$Nivel_Obesidade))

meio_transporte_obesidade_prop <- prop.table(meio_transporte_obesidade_tb)

meio_transporte_obesidade_prop_porcentagem <- prop.table(meio_transporte_obesidade_tb) * 100

meio_transporte_obesidade_prop_linha <- prop.table(meio_transporte_obesidade_tb, margin = 1)
meio_transporte_obesidade_prop_linha_pct <- round(meio_transporte_obesidade_prop_linha * 100, 2)


meio_transporte_obesidade_prop_porcentagem
meio_transporte_obesidade_prop_linha_pct
meio_transporte_obesidade_prop_linha
addmargins(meio_transporte_obesidade_prop_porcentagem)




barplot(meio_transporte_obesidade_tb,
        main = "Obesidade x Meio de Transporte",
        col = c("green", "blue", "yellow", "magenta", "lightblue"),
        beside = TRUE,
        legend = TRUE,
        args.legend = list(x = "topright", bty = "n", ncol = 1))

# Teste do qui-quadrado com simulações Monte Carlo bom para distribuições Assimétricas
set.seed(100)
B <- 10000
teste_chi <- chisq.test(meio_transporte_obesidade_tb, simulate.p.value = TRUE, B = B)
resultado_chi <- data.frame(
  Estatistica = round(teste_chi$statistic, 2),
  Graus_de_Liberdade = NA, # NA pois na simulação não é usado diretamente
  P_valor = ifelse(teste_chi$p.value < 1/(B+1),
                   paste0("< ", format(1/(B+1), scientific = TRUE)),
                   format.pval(teste_chi$p.value, digits = 4, eps = 1e-4)),
  Conclusao = ifelse(teste_chi$p.value < 0.05,
                     "Associação significativa entre as variáveis",
                     "Não há evidência de associação significativa")
)

boxplot(Peso ~ Comida_Alta_Caloria, data = obesidade,
        col = c("lightblue", "lightgreen"),
        main = "Peso Corporal x Consumo de Comida Calórica",
        xlab = "Consome Comida Alta em Calorias?",
        ylab = "Peso (Kg)",
        las = 1)

aggregate(Peso ~ Comida_Alta_Caloria, data = obesidade,
          FUN = function(x) c(n = length(x),
                              mean = mean(x),
                              sd = sd(x),
                              se = sd(x)/sqrt(length(x))))


t_test_result <- t.test(Peso ~ Comida_Alta_Caloria, data = obesidade)
print(t_test_result)


numerica_colunas <- sapply(obesidade, is.numeric)
obesidade_numerica <- obesidade[, numerica_colunas]

cor_mat <- cor(obesidade_numerica)
print(round(cor_mat, 2))

ut <- upper.tri(cor_mat, diag = FALSE)
top_df <- data.frame(
  var1 = rownames(cor_mat)[row(cor_mat)[ut]],
  var2 = colnames(cor_mat)[col(cor_mat)[ut]],
  cor  = cor_mat[ut]
)
top_df <- top_df[order(abs(top_df$cor), decreasing = TRUE), ]
head(top_df, 10)
paleta <- colorRampPalette(c("#4575b4", "white", "#d73027"))(200)
n <- ncol(cor_mat)
op  <- par(mar = c(6, 6, 2, 2))
im<- ncol(cor_mat)
image(1:n, 1:n, t(cor_mat[n:1, ]), col = paleta, axes = FALSE,
      xlab = "", ylab = "", zlim = c(-1, 1))
axis(1, at = 1:n, labels = colnames(cor_mat), las = 2, cex.axis = 0.9)
axis(2, at = 1:n, labels = rev(rownames(cor_mat)), las = 2, cex.axis = 0.9)
for (i in 1:n) for (j in 1:n) {
  valor <- round(cor_mat[j, i], 2)
  text(i, n - j + 1, labels = format(valor, nsmall = 2), cex = 0.8)
}
par(op)


IMC <- obesidade_numerica$Peso / (obesidade_numerica$Altura ** 2)
cor.test(obesidade_numerica$Peso,obesidade_numerica$Altura)
cor.test(obesidade_numerica$Atividade_Fisica,obesidade_numerica$Peso)
cor.test(obesidade_numerica$Agua_Diaria,obesidade_numerica$Refeicoes_Diarias)
cor.test(IMC,obesidade_numerica$Atividade_Fisica)
cor.test(IMC,obesidade_numerica$Altura)


x <- obesidade_numerica$Altura
y <- obesidade_numerica$Peso



cor_xy <- cor(x, y, use = "complete.obs", method = "pearson")
cof_det <- cor_xy^2
cof_det

modelo <- lm(y ~ x) 
summary(modelo)

b0 <- coef(modelo)[1]
b1 <- coef(modelo)[2]
r2 <- summary(modelo)$r.squared


all.equal(r2, cof_det)


plot(x, y,
     pch = 19, col = "#2c7fb488",
     xlab = "Altura (m)", ylab = "Peso",
     main = "Peso vs Altura")
abline(modelo, col = "#d73027", lwd = 2)


eq <- paste0("y = ", round(b0, 2), " + ", round(b1, 2), "·x")
correlacao <- cor.test(x, y)
leg <- paste0("r = ", round(correlacao$estimate, 3),
              " | p = ", formatC(correlacao$p.value, format = "e", digits = 2),
              " | R² = ", round(r2, 3))
legend("topleft", legend = c(eq, leg), bty = "n", cex = 0.9)


grupo_sim <- subset(obesidade, Historico_Familiar_Obesidade == "Sim")
grupo_nao <- subset(obesidade, Historico_Familiar_Obesidade == "Não")
unique(obesidade$Historico_Familiar_Obesidade)


modelo_sim <- lm(Peso ~ Altura, data = grupo_sim)
modelo_nao <- lm(Peso ~ Altura, data = grupo_nao)


summary(modelo_sim)
summary(modelo_nao)
plot(grupo_sim$Altura, grupo_sim$Peso,
     pch = 16, col = "red",
     xlab = "Altura", ylab = "Peso",
     main = "Peso vs Altura por Historico Familiar")

points(grupo_nao$Altura, grupo_nao$Peso,
       pch = 17, col = "blue")

# Retas de regressão
abline(modelo_sim, col = "red", lwd = 2)
abline(modelo_nao, col = "blue", lwd = 2)

legend("topleft",
       legend = c("Historico: Sim", "Historico: Nao"),
       col = c("red", "blue"),
       pch = c(16, 17))


modelo_interacao <- lm(Peso ~ Altura * Historico_Familiar_Obesidade , data = obesidade)
summary(modelo_interacao)

obesidade$IMC <- obesidade$Peso / (obesidade$Altura)^2
t.test(IMC ~ Historico_Familiar_Obesidade, data = obesidade)

boxplot(IMC ~ Historico_Familiar_Obesidade, data = obesidade,
        col = c("blue", "red"), main = "IMC por Histórico Familiar",
        ylab = "IMC", xlab = "Histórico Familiar de Obesidade")

modelo_imc <- lm(IMC ~ Historico_Familiar_Obesidade, data = obesidade)
summary(modelo_imc)

## 3 Perguntas:

# 1.Existe associação entre o tipo de transporte utilizado e o nível de obesidade?

# 2.Indivíduos que consomem comida calórica frequentemente tendem a ter maior peso corporal?

# 3."A relação entre altura e peso corporal apresenta diferenças significativas entre indivíduos com e sem histórico familiar de obesidade?"



