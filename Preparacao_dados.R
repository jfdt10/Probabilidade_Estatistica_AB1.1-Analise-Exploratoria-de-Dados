rm(list = ls()) ## Limpar memoria

setwd("C:/Users/Jean/Desktop/Trabalho_Probabilidade")

Obesidade_raw <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv", header = TRUE,
                    sep = ",", dec = ".")

head(Obesidade_raw)

summary(Obesidade_raw)

names(Obesidade_raw)


# Selecionar colunas corretas
Obesidade_clean <- Obesidade_raw[, c(
  "Gender", "Age", "Weight", "Height", "family_history_with_overweight",
  "FAVC", "FCVC", "NCP", "CAEC", "SMOKE", "CH2O", "SCC",
  "FAF", "TUE", "CALC", "MTRANS", "NObeyesdad"
)]

# Renomear colunas
names(Obesidade_clean) <- c(
  "Genero", "Idade", "Peso", "Altura", "Historico_Familiar_Obesidade",
  "Comida_Alta_Caloria", "Consumo_Vegetais", "Refeicoes_Diarias", "Comer_Entre_Refeicoes",
  "Fumante", "Agua_Diaria", "Monitora_Calorias", "Atividade_Fisica",
  "Uso_Tecnologia", "Consumo_Alcool", "Meio_Transporte", "Nivel_Obesidade"
)

# Vetor de substituições
valores_antigos <- c("Male", "Female")
valores_novos <- c("Masculino", "Feminino")

# Substituir usando match
Obesidade_clean$Genero <- valores_novos[match(Obesidade_clean$Genero, valores_antigos)]

valores_antigos1 <- c("yes","no")
valores_novos1 <- c("Sim","Não")

Obesidade_clean$Historico_Familiar_Obesidade <- valores_novos1[match(Obesidade_clean$Historico_Familiar_Obesidade, valores_antigos1)]

Obesidade_clean$Comida_Alta_Caloria <- valores_novos1[match(Obesidade_clean$Comida_Alta_Caloria, valores_antigos1)]

Obesidade_clean$Fumante <- valores_novos1[match(Obesidade_clean$Fumante, valores_antigos1)]

Obesidade_clean$Monitora_Calorias <- valores_novos1[match(Obesidade_clean$Monitora_Calorias, valores_antigos1)]

valores_antigos2 <- c("no","Sometimes","Frequently","Always")
valores_novos2 <- c("Não","Às Vezes","Frequentemente","Sempre")

Obesidade_clean$Comer_Entre_Refeicoes <- valores_novos2[match(Obesidade_clean$Comer_Entre_Refeicoes, valores_antigos2)]
Obesidade_clean$Consumo_Alcool <- valores_novos2[match(Obesidade_clean$Consumo_Alcool, valores_antigos2)]

table(Obesidade_clean$Meio_Transporte)

valores_antigos3 <- c("Automobile","Bike","Motorbike","Public_Transportation","Walking")
valores_novos3 <- c("Carro","Bicicleta","Moto","Transporte Público","À Pé")

Obesidade_clean$Meio_Transporte <- valores_novos3[match(Obesidade_clean$Meio_Transporte, valores_antigos3)]

table(Obesidade_clean$Nivel_Obesidade)

valores_antigos4 <- c("Insufficient_Weight","Normal_Weight","Overweight_Level_I","Overweight_Level_II","Obesity_Type_I","Obesity_Type_II","Obesity_Type_III")
valores_novos4 <- c("Abaixo do Peso","Peso Normal","Sobrepeso Nível 1","Sobrepeso Nível 2","Obesidade Nível 1","Obesidade Nível 2","Obesidade Nível 3")

Obesidade_clean$Nivel_Obesidade <- valores_novos4[match(Obesidade_clean$Nivel_Obesidade,valores_antigos4)]

Obesidade_clean$Peso <- round(Obesidade_clean$Peso, 1)
Obesidade_clean$Altura <- round(Obesidade_clean$Altura, 2)

Obesidade_clean$Idade <- as.integer(round(Obesidade_clean$Idade))
Obesidade_clean$Agua_Diaria <- round(Obesidade_clean$Agua_Diaria,2)

Obesidade_clean$Uso_Tecnologia <- as.integer(Obesidade_clean$Uso_Tecnologia)

Obesidade_clean$Atividade_Fisica <- round(Obesidade_clean$Atividade_Fisica,1)

valores_antigos5 <- c("0","1","2")
valores_novos5 <- c("Pouco","Moderado","Prolongado")

Obesidade_clean$Uso_Tecnologia <- valores_novos5[match(Obesidade_clean$Uso_Tecnologia,valores_antigos5)]

Obesidade_clean$Consumo_Vegetais <- as.integer(Obesidade_clean$Consumo_Vegetais)

Obesidade_clean$Refeicoes_Diarias <- as.integer(Obesidade_clean$Refeicoes_Diarias)

valores_antigos5 <- c("1","2","3")
valores_novos5 <- c("Pouco","Frequentemente","Sempre")

Obesidade_clean$Consumo_Vegetais <- valores_novos5[match(Obesidade_clean$Consumo_Vegetais,valores_antigos5)]

typeof(Obesidade_clean$Consumo_Vegetais)

str(Obesidade_clean)


dados <- c("Genero",
          "Historico_Familiar_Obesidade",
          "Comida_Alta_Caloria",
          "Consumo_Vegetais",
          "Comer_Entre_Refeicoes",
          "Fumante",
          "Monitora_Calorias",
          "Uso_Tecnologia",
          "Consumo_Alcool",
          "Meio_Transporte",
          "Nivel_Obesidade")


Obesidade_clean[dados] <- lapply(Obesidade_clean[dados], factor)

write.csv(Obesidade_clean,
          file = "Obesidade_clean.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")



library(openxlsx)

# Exporta para arquivo Excel
write.xlsx(Obesidade_clean, file = "Obesidade_clean.xlsx")


