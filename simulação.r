# Importação do pacote gráfico
library(ggplot2)

# Criação da matriz de transição e do vetor de probabilidade
matriz.transição <- matrix(c(0.40,0.25,0,0.40,0.50,0.35,0.20,0.25,0.65),3,3)
vetor.probabilidade <- matrix(c(0.70,0.15,0.15),1,3)

# Criação dos restaurantes noturnos
restaurante.japonês <- NULL
restaurante.chinês <- NULL
restaurante.italiano <- NULL

# Quantitativo de pessoas e de noites
pessoas.totais <- 1000
pessoas <- c(1:pessoas.totais)
noites.totais <- 60
noites <- c(1:noites.totais)

# Histórico das pessoas nos restautantes por noite
histórico <- data.frame(n = NULL, p = NULL, r = NULL, q = NULL)

### n = noite, p = pessoa, r = restaurante, q = quantidade

# Transição das pessoas nos restaurantes e registro dos seus históricos
for (pessoa in pessoas) {
  
  prob <- runif(1) # Valor aleatório entre 0 e 1
  p1 <- vetor.probabilidade[1]
  p2 <- vetor.probabilidade[2]
  p3 <- vetor.probabilidade[3]
  
  if (prob <= p1) {
    restaurante.japonês <- append(restaurante.japonês, pessoa)
    histórico <- rbind(histórico, data.frame(n = 1, p = pessoa, r = "japonês", q = 1))
  }
  else if (prob <= p1 + p2) {
    restaurante.chinês <- append(restaurante.chinês, pessoa)
    histórico <- rbind(histórico, data.frame(n = 1, p = pessoa, r = "chinês", q = 1))
  }
  else {
    restaurante.italiano <- append(restaurante.italiano, pessoa)
    histórico <- rbind(histórico, data.frame(n = 1, p = pessoa, r = "italiano", q = 1))
  }
  
  if (length(noites) == 1) {break}
  
  for (noite in noites[-1]) {
    
    prob <- runif(1) # Valor aleatório entre 0 e 1
    
    if (pessoa %in% restaurante.japonês) {
      
      p12 <- matriz.transição[1,2]
      p13 <- matriz.transição[1,3]
      
      if (prob <= p12) {
        restaurante.chinês <- append(restaurante.chinês, pessoa)
        restaurante.japonês <- restaurante.japonês[restaurante.japonês != pessoa]
        histórico <- rbind(histórico, data.frame(n = noite, p = pessoa, r = "chinês", q = 1))
        next
      }
      else if (prob <= p12 + p13) {
        restaurante.italiano <- append(restaurante.italiano, pessoa)
        restaurante.japonês <- restaurante.japonês[restaurante.japonês != pessoa]
        histórico <- rbind(histórico, data.frame(n = noite, p = pessoa, r = "italiano", q = 1))
        next
      }
      else {
        histórico <- rbind(histórico, data.frame(n = noite, p = pessoa, r = "japonês", q = 1))
        next
      }
      
    }
    else if (pessoa %in% restaurante.chinês) {
      
      p21 <- matriz.transição[2,1]
      p23 <- matriz.transição[2,3]
      
      if (prob <= p21) {
        restaurante.japonês <- append(restaurante.japonês, pessoa)
        restaurante.chinês <- restaurante.chinês[restaurante.chinês != pessoa]
        histórico <- rbind(histórico, data.frame(n = noite, p = pessoa, r = "japonês", q = 1))
        next
      }
      else if (prob <= p21 + p23) {
        restaurante.italiano <- append(restaurante.italiano, pessoa)
        restaurante.chinês <- restaurante.chinês[restaurante.chinês != pessoa]
        histórico <- rbind(histórico, data.frame(n = noite, p = pessoa, r = "italiano", q = 1))
        next
      }
      else {
        histórico <- rbind(histórico, data.frame(n = noite, p = pessoa, r = "chinês", q = 1))
        next
      }
      
    }
    else {
      
      p31 <- matriz.transição[3,1]
      p32 <- matriz.transição[3,2]
      
      if (prob <= p31) {
        restaurante.japonês <- append(restaurante.japonês, pessoa)
        restaurante.italiano <- restaurante.italiano[restaurante.italiano != pessoa]
        histórico <- rbind(histórico, data.frame(n = noite, p = pessoa, r = "japonês", q = 1))
        next
      }
      else if (prob <= p31 + p32) {
        restaurante.chinês <- append(restaurante.chinês, pessoa)
        restaurante.italiano <- restaurante.italiano[restaurante.italiano != pessoa]
        histórico <- rbind(histórico, data.frame(n = noite, p = pessoa, r = "chinês", q = 1))
        next
      }
      else {
        histórico <- rbind(histórico, data.frame(n = noite, p = pessoa, r = "italiano", q = 1))
        next
      }
      
    }
    
  }
  
} ; quantitativo.final <- data.frame(
  restaurantes = c("japonês", "chinês", "italiano"),
  pessoas = c(
    length(restaurante.japonês),
    length(restaurante.chinês),
    length(restaurante.italiano)
  ),
  percentuais = c(
    paste(100*length(restaurante.japonês)/pessoas.totais,"%"),
    paste(100*length(restaurante.chinês)/pessoas.totais,"%"),
    paste(100*length(restaurante.italiano)/pessoas.totais,"%")
  )
) 

# Gráfico da Porcentagem de Pessoas nos Restaurantes por Noite
ggplot(histórico, aes(x = n, y = q, fill = r)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Percentual de Pessoas nos Restaurantes por Noite",
    x = "Noites",
    y = "Porcentagem",
    fill = "Restaurantes"
  ) +
  scale_fill_manual(values = c(
    "japonês" = "#99c1f0",
    "chinês"  = "#3184e4",
    "italiano"  = "#c060cb"
  )) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

# Gráfico da Proporção de Pessoas nos Restaurantes na Última Noite
par(mar = c(4, 4, 4, 4)) ; pie(
  main = paste("Proporção de Pessoas nos Restaurantes na", noites.totais, "ª noite"),
  x = quantitativo.final$pessoas,
  labels = quantitativo.final$percentuais,
  col = c("#99c1f0","#3184e4","#c060cb"),
  radius = 1
) ; legend(
  "right",
  legend = quantitativo.final$restaurantes,
  fill = c("#99c1f0","#3184e4","#c060cb")
)
