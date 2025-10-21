getwd()

dados <- read.csv("Pokemon_full.csv")

head(dados)# olha as primeiras linhas

library(tidyverse)

dados <- read.csv("~Aulas R/Pokemon_full.csv")

dados <- read.csv("C:/Users/Bell Bonfim/UNESP/Aulas R/Pokemon_full.csv")

names(dados)

# Seleciona colunas

select(dados, name, hp, speed, attack)

select(dados, speed)

#filtra colunas
filter(dados,attack < 50)

# Operações
mutate(dados, x = attack+speed)
mutate(dados, attack = attack/2)
mutate(dados, IMC = weight/(height*height))

dados <- mutate(dados, IMC = weight/(height*height))

# operador. elementos à esquerda tornam-se primeiro argumento

df <- select(dados, name, hp, attack, speed)
df <- filter(df, attack < 50)
df <- mutate(df, x = attack+speed)
df

dados %>%
  select(name, hp, attack, speed) %>%
  filter(attack < 50) %>%
  mutate(x = attack+speed)

# gsub modifica modifica caracteres.

x <- c("IsabeL", "Morgana", "Leônidas")
x %>%
gsub("L", "l")


dados %>% 
  filter(height > 10) %>% 
  select(name, height, weight) %>% 
  mutate(imc = weight/(height*height)) %>% 
  ggplot()+
  geom_density(aes(x = imc))

head(dados)
dados %>% head
head # primeiras linhas

# interesante. 

glimpse(dados) # estrutura, tipo de variável, primeiros valores, problemas
summary(dados)
str(dados) 

dados %>% pull(IMC) # vetor 
dados %>% select(IMC) # coluna

dados %>% pull(weight)
dados %>% select(weight)

dados %>% pull(speed)
dados %>% select(speed)

media = mean()

dados %>% 
  mutate(mean(IMC))

dados %>% 
  summarise(mean(IMC), desvio = sd(IMC)) #resume dados em uma coluna

dados %>% 
  group_by(type) %>% 
  summarise(mean(IMC), sd(IMC))

dados %>% 
  group_by(type) %>% 
  mutate(mean(IMC), sd(IMC))

dados %>% 
  group_by(type) %>% 
  mutate(mean(IMC), sd(IMC))

dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC), desvio = sd(IMC)) %>% 
  filter(IMC > media)

dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC)) %>% 
  filter(IMC > media)

df <- dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC))

dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC)) %>% 
  filter(IMC > media) %>% view

df %>% 
  ungroup() %>% 
  mutate(media2 = mean(IMC))

grep("saur", dadosSname)

grep("saur", dados$name)
grepl("chu", dados$name) 
grep("saur|[Ff]fly", dados$name)

grep("[Ss]aur", dados$name)

n <- c("097.765.986-90", "123.765.98-37")
grepl("\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}", n)

grepl("[Dd]i[óo]xido *de\\s+[eE]nxofre", x)

grepl(".", c("a", "b", "c", "0", " "))

dados %>% 
  filter(attack > 50)

dados$attack > 50

dados %>% 
  filter(grepl("saur|fly", name), attack > 50, type != "grass")

grepl("saur", "ivysaur")
df1 <- dados %>% 
  filter(attack > 80)

df2 <- dados %>% 
  filter(attack <= 80)

rbind(df1, df2)

df1 <- dados %>% 
  select(attack, speed, height) %>% 
  filter(attack > 80)

df2 <- dados %>% 
  select(attack, weight, height, hp) %>% 
  filter(attack <= 80)

rbind(df1, df2)

bind_rows(df1, df2)

df1 <- dados %>% head(100)
df2 <- dados %>% tail(100)

cbind(df1, df2) %>% names

bind_cols(df1, df2, .name_repair = "check_unique")


df_resumo <- dados %>% 
  group_by(type) %>% 
  summarise(mean(IMC), sd(IMC))

# Join

# right, left, full, inner

left_join(dados, df_resumo, by = "type")

left_join(dados, df_resumo, by = c("type"))
df_resumo_mis <- df_resumo %>%  filter(type != "grass")

left_join(dados, df_resumo_mis, by = c("type"))

right_join(dados, df_resumo_mis, by = c("type")) 

right_join(dados, df_resumo_mis, by = c("type"))

left_join(dados, df_resumo_mis, by = c("type"))

full_join(dados, df_resumo_mis, by = c("type")) %>% View

df_resumo_mis$type[5] = "thomas" 
right_join(dados, df_resumo_mis, by = c("type")) %>% View

left_join(dados, df_resumo_mis, by = c("type"))
