install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(ggplot2)

getwd()

setwd("C:/Users/jande/Documentos/Estudo de Caso/datasets/trip_files")


# lendo alguns dos arquivos csv
trips01 <- read.csv("202201-divvy-tripdata.csv")
trips02 <- read.csv("202202-divvy-tripdata.csv")
trips03 <- read.csv("202203-divvy-tripdata.csv")
trips04 <- read.csv("202204-divvy-tripdata.csv")
trips05 <- read.csv("202205-divvy-tripdata.csv")
trips06 <- read.csv("202206-divvy-tripdata.csv")
trips07 <- read.csv("202207-divvy-tripdata.csv")
trips08 <- read.csv("202208-divvy-tripdata.csv")
trips09 <- read.csv("202209-divvy-tripdata.csv")
trips10 <- read.csv("202210-divvy-tripdata.csv")
trips11 <- read.csv("202211-divvy-tripdata.csv")
trips12 <- read.csv("202212-divvy-tripdata.csv")
# comparando se nomes de colunas são iguais
list(colnames(trips01),
colnames(trips02),
colnames(trips03),
colnames(trips04),
colnames(trips05),
colnames(trips06),
colnames(trips07),
colnames(trips08),
colnames(trips09),
colnames(trips10),
colnames(trips11),
colnames(trips12))

str(trips01)
str(trips02)
str(trips03)

# unindo data frames em um único

all_trips <- list.files(path='C:/Users/jande/Documentos/Estudo de Caso/datasets/trip_files/divvy_tripdata_2022') %>% 
  lapply(read_csv) %>% 
  bind_rows

glimpse(all_trips)


View(all_trips)

# data cleaning e preparacao para análise
#inspecionando nova tabela
colnames(all_trips) # quais colunas
nrow(all_trips) # numero de linhas
dim(all_trips) # dimensoes do df
head((all_trips)) # 6 primeiras linhas do df
str(all_trips) # lista de colunas e tipos de dado
summary(all_trips) # resumo estatistico dos dados
glimpse(all_trips)

# buscando possiveis erros de digitação
table(all_trips$member_casual)
table(all_trips$rideable_type)

# adicionando colunas data, mes, dia e ano para agreg futuras
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# verificando e removendo valores nulos
sum(is.na(all_trips))
all_trips_v2 <- all_trips %>% drop_na()
sum(is.na(all_trips_v2))


# adicionando duracao da viagem em segundos
all_trips_v2$duracao_passeio <- difftime(all_trips_v2$ended_at, all_trips_v2$started_at)

str(all_trips_v2)
nrow(all_trips_v2)
# convertendo duracao_passeio para numerico
all_trips_v2$duracao_passeio <- as.numeric(as.character(all_trips_v2$duracao_passeio))
is.numeric(all_trips_v2$duracao_passeio)

# removendo duracao_passeio negativa
all_trips_v2 <- all_trips_v2[!(all_trips_v2$start_station_name == "HQ QR" | all_trips_v2$duracao_passeio<0),]

table(all_trips_v2$duracao_passeio)

# analise descritiva
mean(all_trips_v2$duracao_passeio)
median(all_trips_v2$duracao_passeio)
max(all_trips_v2$duracao_passeio)
min(all_trips_v2$duracao_passeio)

summary(all_trips_v2$duracao_passeio)

# comparando membros e usuarios casuais
aggregate(all_trips_v2$duracao_passeio ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$duracao_passeio ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$duracao_passeio ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$duracao_passeio ~ all_trips_v2$member_casual, FUN = min)


# media de corridas por dia membros vs usuarios casuais
aggregate(all_trips_v2$duracao_passeio ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("domingo", "segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado"))


# media de corridas por dia por tipo de corrida
aggregate(all_trips_v2$duracao_passeio ~ all_trips_v2$rideable_type + all_trips_v2$day_of_week, FUN = mean)

# construindo vizualizacoes com ggplot2

#vizualizacao media de viagens por tipo de membro e dia da semana
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duracao_passeio)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# vizualizacao para media de duracao de viagem por dia da semana
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,duraçao_média = mean(duracao_passeio)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = duraçao_média, fill = member_casual)) +
  geom_col(position = "dodge")


# exportando para csv
write.csv(all_trips_v2, "C:\\Users\\jande\\Documentos\\estudo_de_caso\\divvy_trip_data_2022.csv", row.names=FALSE)
