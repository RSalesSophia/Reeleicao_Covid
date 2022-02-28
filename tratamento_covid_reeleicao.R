#Pacotes
library(dplyr)
library(magrittr)
library(electionsBR)
library(fuzzyjoin)
library(ggplot2)
library(extrafont)
library(jtools)


#Configurações de tela 
options(scipen=999)
windowsFonts(Times=windowsFont("TT Times New Roman"))
options(OutDec= ",")
options(digits = 3)

#Trabalhando com as informações relativos as características pessoais dos candidatos

#Importando as informações direto do pacote 'electionsBR'
informacao_16 <- candidate_local(2016)

#Exportando essas informações para o computador
write.csv(informacao_16,'informacao_2016.csv', row.names =FALSE)
#-------------------------------------------------------------------------------#
#Importando a informação de 2016
informacao_16 = read.csv("informacao_2016.csv")

#Selecionando algumas variáveis e aplicando filtro do turno disputado
#Eleitos no 1º turno
turno_1 = informacao_16 %>%
  filter(DS_CARGO == "PREFEITO" & CD_TIPO_ELEICAO == 2 & NR_TURNO == 1 &
           DS_SIT_TOT_TURNO == "ELEITO") %>%
  select(SG_UE, NM_UE, NM_CANDIDATO, DT_ELEICAO)

#Eleitos no 2º turno           
turno_2 = informacao_16 %>%
  filter(DS_CARGO == "PREFEITO" & CD_TIPO_ELEICAO == 2 & NR_TURNO == 2 &
           DS_SIT_TOT_TURNO == "ELEITO") %>%
  select(SG_UE, NM_UE, NM_CANDIDATO, DT_ELEICAO)

#União dos dois turnos
inf_16  = rbind(turno_1, turno_2)

#Tendo em vista que pode ter prefeitos eleitos em 2016 podem ter sido
#cassados ou deixado o cargo, iremos filtrar as eleições suplementares
#e considerar os candidatos eleitos nessas eleições
#Isto é, caso tenha ocorrido uma eleição suplementar no município, serão 
#considerados os candidatos eleitos dessa eleição, não da eleição ordinária.

suple = informacao_16 %>%
  filter(DS_CARGO == "PREFEITO" & CD_TIPO_ELEICAO == 1 & DS_SIT_TOT_TURNO == "ELEITO") %>%
  select(SG_UE, NM_UE, NM_CANDIDATO, DT_ELEICAO)

#Fazer um anti via SIGLA_UE: as informações das eleições ordinárias
#dos municípios que tiveram as suplementares serão excluídas em inf_16
#A intenção aqui é considerar o último prefeito do municipio para aquela eleição
anti = anti_join(inf_16, suple, by = "SG_UE")

#Unindo a base anti com suple
inf_16 = rbind(anti, suple)

#Conferindo se as informações são distintas 
n_distinct(inf_16$SG_UE)

#Tem uma informação referente a sg_ue que não é única
#Descobrindo quem é: 

duplicado = inf_16 %>% 
  group_by(SG_UE) %>% 
  mutate(n = n()) %>%
  filter(n == 2)

#O município de TIANGUÁ teve duas eleições suplementar
#Então vamos excluir de inf_16 a informação mais nova - DATA: 03/06/2018
inf_16 %<>%
  filter(!(SG_UE == 15695 & DT_ELEICAO == "03/06/2018"))

#Criando uma chave com o nome do candidato e da cidade
inf_16 %<>%
  mutate(CHAVE = paste0(NM_UE, NM_CANDIDATO))

#Retirando todos os acentos dessa chave
inf_16$CHAVE = stri_trans_general(str = inf_16$CHAVE, id = "Latin-ASCII")

#Retirando os espaços brancos
inf_16$CHAVE = gsub(" ", "", inf_16$CHAVE, fixed = TRUE)

#Retirando os apóstrofos
inf_16$CHAVE = gsub("'", "", inf_16$CHAVE, fixed = TRUE)

#Retirando os traços
inf_16$CHAVE = gsub("-", "", inf_16$CHAVE, fixed = TRUE)
#-------------------------------------------------------------------------#
#Importando a informação de 2020
informacao_20 = read.csv("informacao_2020.csv")

#Eleitos no 1º turno
turno_1 = informacao_20 %>%
  filter(DS_CARGO == "PREFEITO" & CD_TIPO_ELEICAO == 2 & NR_TURNO == 1
         & DS_SITUACAO_CANDIDATURA == "APTO") %>%
  select(SG_UE, NM_UE, NM_CANDIDATO, DS_SIT_TOT_TURNO)

#Eleitos no 2º turno           
turno_2 = informacao_20 %>%
  filter(DS_CARGO == "PREFEITO" & CD_TIPO_ELEICAO == 2 & NR_TURNO == 2 &
           DS_SITUACAO_CANDIDATURA == "APTO") %>%
  select(SG_UE, NM_UE, NM_CANDIDATO, DS_SIT_TOT_TURNO)

#Unindo as bases os candidatos que ganharam/perderam definitivamente em
#eleições de primeiro turno e do segundo

#1º Passo = Fazer o anti-join das bases
#O objetivo dessa base é apenas ter os candidatos que não chegaram a disputar o segundo turno. 
#Isso é necessário pois candidatos que chegam a ir para o segundo turno,
#também disputam o primeiro turno.
#Não queremos informações duplicadas dos candidatos, queremos apenas os votos que culminaram na perda ou não da eleição.

anti = anti_join(turno_1, turno_2, by = "SG_UE")

#2º Passo = Unir essa base dos candidatos que apenas disputuram o primeiro turno
#e tiveram seu resultado final nesse turno e os candidatos que decidiram a eleição
#no segundo turno. 

inf_20 = rbind(anti, turno_2)

#Fazendo uma descrição da variável DS_SIT_TOT_TURNO
inf_20 %<>%
  mutate(DS_SIT_TOT_TURNO = as.factor(DS_SIT_TOT_TURNO))

summary(inf_20$DS_SIT_TOT_TURNO)

#Excluindo aqueles candidatos que informações nulas
inf_20 %<>%
  filter(DS_SIT_TOT_TURNO != "#NULO#")

#Criando uma chave com parte do nome e da cidade
inf_20 %<>%
  mutate(CHAVE = paste0(NM_UE, NM_CANDIDATO)) %>%
  select(-NM_CANDIDATO)

#Retirando todos os acentos dessa chave
inf_20$CHAVE = stri_trans_general(str = inf_20$CHAVE, id = "Latin-ASCII")

#Retirando os espaços brancos
inf_20$CHAVE = gsub(" ", "", inf_20$CHAVE, fixed = TRUE)

#Retirando os apóstrofos
inf_20$CHAVE = gsub("'", "", inf_20$CHAVE, fixed = TRUE)

#Retirando os traços
inf_20$CHAVE = gsub("-", "", inf_20$CHAVE, fixed = TRUE)

#Descobrindo quais candidatos de 2016 disputaram também em 2020
base = stringdist_inner_join(x = inf_20, y = inf_16, by = "CHAVE",
                             max_dist = 4)

##Excluindo informações a união foi incorreta
#Se o SG_UE do inf_20 for diferente de inf_16, quer dizer que a união foi
#incorreta
base %<>%
  mutate(CONF = if_else((SG_UE.x == SG_UE.y) == TRUE, 1,0)) %>%
  filter(CONF == 1)

#Excluindo as variáveis
base %<>%
  rename(SG_UE = SG_UE.x) %>%
  select(SG_UE, DS_SIT_TOT_TURNO, NM_CANDIDATO) %>%
  mutate(SG_UE = as.character(SG_UE))

#------------------------------------------------------------------------------#
#Unindo a base anterior com a base que tem os código do município (SG_UE) equivalente ao IBGE

#Importando a base de códigos do IBGE e do TSE
codigo = read.csv("codigotse_ibge.csv", sep = ";")

#Selecionando apenas as colunas referentes aos códigos
codigo %<>% 
  mutate(SG_UE = as.character(codigo_tse)) %>%
  select(SG_UE, codigo_ibge)

#Fazendo o merge entre a base de códigos com a info
base = inner_join(base, codigo, by = "SG_UE")
#------------------------------------------------------------------------------#
#Importando dados sobre casos e obitos de covid até 14/11/2020 (um dia antes da 
#eleição)

casos_covid = read.csv("covid-19(dados_municipios).csv", encoding = "UTF-8")

#Descrição da base de dados
glimpse(casos_covid)

#Selecionando variáveis
casos_covid %<>%
  rename(CODUFMUN = city_ibge_code,
                CASOS_ACUMULADOS = last_available_confirmed_per_100k_inhabitants) %>%
  mutate(OBITOS_ACUMULADOS = (last_available_deaths/estimated_population)*100000) %>%
  select(CODUFMUN,CASOS_ACUMULADOS, OBITOS_ACUMULADOS) %>%
  arrange(CODUFMUN)

#Sabe-se que os códigos do CODUFMUN de 11 a 53 são referentes ao estado.
#Portanto pode-se excluir essas informações
casos_covid %<>%
  filter(CODUFMUN < 11 | CODUFMUN > 53) 

#Unindo esse dataframe com a base:
base = left_join(base, casos_covid, by = c("codigo_ibge" = "CODUFMUN"))

#Exportando a base:
write.csv(base,'covid_reeleicao.csv', row.names =FALSE)
#------------------------------------------------------------------------------#
#A variável dependente desse modelo consiste em: dos candidatos que 
#tentavam a reeleição quais ganharam

base = read.csv("covid_reeleicao.csv")

#Criando a variável:
base %<>% 
  mutate(Reeleito = if_else(DS_SIT_TOT_TURNO == "ELEITO", 1,0)) %>%
  rename("Óbitos Acumulados" = OBITOS_ACUMULADOS)

#Descrição da variável OBITOS ACUMULADOS
summary(base$`Óbitos Acumulados`)

#Rodando um modelo logit
regressao = glm(Reeleito ~ `Óbitos Acumulados`, family = binomial(link="logit"), data = base)
summary(regressao)

# Criando campo de predição de probabilidade
base$PRED=predict(regressao, newdata=base, type="response")

#Gráfico da probabilidade 
ggplot(base, aes(x=`Óbitos Acumulados`, y = PRED, colour = PRED)) + 
  geom_point(size = 3) +
  scale_colour_gradientn(colours = c("deepskyblue3", "deeppink", "darkorchid1")) +
  labs(x = "Óbitos acumulados por COVID-19 (100k habitantes)", y = "Probabilidade predita",
       title = "Probabilidade de ser reeleito e nº de óbitos acumulados por COVID-19",
       caption = "Data: TSE (2020) e  Brasil.io (2020)") +
  theme_classic(base_family = "Times New Roman", base_size = 11) +
  theme(legend.position="none", 
        axis.title.y = element_text(vjust=3),
        plot.title = element_text(family = "Times New Roman", size = (15), hjust = 0.5)) 




