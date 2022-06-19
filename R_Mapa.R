
#carregando os bancos
mapa <- rgdal::readOGR(
  dsn = "https://raw.githubusercontent.com/jonates/opendata/master/arquivos_geoespaciais/unidades_da_federacao.json",
  use_iconv = T
)
mapa@data

dt <- utils::read.csv(
  file = 'https://raw.githubusercontent.com/jonates/opendata/master/projecao_IBGE_2018/projecao_IBGE_2018_atualizada06042020_semponto.csv',
  sep = ';',
  dec = ','
)
dt

#--------------------------------------------------------------------------------
#Estatística descritiva

#Carregando pacotes------------------------------------------------------------
install.packages('dplyr')
install.packages('lubridate')
install.packages('forcats')
install.packages('ggplot2')
install.packages('tibble')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('janitor')
install.packages('skimr')
install.packages('gmodels')
install.packages('psych')
install.packages('visdat')
install.packages('inspectdf')
install.packages("readOGR")
install.packages('dplyr')
install.packages("janitor")
install.packages("knitr")
install.packages("DT")
install.packages("kableExtra")
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(janitor)
library(skimr)
library(gmodels)
library(psych)
library(visdat)
library(inspectdf)
library(dplyr)


# Onde se encontra a Bahia no ranking de prevalÃªncia de obitos entre as UF?
# Construa uma tabela com duas colunas (Sigla_UF,obitos)


# SoluÃ§Ã£o 1
dtUF <- dt %>%
  dplyr:: filter(Sigla_UF$obito != "BR")
names(dt)

# SoluÃ§Ã£o 2
tab_prevObitos_UF <- dt %>%
  dplyr::select(Nome_UF, Obitos) %>%
  dplyr::filter(Nome_UF != 'Todas') %>%
  dplyr::arrange(desc(Obitos)) %>%
  tibble::view()

# Salvando o resultado em um arquivo csv
write.csv(
  x = tab_prevObitos_UF,
  file = './tabela_prevObitos_por_UF_20220106.csv',
  row.names = FALSE
)

#____________________________________________________________________________________



# Espiando a estrutura do conjunto de dados
dplyr::glimpse(dt)

# Overview das variÃ?Â¡veis
skimr::skim(dt)


# Verificando os valores missing de forma visual
is.na(dt)


# AnÃ?Â¡lise descritiva ------------------------------------------------------


# Filtrando somente BAHIA
df_BA <- dt %>% filter(Nome_Regiao == "Nordeste")

# Verificando algumas estatÃ?Â­sticas descritivas
base::summary(dt %>% select(Populacao_Homens))
base::summary(dt %>% select(Populacao_Mulheres))

base::summary(dt %>% select(Obitos))
base::summary(dt %>% select(Nascimentos))

base::summary(dt %>% select(EVN_Homens))
base::summary(dt %>% select(EVN_Mulheres))



# EstatÃ?Â­sticas descritivas usando o dplyr estratificado por Nome_Regiao --> obito
dt %>%
  dplyr::group_by(Sigla_UF) %>%
  dplyr::summarise(
    min = min(Obitos, na.rm = TRUE),
    p5 = quantile(Obitos, 0.05, na.rm = TRUE ),
    q1 = quantile(Obitos, 0.25, na.rm = TRUE ),
    md = quantile(Obitos, 0.50, na.rm = TRUE ),
    q3 = quantile(Obitos, 0.75, na.rm = TRUE ),
    p95 = quantile(Obitos, 0.95, na.rm = TRUE ),
    max = max(Obitos, na.rm = TRUE ),
    media = mean(Obitos, na.rm = TRUE ),
    desvio_padrao = sd(Obitos, na.rm = TRUE ),
    cv = sd(Obitos, na.rm = TRUE )/mean(Obitos, na.rm = TRUE )*100,
    soma = sum(Obitos, na.rm = TRUE ),
    contagem = n()
  )


# EstatÃ?Â­sticas descritivas usando o dplyr estratificado por unidade da federaÃ?Â§Ã?Â£o --> Nascimentos
dt %>%
  dplyr::group_by(Sigla_UF) %>%
  dplyr::summarise(
    min = min(Nascimentos, na.rm = TRUE),
    p5 = quantile(Nascimentos, 0.05, na.rm = TRUE ),
    q1 = quantile(Nascimentos, 0.25, na.rm = TRUE ),
    md = quantile(Nascimentos, 0.50, na.rm = TRUE ),
    q3 = quantile(Nascimentos, 0.75, na.rm = TRUE ),
    p95 = quantile(Nascimentos, 0.95, na.rm = TRUE ),
    max = max(Nascimentos, na.rm = TRUE ),
    media = mean(Nascimentos, na.rm = TRUE ),
    desvio_padrao = sd(Nascimentos, na.rm = TRUE ),
    cv = sd(Nascimentos, na.rm = TRUE )/mean(Nascimentos, na.rm = TRUE )*100,
    soma = sum(Nascimentos, na.rm = TRUE ),
    contagem = n()
  ) %>%
  as.data.frame()


# AnÃ?Â¡lise descritiva estratifica com o pacote psych
psych::describeBy(
  x =dt$Nascimento,
  group = dt$Sigla_UF
)

# Fazendo GrÃ?Â¡ficos para variÃ?Â¡veis quantitativas contÃ?Â­nuas
hist(dt$Nascimentos)
boxplot(dt$Nascimentos)


hist(dt$Obitos)
boxplot(dt$Obitos)


# Boxplot da Taxa de Fecundidade de 15 a 19 anos por Nome_RegiÃ£o
grafico_txfecg1519 <- dt %>%
  dplyr:: filter(Sigla_UF != 'BR')
ggplot2::ggplot(data = grafico_txfecg1519)+  
  ggplot2::aes(x = Sigla_UF, y = TEF_15_19, fill = Nome_Regiao)+
  ggplot2::geom_boxplot()+
  ggplot2::labs(
    x = 'RegiÃ£o em UF',
    y = 'Taxa de fecundidade de 15 a 19 anos',
    fill = 'Nome da regiÃ£o'
  )+
  theme(legend.position = 'none')


# Boxplot Ã­ndice de Envelhecimento por Unidade da FederaÃ?Â§Ã?Â£o
ggplot2::ggplot(data = dt)+  
  ggplot2::aes(x = Sigla_UF, y = Indice_Envelhecimento, fill = Sigla_UF)+
  ggplot2::geom_boxplot()+
  ggplot2::labs(
    x = 'Unidade da FederaÃ§Ã£o',
    y = 'Indice de Envelhecimento'
  )+
  ggplot2::theme(legend.position = 'none')
#_____________________________________________________________________________

#Transformando tipo da variável
mapa@data$CD_GEOCUF<-as.integer(mapa@data$CD_GEOCUF)


#visualizar
dplyr::glimpse(mapa@data)
mapa@data

#Renomeando variável
dt<-dplyr:: rename(dt, CD_GEOCUF = Codigo_UF)

#_______________________________________________________FILTRAR
dt %>%
  filter(Ano == 2025 & Nome_UF != "Todas")->dt


#Unindo banco
mapa@data <- dplyr::left_join(
  x = mapa@data,
  y = dt,
  by = "CD_GEOCUF"
)

mapa@data%>%
  mutate(UF= case_when(
    CD_GEOCUF== 11 ~ "Rondônia (RO)",
    CD_GEOCUF== 12 ~ "Acre (AC)",
    CD_GEOCUF== 13 ~ "Amazonas (AM)",
    CD_GEOCUF== 14 ~ "Roraima (RR)",
    CD_GEOCUF== 15 ~ "Pará (PA)",
    CD_GEOCUF== 16 ~ "Amapá (AP)",
    CD_GEOCUF== 17 ~ "Tocantins (TO)",
    CD_GEOCUF== 21 ~ "Maranhão (MA)",
    CD_GEOCUF== 22 ~ "Piauí (PI)",
    CD_GEOCUF== 23 ~ "Ceará (CE)",
    CD_GEOCUF== 24 ~ "Rio Grande do Norte (RN)",
    CD_GEOCUF== 25 ~ "Paraíba (PB)",
    CD_GEOCUF== 26 ~ "Pernambuco (PE)",
    CD_GEOCUF== 27 ~ "Alagoas (AL)",
    CD_GEOCUF== 28 ~ "Sergipe (SE)",
    CD_GEOCUF== 29 ~ "Bahia (BA)",
    CD_GEOCUF== 31 ~ "Minas Gerais (MG)",
    CD_GEOCUF== 32 ~ "Espírito Santo (ES)",
    CD_GEOCUF== 33 ~ "Rio de Janeiro (RJ)",
    CD_GEOCUF== 35 ~ "São Paulo (SP)",
    CD_GEOCUF== 41 ~ "Paraná (PR)",
    CD_GEOCUF== 42 ~ "Santa Catarina (SC)",
    CD_GEOCUF== 43 ~ "Rio Grande do Sul (RS)",
    CD_GEOCUF== 50 ~ "Mato Grosso do Sul (MS)",
    CD_GEOCUF== 51 ~ "Mato Grosso (MT)",
    CD_GEOCUF== 52 ~ "Goiás (GO)",
    CD_GEOCUF== 53 ~ "Distrito Federal (DF)"
  )) ->mapa@data
  



# Pacotes -----------------------------------------------------------------

# install.packages("rgdal")
# install.packages("leaflet")

library(rgdal)
library(leaflet)
library(RColorBrewer)


# mapa@data$Populacao_Total <- format(mapa@data$Populacao_Total, big.mark = ".", decimal.mark = ",")
#Paleta de cores
library("viridis")           # Load
viridis(27)-> cores_27

# cores_27 <- brewer.pal(n = 9, name = "PiYG")

# #Criando vetor com de 15 cores
# cores_27 <-c("#9ACD32", "#FFD700","#808000","#D8BFD8","#DAA520","#00BFFF",
#              "#32CD32","#DEB887","#5F9EA0","#FF7F50","#20B2AA","#4169E1","#FFC0CB","#40E0D0",'#BC8F8F')
# 
#Criando paleta de cores
paleta_regioes_pesquisas <- leaflet::colorFactor(
  palette = cores_27,
  domain = factor(
    x = mapa$Populacao_Total,
    levels= (mapa$Populacao_Total)
  ))


# 
# paleta_regioes_pesquisas <- leaflet::colorNumeric(
#   palette = "viridis",
#   domain = range(mapa@data$Populacao_Total)
#   )

mapa@data$Populacao_Total <- format(mapa@data$Populacao_Total, big.mark = ".", decimal.mark = ",")


#Fazendo o mapa----------------------------------------------------
leaflet(
  data = mapa,
  options = leafletOptions (
    zoomControl = TRUE,
    minZoom = 1.0, maxZoom = 20.5,
    dragging = TRUE,
    doubleClickZoom = TRUE
  )
) %>%
  addTiles()%>%
  setView(lat = -10.64935694548079, lng = -50.52766173393505, zoom = 2.5)%>% 
  # setView(lat = -13.166149, lng = -41.559343, zoom = 6.0) %>% #Bahia
  addPolygons(
    stroke = T,
    opacity = 1,
    color = "black",
    weight = 0.5,
    smoothFactor = 0.0,
    fillOpacity = 1,
    label = lapply(paste0(" ", mapa$UF,"<br>","<b>População: ",mapa$Populacao_Total), htmltools::HTML),
                                 
                    #               html('', UF, "|População =", 
                    # <br>,<b>,
                    # Populacao_Total</b>),
                    
    fillColor = ~paleta_regioes_pesquisas(Populacao_Total),
    highlight = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 1,
      bringToFront = TRUE),
    layerId = ~CD_GEOCUF
  )%>%
  addLegend(
    position = "bottomleft",
    opacity = 1,
    title= "Distribuição da População por UF",
    pal = paleta_regioes_pesquisas,
    values = levels(mapa@data$Populacao_Total)
  )->mapa_1

dplyr::glimpse(mapa@data)



class(mapa_1)

View(mapa)
