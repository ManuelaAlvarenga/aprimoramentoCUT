## Instalar o pacote "rwts" e suas dependências a partir do github
# OBS: requer o uso do pacote devtools ou remotes
# devtools::install_github("e-sensing/sits", dependencies = TRUE)
# ou
# remotes::install_github("e-sensing/sits", dependencies = TRUE)
# preferencialmente use o pacote "sits" na versão disponibilizada no CRAN

#Carregar os pacotes
library(sf)
library(dplyr)
library(rwlts)
library(sits)

#Configura diretório de trabalho local
setwd("/home/fred/Documentos/IBGE/Cobertura e Uso da Terra/") # Diretório deve ser mudado para cada máquina

#Configurar o ambiente com as credenciais para acessar o servidor do Brazil Data Cube (acess token)
chaveBDC <-read.table("Token BDC") #carrega chave pessoal, salvada em arquivo de texto
Sys.setenv("BDC_ACCESS_KEY" = chaveBDC[1, 1])

# Configurando o sf para lidar com coordenadas planas
### Para os comando usados, ACHO que não faz diferença. Vignette enfatiza efeito em operações (https://cran.r-project.org/web/packages/sf/vignettes/sf7.html)
# Já que representa a saída de um "default", entendo ser bom ter certeza que é interessante e necessário (EU não tenho fundamentação teórica para julgar isso).
sf::sf_use_s2(F)

#Carregando shapefile com os centroides MCUT Brasil 1km²
centroides_shp <- sf::st_read("~/R/centroides_ret_envolvente_intersec_BR", quiet = TRUE) 
centroides_shp <- sf::st_read("vetoriais/centroides_ret_envolvente_intersec_BR.shp", quiet = TRUE) #SUGIRO mudar o diretório para "vetoriais" ou algo assim. Acho pouco informativo deixar como "R" uma pasta dentro de um projeto do R

centroides_shp


#Filtrando as áreas estáveis
pontos_estaveis <-
  filter(
    centroides_shp,
    USO2000 == USO2010 &
      USO2010 == USO2012 &
      USO2012 == USO2014 & USO2014 == USO2016 & USO2016 == USO2018
  )
pontos_estaveis

#### Das áreas estáveis, extrair quantidades fixas (lotes de 500?) de amostras aleatórias sem reposição.
# pontos_estaveis500<-pontos_estaveis[sample(x=1:nrow(pontos_estaveis),size=500),]
# pontos_estaveis1000<-pontos_estaveis[sample(x=1:nrow(pontos_estaveis),size=1000),]
# pontos_estaveis1500<-pontos_estaveis[sample(x=1:nrow(pontos_estaveis),size=1500),]
# PS: acho que a solução do loop apresentada a seguir será melhor para não ter que repetir código dos passos seguintes para cada tamanho de amostra

#### A ideia é buscar as trajetórias e depois rodar o SITS por partes, para não sobrecarregar, avaliar se é necessário.
## [Fred]: mas para saber o efeito de quantidades diferentes de pontos para treinamento do classificador, precisaria de empregar todas as 
# hipóteses de tamanho de amostra (ex. 500, 1000, 1500...) simultaneamente. Aí seriam avaliadas, como variáveis resposta, os indicadores de 
# acurácia (tabela de confusão) e custo em termos de tempo de processamento e/ou memória RAM necessária.
# 
#### Método, a partir da função sample()?


#Chamando o serviço R_wlts
wlts_bdc <- "https://brazildatacube.dpi.inpe.br/wlts/"

#Listando a coleções disponíveis
rwlts::list_collections(wlts_bdc)

n_amostras<-c(500, 1000, 1500)

for (i in n_amostras){

  #Extraindo a lat_long do shapefile
lat_long <- as.data.frame(st_coordinates(pontos_estaveis[sample(x=1:nrow(pontos_estaveis),size=i),]))
print(paste("amostragem:",i))
print(head(lat_long))


#Pegar o valor das classe do IBGE para o ano de 2018

system.time({
  amostra_estaveis_ibge <- get_trajectory(
    wlts_bdc,
    latitude = lat_long$Y,
    longitude = lat_long$X,
    collections = "ibge_cobertura_uso_terra"
  )
  
  amostra_ibge_2018 <-
    filter(amostra_estaveis_ibge$result, date == 2018)
}) #considere o valor "decorrido" dado em segundos

if(i==500){amostra_ibge_2018_n500<<-amostra_ibge_2018}
if(i==1000){amostra_ibge_2018_n1000<<-amostra_ibge_2018}
if(i==1500){amostra_ibge_2018_n1500<<-amostra_ibge_2018}
}
##################
######### Seção a seguir seria descontinuada se optar por avaliar concordância levando em consideração mais pixels 
#         dos produtos MapBiomas e TerraClass para cada pixel IBGE
#         Nesse contexto, o objeto amostra_concordancia.df seria mais facilmente produzido sem usar o get_trajectory e sim 
#         comandos genéricos do pacote sf
############################################

# #Amostra Mapbiomasv. 6
# system.time({
#   amostra_mapbiomas <- get_trajectory(
#     wlts_bdc,
#     latitude = lat_long$Y,
#     longitude = lat_long$X,
#     collections = "mapbiomas-v6"
#     )
#   
#   amostra_mapbiomas_2018 <-filter(amostra_mapbiomas$result, date == 2018)
# })
# 
# head(amostra_mapbiomas_2018)
# 
# #Amostra TerraClass Cerrado
# system.time({
#   amostra_terraclass <- get_trajectory(
#     wlts_bdc,
#     latitude = lat_long$Y,
#     longitude = lat_long$X,
#     collections = "terraclass_cerrado"
#   )
#   
#   amostra_terraclass_2018 <-filter(amostra_terraclass$result, date == 2018)
# }) 
# 
# head(amostra_terraclass_2018)
# 
# #Olhar as amostras juntas em um mesmo dataframe
# 
# ####OBS: fiz sucessivos merges, pois a amostra do MapBiomas em alguns casos apresentou quantidade menor de feições e com isso não consegui unificar o dataframe de uma só vez
# 
# merge <-merge(amostra_ibge_2018, amostra_mapbiomas_2018, by = "point_id")
# View(merge)
# merge2 <- merge(merge, amostra_terraclass_2018, by = "point_id")
# View(merge2)
# 
# lat_long_id <- dplyr::mutate(lat_long, point_id = row_number())
# dim(lat_long_id)
# 
# merge3 <- merge(merge2, lat_long_id, by = "point_id")
# head(merge3)
# 
# amostra_concordancia.df <-
#   data.frame(
#     point_id = merge3$point_id,
#     latitude = merge3$Y,
#     longitude = merge3$X,
#     ibge_2018 = merge3$class.x,
#     mapbiomas_2018 = merge3$class.y,
#     terraclass_2018 = merge3$class
#   )
# View(amostra_concordancia.df)


####Criar regras para "tradução" de legenda e criar campo novo "label"
#### Há uma legenda Nível 3 para o mapeamento, mas sua "tradução" deve ser feita a partir do conjunto das combinações para aumentar a compatibilidade.
#### Há uma tabela de domínio das combinações em excel. Importar ela e buscar correspondências no dataframe gerado? Como fazer?

## Exemplo de como implementar isso extraído do código de outro script meu (variáveis não farão sentido aqui, reproduzido só para pensar
#  em uma das estratégias possíveis)

#contabiliza correspondências. ATENÇÃO: seleção de classes "sinônimas" abaixo é informal, só para teste do script
# ibge_amostra$mb.concor<-NA
# ibge_amostra$tc.concor<-NA
# for (i in 1:nrow(ibge_amostra)) {
#   if(ibge_amostra$USO2018[i]==1){
#     ibge_amostra$mb.concor[i]<-sum(lista_por_amostra[[i]]$mb%in%c(24,30))/sum(!is.na(lista_por_amostra[[i]]$mb))
#     ibge_amostra$tc.concor[i]<-sum(lista_por_amostra[[i]]$tc%in%c(16,17,19,51,52))/sum(!is.na(lista_por_amostra[[i]]$tc))
#   }
#   if(ibge_amostra$USO2018[i]==2){
#     ibge_amostra$mb.concor[i]<-sum(lista_por_amostra[[i]]$mb%in%c(18,19,39,20,40,41,36,46,47,48))/sum(!is.na(lista_por_amostra[[i]]$mb))
#     ibge_amostra$tc.concor[i]<-sum(lista_por_amostra[[i]]$tc%in%c(12,13,14,15))/sum(!is.na(lista_por_amostra[[i]]$tc))
#   }
#   if(ibge_amostra$USO2018[i]==3){
#     ibge_amostra$mb.concor[i]<-sum(lista_por_amostra[[i]]$mb%in%c(15))/sum(!is.na(lista_por_amostra[[i]]$mb))
#     ibge_amostra$tc.concor[i]<-sum(lista_por_amostra[[i]]$tc%in%c(10))/sum(!is.na(lista_por_amostra[[i]]$tc))
#   }
#   if(ibge_amostra$USO2018[i]==4){
#     ibge_amostra$mb.concor[i]<-0
#     ibge_amostra$tc.concor[i]<-0
#   }
#   if(ibge_amostra$USO2018[i]==5){
#     ibge_amostra$mb.concor[i]<-sum(lista_por_amostra[[i]]$mb%in%c(9))/sum(!is.na(lista_por_amostra[[i]]$mb))
#     ibge_amostra$tc.concor[i]<-sum(lista_por_amostra[[i]]$tc%in%c(9))/sum(!is.na(lista_por_amostra[[i]]$tc))
#   }
#   if(ibge_amostra$USO2018[i]==6){
#     ibge_amostra$mb.concor[i]<-sum(lista_por_amostra[[i]]$mb%in%c(1,3))/sum(!is.na(lista_por_amostra[[i]]$mb))
#     ibge_amostra$tc.concor[i]<-sum(lista_por_amostra[[i]]$tc%in%c(1,2))/sum(!is.na(lista_por_amostra[[i]]$tc))
#   }
#   if(ibge_amostra$USO2018[i]==9){
#     ibge_amostra$mb.concor[i]<-sum(lista_por_amostra[[i]]$mb%in%c(11))/sum(!is.na(lista_por_amostra[[i]]$mb))
#     ibge_amostra$tc.concor[i]<-0
#   }
#   if(ibge_amostra$USO2018[i]==10){
#     ibge_amostra$mb.concor[i]<-sum(lista_por_amostra[[i]]$mb%in%c(12))/sum(!is.na(lista_por_amostra[[i]]$mb))
#     ibge_amostra$tc.concor[i]<-0
#   }
#   if(ibge_amostra$USO2018[i]==11){
#     ibge_amostra$mb.concor[i]<-0
#     ibge_amostra$tc.concor[i]<-0
#   }
#   if(ibge_amostra$USO2018[i]==12){
#     ibge_amostra$mb.concor[i]<-sum(lista_por_amostra[[i]]$mb%in%c(31,33))/sum(!is.na(lista_por_amostra[[i]]$mb)) #generalização
#     ibge_amostra$tc.concor[i]<-sum(lista_por_amostra[[i]]$tc%in%c(20))/sum(!is.na(lista_por_amostra[[i]]$tc))
#   }
#   if(ibge_amostra$USO2018[i]==13){
#     ibge_amostra$mb.concor[i]<-sum(lista_por_amostra[[i]]$mb%in%c(31,33))/sum(!is.na(lista_por_amostra[[i]]$mb)) #generalização
#     ibge_amostra$tc.concor[i]<-sum(lista_por_amostra[[i]]$tc%in%c(20))/sum(!is.na(lista_por_amostra[[i]]$tc))
#   }
#   if(ibge_amostra$USO2018[i]==14){
#     ibge_amostra$mb.concor[i]<-sum(lista_por_amostra[[i]]$mb%in%c(25))/sum(!is.na(lista_por_amostra[[i]]$mb))
#     ibge_amostra$tc.concor[i]<-sum(lista_por_amostra[[i]]$tc%in%c(7,8))/sum(!is.na(lista_por_amostra[[i]]$tc))
#   }
# }
# 


#Exportar dataframe para csv
# write.csv(amostra_concordancia10.df,"~/R/amostra_concordancia.csv",fileEncoding = "utf-8",row.names = FALSE)


## Referências:

# Rolf Simoes, Gilberto Camara, Felipe Souza, Pedro Andrade, Lorena Santos, Karine Ferreira, Gilberto
#Queiroz, Alexandre Carvalho, Victor Maus (2021), SITS: Data Analysis and Machine Learning using
#Satellite Image Time Series. URL https://github.com/e-sensing/sits.
citation("sits")
