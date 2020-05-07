
#install.packages(c("sf"))
library(sf)
library(tidyverse)


## Bancos de dados soja heterogenea
# Importa o bdados inteiro, mas ja com o fator multipl p/ grupos heterog aplicados
sojah_2018 <- read.csv("SOJAH_2018.csv", sep = ";")
sojah_2017 <- read.csv("SOJAH_2017.csv", sep = ";")
sojah_2019 <- read.csv("SOJAH_2019.csv", sep = ";")

str(sojah_2018)
names(sojah_2018)


# resumo estatistico e df com valores referencia

sojah_2017 %>% # 2017
    summarise(mean_pr = mean(Rendimentoseco),
              sd_pr = sd(Rendimentoseco),
              med_pr = median(Rendimentoseco))

summ_sojah_2017 <- data.frame(mean_pr = 3895,
                              sd_pr = 471,
                              med_pr = 3986)


sojah_2018 %>% # 2018
    summarise(mean_pr = mean(Rendimentoseco),
              sd_pr = sd(Rendimentoseco),
              med_pr = median(Rendimentoseco))

summ_sojah_2018 <- data.frame(mean_pr = 4093,
                              sd_pr = 452,
                              med_pr = 4156)


sojah_2019 %>% # 2019
    summarise(mean_pr = mean(Rendimentoseco),
              sd_pr = sd(Rendimentoseco),
              med_pr = median(Rendimentoseco))

summ_sojah_2019 <- data.frame(mean_pr = 4274,
                              sd_pr = 527,
                              med_pr = 4364)




## DF simples, uma coluna so, com a produtiv original, sem fator aplicado
# será usada para criar os grupos (inverso do q foi feito no excel anteriormente..)

sojah_base_rs <- read.csv("sojah_base_rs.csv")
head(sojah_base_rs)
str(sojah_base_rs)



# Usa os dfs para determinar os grupos:
# Obs: esses grupos eu que criei artificialmente para ter "zonas" heterogeneas bem visiveis nos mapas
# talvez nem seja necessario criar essa coluna
#sojah_2018 <-
#    sojah_2018 %>%
#    mutate(grupos = round(Rendimentoseco / sojah_base_rs$Rendimentoseco, digits=3))
#
#

edit(sojah_2017)


# Essa coluna de grupo sim, faz mais sentido:
# correto seria usar parametros estatisticos e criar grupos a partir disso
# ex: 1.5 sd * media num ifelse: qual a melhor medida de desv padrao?
# qr = quantis
# pr_cm = preço de comercialização, supondo 74 R$/sc ou 1.23 R$/kg


sojah_2017 <-
    sojah_2017 %>%
    mutate(gr = ifelse(Rendimentoseco < 1 * summ_sojah_2017$sd_pr + summ_sojah_2017$mean_pr, "inf",
                       ifelse(Rendimentoseco > 1 * summ_sojah_2017$sd_pr + summ_sojah_2017$mean_pr, "sup", "error")),
           qr = ntile(Rendimentoseco, 4),
           pr_cm = 1.23 * Rendimentoseco)


sojah_2018 <-
    sojah_2018 %>%
    mutate(gr = ifelse(Rendimentoseco < 1 * summ_sojah_2018$sd_pr + summ_sojah_2018$mean_pr, "inf",
                       ifelse(Rendimentoseco > 1 * summ_sojah_2018$sd_pr + summ_sojah_2018$mean_pr, "sup", "error")),
           qr = ntile(Rendimentoseco, 4),
           pr_cm = 1.23 * Rendimentoseco)

sojah_2019 <-
    sojah_2019 %>%
    mutate(gr = ifelse(Rendimentoseco < 1 * summ_sojah_2019$sd_pr + summ_sojah_2019$mean_pr, "inf",
                       ifelse(Rendimentoseco > 1 * summ_sojah_2019$sd_pr + summ_sojah_2019$mean_pr, "sup", "error")),
           qr = ntile(Rendimentoseco, 4),
           pr_cm = 1.23 * Rendimentoseco)


#-----------
## Espacial:
# indica quais as col geom
sojah_2018_sf <- st_as_sf(sojah_2018, coords=c("Longitude", "Latitude"))
sojah_2017_sf <- st_as_sf(sojah_2017, coords=c("Longitude", "Latitude"))
sojah_2019_sf <- st_as_sf(sojah_2019, coords=c("Longitude", "Latitude"))

head(sojah_2018_sf)

# acerta SRC
st_crs(sojah_2018_sf) <- 4618
st_crs(sojah_2017_sf) <- 4618
st_crs(sojah_2019_sf) <- 4618


#-------------------------
## Análise: Plotar dados
# plota espacial e não espacial
plot(sojah_2018_sf)
plot(sojah_2018_sf$Rendimentoseco) # plota nao espacial pq é vetor numerico: interessante mostrar?


# plota espacial, apenas o indicado
plot(sojah_2018_sf["Rendimentoseco"])


# plota apenas as colunas geometricas
plot(st_geometry(sojah_2018_sf))


# ggplot lida com sf tbm: geom_sf
ggplot(sojah_2018_sf)+
    geom_sf(aes(fill=Rendimentoseco))

names(sojah_2018_sf)

## Raster

# Importar e descobrir tamanho dos pixels

library(raster)
library(rgdal)

soja_rst <- raster("C:\\Users\\carol\\Desktop\\FAZ_MORENA\\PORTFOLIO\\QGIS\\milho_2017_idw.tif")

soja_rst

#---------------------
# Análises
# Apenas classificar: usar até mesmo quartis
# Mostrar homogeneidade: desvio pad e parametros
# Mostrar tendencia de aumentos/diminuição de niveis de produtivi

sh_1 <-
    sojah_2017 %>%
    mutate(index = seq(1, nrow(sojah_2017))) %>%
    ggplot()+
    geom_point(aes(x = index, y = Rendimentoseco, color=as.factor(qr)))+
    annotate(geom="text", x = 30000, y = 10, label = "2017")+
    scale_colour_viridis_d()+
    geom_hline(yintercept = summ_sojah_2017$mean_pr, color="red")+
    xlab("Pontos amostrais")+
    ylab("Produtividade soja (kg/ha)")+
    scale_y_continuous(limits=c(0,6000), breaks=seq(0,6000,by=500))+
    theme_bw()+
    guides(color=FALSE)



sh_2 <-
    sojah_2018 %>%
    mutate(index = seq(1, nrow(sojah_2018))) %>%
    ggplot()+
    geom_point(aes(x = index, y = Rendimentoseco, color=as.factor(qr)))+
    annotate(geom="text", x = 30000, y = 10, label = "2018")+
    scale_colour_viridis_d()+
    geom_hline(yintercept = summ_sojah_2018$mean_pr, color="red")+
    xlab("Pontos amostrais")+
    ylab("Produtividade soja (kg/ha)")+
    scale_y_continuous(limits=c(0,6000), breaks=seq(0,6000,by=500))+
    theme_bw()+
    guides(color=FALSE)+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank())

sh_3 <-
    sojah_2019 %>%
    mutate(index = seq(1, nrow(sojah_2019))) %>%
    ggplot()+
    geom_point(aes(x = index, y = Rendimentoseco, color=as.factor(qr)))+
    annotate(geom="text", x = 30000, y = 10, label = "2019")+
    scale_colour_viridis_d()+
    geom_hline(yintercept = summ_sojah_2019$mean_pr, color="red")+
    xlab("Pontos amostrais")+
    ylab("Produtividade soja (kg/ha)")+
    scale_y_continuous(limits=c(0,6000), breaks=seq(0,6000,by=500))+
    theme_bw()+
    guides(color=guide_legend(title="Quartis"))+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank())



library(cowplot)

# indica visualmente que o patamar das produtividades aumentou
plot_grid(sh_1, sh_2, sh_3, ncol = 3)


#------------------------

# checar a média geral de preço de comercializ
sojah_2017 %>%
    summarise(mean(pr_cm))

sojah_2018 %>%
    summarise(mean(pr_cm))

sojah_2019 %>%
    summarise(mean(pr_cm))

# fazer o df com os preços de cd quartil e ja adicionar a media geral
res_com_s2017 <-
    sojah_2017 %>%
    group_by(qr) %>%
    summarise(vm = mean(pr_cm)) %>%
    add_row(qr = "geral", vm = 4791.1)

res_com_s2018 <-
    sojah_2018 %>%
    group_by(qr) %>%
    summarise(vm = mean(pr_cm)) %>%
    add_row(qr = "geral", vm = 5035.256)

res_com_s2019 <-
    sojah_2019 %>%
    group_by(qr) %>%
    summarise(vm = mean(pr_cm)) %>%
    add_row(qr = "geral", vm = 5255.439)


# plotar

s_va <-
    ggplot(res_com_s2017)+
    geom_col(aes(x = qr, y = vm, fill = qr),
             width = 0.4, color = "black", alpha = 0.7)+
    annotate("text", x = "1", y = 5500, label="2017")+
    coord_flip()+
    scale_fill_viridis_d()+
    theme_bw()+
    scale_y_continuous(limits=c(0,6000), breaks=seq(0,6000,by=200))+
    xlab("Grupos") + ylab("Valor médio (R$/ha)")+
    guides(fill=guide_legend(title=NULL))+
    theme(axis.text.x = element_text(face = "bold", color = "black",
                                     size = 12, angle = 45))


s_vb <-
    ggplot(res_com_s2018)+
    geom_col(aes(x = qr, y = vm, fill = qr),
             width = 0.4, color = "black", alpha = 0.7)+
    annotate("text", x = "1", y = 5500, label="2018")+
    coord_flip()+
    scale_fill_viridis_d()+
    theme_bw()+
    scale_y_continuous(limits=c(0,6000), breaks=seq(0,6000,by=200))+
    xlab("Grupos") + ylab("Valor médio (R$/ha)")+
    guides(fill=guide_legend(title=NULL))+
    theme(axis.text.x = element_text(face = "bold", color = "black",
                                     size = 12, angle = 45))


s_vc <-
    ggplot(res_com_s2019)+
    geom_col(aes(x = qr, y = vm, fill = qr),
             width = 0.4, color = "black", alpha = 0.7)+
    annotate("text", x = "1", y = 5500, label="2019")+
    coord_flip()+
    scale_fill_viridis_d()+
    theme_bw()+
    scale_y_continuous(limits=c(0,6000), breaks=seq(0,6000,by=200))+
    xlab("Grupos") + ylab("Valor médio (R$/ha)")+
    guides(fill=guide_legend(title=NULL))+
    theme(axis.text.x = element_text(face = "bold", color = "black",
                                     size = 12, angle = 45))


plot_grid(s_va, s_vb, s_vc, ncol=1)

# unindo os 3 dfs, para uma comparação entre anos mais eficiente para cada grupo

s_valores <- bind_rows(res_com_s2017, res_com_s2018)

s_valores <- bind_rows(s_valores, res_com_s2019)

s_valores <-
    s_valores %>%
    mutate(id = rep(c("2017", "2018", "2019"), each=5))


edit(s_valores)

s_valores_plot <-
    ggplot(s_valores)+
    geom_col(aes(x = qr, y = vm, fill = qr),
             width = 0.6, color = "black", alpha = 0.7)+
    facet_wrap(.~id)+
    #coord_flip()+
    scale_fill_viridis_d()+
    theme_bw()+
    scale_y_continuous(limits=c(0,6000), breaks=seq(0,6000,by=400))+
    xlab("Grupos") + ylab("Valor médio (R$/ha)")+
    guides(fill=guide_legend(title=NULL))+
    ggtitle("Evolução do rendimento médio de comercialização",
            subtitle = "De 2017 para 2019 houve um aumento de 9.6% na média total")+
    theme(axis.text.x = element_text(face = "bold", color = "black",
                                     size = 10, angle = 45))




#---------------------
# parametros
#--------------------

# de 2017 p/ 2019:
# SD aumentou 12%
# Produtividades aumentaram 10%

sojah_2017 %>%
    group_by(qr) %>%
    summarise(mn = mean(Rendimentoseco), dp = sd(Rendimentoseco))

sojah_2018 %>%
    group_by(qr) %>%
    summarise(mn = mean(Rendimentoseco), dp = sd(Rendimentoseco))

sojah_2019 %>%
    group_by(qr) %>%
    summarise(mn = mean(Rendimentoseco), dp = sd(Rendimentoseco))
