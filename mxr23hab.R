#MXR23 - ORGANIZANDO DADOS----
#####....----

dev.off() #apaga os graficos, se houver algum
rm(list=ls(all=TRUE)) #limpa a memória
cat("\014") #limpa o console
#shell.exec(getwd())
getwd()
setwd("D:/Elvio/OneDrive/MSS/_Bentos-2006/Bentos2006_Q")
library(openxlsx)

##CARREGANDO MATRIZES BRUTAS----

habitat <- read.xlsx("D:/Elvio/OneDrive/MSS/_rebio23-mxr/mxr23_Q/data/rebio23-habitat.xlsx",
                     rowNames = T,
                     colNames = T,
                     sheet = "ambiente",
                     rows = 2:20)
habitat[1:5,1:5] #[1:5,1:5] mostra apenas as linhas e colunas de 1 a 5.
habitat <- habitat[, colnames(habitat) != "w.DO_%" &
                   colnames(habitat) != "w.Sechi_cm"]
habitat

t_grps <- read.xlsx("D:/Elvio/OneDrive/MSS/_rebio23-mxr/mxr23_Q/data/rebio23-habitat.xlsx",
                     rowNames = T,
                     colNames = T,
                     sheet = "grupos",
                     rows = 2:20)
t_grps


##SALVANDO MATRIZES FINAIS----

write.table(habitat, "m_hab.csv",
            sep = ";", dec = ".", #"\t",
            row.names = TRUE,
            quote = TRUE,
            append = FALSE)
write.table(t_grps, "t_grps.csv",
            sep = ";", dec = ".", #"\t",
            row.names = TRUE,
            quote = TRUE,
            append = FALSE)
t_grps <- read.csv("t_grps.csv",
                   sep = ";", dec = ".",
                   row.names = 1,
                   header = TRUE,
                   na.strings = NA)
m_hab <- read.csv("m_hab.csv",
                   sep = ";", dec = ".",
                   row.names = 1,
                   header = TRUE,
                   na.strings = NA)

#MXR23----
####----

##ORGANIZANDO DADOS----

dev.off()
rm(list=ls(all=TRUE))
cat("\014")
t_grps <- read.csv("t_grps.csv",
                   sep = ";", dec = ".",
                   row.names = 1,
                   header = TRUE,
                   na.strings = NA)
m_hab <- read.csv("m_hab.csv",
                  sep = ";", dec = ".",
                  row.names = 1,
                  header = TRUE,
                  na.strings = NA)

##CORRELOGRAMA E REMOÇÃO DE VARIÁVEIS REDUNDANTES OU DESNECESSÁRIAS----

library(psych)
colnames(m_hab)

png("fig-m.hab_pairs.png")
pairs.panels(m_hab[,13:19],
             method = "pearson", # correlation method
             scale = FALSE, lm = FALSE,
             hist.col = "#00AFBB", pch = 19,
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             alpha = 0.5)
dev.off()

cor <- cor(m_hab)
cor

library(corrplot)
png("fig-hab_corrplot.png")
corrplot(cor, method = "circle")
dev.off()

#### IMPRESSÃO EM PAPEL
#win.print()
#corrplot(cor, method = "circle")
#dev.off()

##DELETANDO COLINEARES----

sink(file = "colineares.txt", append = F, split = T)
colnames(m_hab)
del_cols <- c() #"g.river_length","g.altitude" #NÃO DELETEI VARIÁVEIS
m_hab_part <- m_hab[, !(colnames(m_hab) %in% del_cols)]

##SOMANDO REDUNDANTES----

m_hab_part$s.gravel <- m_hab_part$s.smlgrav + m_hab_part$s.lrggrav + m_hab_part$s.cobbles
m_hab_part <- m_hab_part[, !(colnames(m_hab_part)
                             %in% c("s.smlgrav", "s.lrggrav", "s.cobbles"))]
m_hab_part$s.rock <- m_hab_part$s.rocks + m_hab_part$s.bedrock
m_hab_part <- m_hab_part[, !(colnames(m_hab_part)
                             %in% c("s.rocks", "s.bedrock"))]
m_hab_part$h.algae <- m_hab_part$h.filalgae + m_hab_part$h.attalgae
m_hab_part <- m_hab_part[, !(colnames(m_hab_part)
                             %in% c("h.filalgae", "h.attalgae"))]
m_hab_part$h.debris <- m_hab_part$h.smldeb + m_hab_part$h.lrgdeb
m_hab_part <- m_hab_part[, !(colnames(m_hab_part)
                             %in% c("h.smldeb", "h.lrgdeb"))]

colnames(m_hab_part)
m_hab_part
sink()

write.table(m_hab_part, "m_hab_part.csv",
            sep = ";", dec = ".", #"\t",
            row.names = TRUE,
            quote = TRUE,
            append = FALSE)
m_hab_part <- read.csv("m_hab_part.csv",
                       sep = ";", dec = ".",
                       row.names = 1,
                       header = TRUE,
                       na.strings = NA)
