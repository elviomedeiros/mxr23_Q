# MXR23 #####################################################################
dev.off() #apaga os graficos, se houver algum
rm(list=ls(all=TRUE)) #limpa a memória
cat("\014") #limpa o console
#shell.exec(getwd())
getwd()
setwd("D:/Elvio/OneDrive/MSS/_rebio23-mxr/mxr23_R")
library(openxlsx)
mxr23 <- read.xlsx("D:/Elvio/OneDrive/MSS/_rebio23-mxr/mxr23_Q/data/rebio23-peixes_5-10.xlsx",
                   rowNames = T,
                   colNames = T,
                   sheet = "peixes-fisio")
mxr23#[1:5,1:5] mostra apenas as linhas e colunas de 1 a 5.

data <- mxr23
data <- mxr23[!row.names(mxr23) %in% c("4-9-85", #NA
                                       "1-10-7"),] #As.bim grande

data[c("1-10-4","1-10-5"), c("Rodamina_rfu_mg", "TH_musculo_p",
                             "TH_branquia_p", "Proteinas_Totais_mg"
                             )] <- NA

#RESUMO DOS DADOS ###########################################################

library(dplyr)
library(tidyr)
grouped_data <- group_by(data, Especie, Amostra_tipo)
summarised_data <- summarise(grouped_data, count = n())
species_count_table <- spread(summarised_data, Amostra_tipo, count, fill = 0)
print(species_count_table)

species_count_table <- species_count_table %>%
  rowwise() %>%
  mutate(N = sum(c_across(where(is.numeric))))

resumo <- as.data.frame(species_count_table)
resumo

rownames(resumo) <- resumo[,1] #tem  que ser um df
resumo[,1] <- NULL

soma <- apply(resumo,2,sum)
soma

soma_row <- as.data.frame(t(soma))
rownames(soma_row) <- "Soma"

resumo <- rbind(resumo, soma_row)
resumo

#############################################################################

resumo2 <- data %>%
  group_by(Site = Site, Especie = Especie, Amostra_tipo = Amostra_tipo) %>%
  summarise(Count = n()) %>%
  arrange(Site, Especie, Amostra_tipo)

resumo2 <- as.data.frame(resumo2)
resumo2

write.table(resumo2, file = "resumo.csv", row.names = T, sep = "\t")
read.table("resumo.csv", check.names = F)

resumo2_wide <- resumo2 %>%
  pivot_wider(names_from = c(Site, Amostra_tipo), values_from = Count, values_fill = list(Count = 0))
resumo2_wide <- as.data.frame(resumo2_wide)
resumo2_wide

# Create the gt table
library(gt)
gt_table <- resumo2_wide %>%
  gt() %>%
  tab_header(
    title = "Species Count by Site and Sample Type"
  ) %>%
  cols_label(
    Especie = "Species"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 0
  ) %>%
  cols_width(
    everything() ~ px(100)
  ) %>%
  opt_table_outline()

# Print the gt table
print(gt_table)

# GRÁFICO POR N ########################################################

# Filter out Amostra_tipo "xx"
filtered_Amostra_tipo <- data %>%
  filter(Amostra_tipo != "xx")
data <- filtered_Amostra_tipo

# Count occurrences of each species by Amostra_tipo and Site
species_count <- data %>%
  group_by(Site, Especie, Amostra_tipo) %>%
  summarise(Count = n(), .groups = 'drop')
# Plot the counts
library(ggplot2)
ggplot(species_count, aes(x = Amostra_tipo, y = Count, fill = Especie)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Site) +
  labs(title = "Count of Each Species per Amostra_tipo for Each Site",
       x = "Amostra Tipo",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Count occurrences of each species by Amostra_tipo and Site
species_count <- data %>%
  group_by(Site, Especie, Amostra_tipo) %>%
  summarise(Count = n(), .groups = 'drop')
# Plot the counts with species on the x-axis
ggplot(species_count, aes(x = Especie, y = Count, fill = Amostra_tipo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Site) +
  labs(title = "Count of Each Species per Amostra_tipo for Each Site",
       x = "Species",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# GRAFICO POR ANÁLISE ########################################################

#N #data <- data %>% mutate(N = 1)
#RPC
#Rodamina_rfu_mg
#TH_musculo_p
#TH_branquias_p
#Proteinas_Totais_mg

# Convert the column to numeric, replace commas with dots for proper conversion
data$Proteinas_Totais_mg <- as.numeric(gsub(",", ".", data$Proteinas_Totais_mg))
VAR <- deparse(substitute(data$Proteinas_Totais_mg))
var <- "Proteinas_Totais_mg" %>% rlang::sym()
str(data)

# Calculate the average for each combination of species, Amostra_tipo, and Site
average <- data %>%
  group_by(Site, Especie, Amostra_tipo) %>%
  summarise(Average = mean(!!var, na.rm = TRUE), .groups = 'drop')
# Plot the average RPC
ggplot(average, aes(x = Especie, y = Average, fill = Amostra_tipo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Site) +
  labs(title = paste("Average POR ANÁLISE para", VAR, "per Species and Amostra_tipo for Each Site"),
       x = "Species",
       y = paste("Average", VAR)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# GRÁFICO POR ESPÉCIE ########################################################

# Filter out the species Astyanax bimaculatus and Astyanax fasciatus

filtered_data <- data

filtered_data <- data %>%
  filter(!Especie %in% c("Astyanax bimaculatus",
                        "Astyanax fasciatus",
                        "Characidium bimaculatum")) #EXCLUI USANDO O "!"
filtered_data <- data %>%
  filter(Especie %in% c("Hemigrammus unilineatus")) #ESCOLHE

# Calculate the average for each combination of Amostra_tipo, Especie, and Site
average <- filtered_data %>%
  group_by(Site, Amostra_tipo, Especie) %>%
  summarise(Average = mean(!!var, na.rm = TRUE), .groups = 'drop')
# Plot the average RPC with Amostra_tipo on the x-axis
ggplot(average, aes(x = Amostra_tipo, y = Average, fill = Especie)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Site) +
  labs(title = paste("Average", VAR, "per Amostra_tipo for Each Site"),
       x = "Amostra Tipo",
       y = paste("Average", VAR)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# CORRELAÇÕES ##########################################################

data <- filtered_data

colnames(data)
cor <- data %>%
  select(Ponto, Ref_site, Compr.total_mm, Compr.padrao_mm, Altura_mm,
  Peso_g, RPC, Rodamina_rfu_mg, TH_branquia_p, TH_musculo_p,
  Proteinas_Totais_mg)
str(cor)
cor[] <- lapply(cor, function(x) as.numeric(gsub(",", ".", x)))

plot(cor)
plot(cor[,3:7])

cor(cor,method="pearson")
cor(cor[,1:3], method="spearman")

library(psych)
pairs.panels(cor[,8:11],
             method = "pearson", # correlation method
             scale = FALSE, lm = FALSE,
             hist.col = "#00AFBB", pch = 19,
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             alpha = 0.5
)

cor %>%
  with(cor.test(RPC, Peso_g, method = "pearson"))

with(cor, plot(scale(RPC), scale(Peso_g)))
with(cor, plot(RPC, Peso_g))

plot((m_part$"Overh_veg" - mean(m_part$"Overh_veg")) / sd(m_part$"Overh_veg"))
#plot((m_trns$"m.elev" - mean(m_trns$"m.elev")) / sd(m_trns$"m.elev"))
with(cor, plot((RPC - mean(Peso_g)) / sd(Peso_g)))
with(cor, plot((RPC - mean(RPC)) / sd(RPC),
                  (Peso_g - mean(Peso_g)) / sd(Peso_g)))

# FIM #########################################################################

