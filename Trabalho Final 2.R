# Trabalho final de econometria

################### Puxando os pacotes: 

library(dplyr)
library(readxl)
library(tidyverse)
library(WDI)
library(haven)
library(mFilter)
library(countrycode)
library(papeR)
library(lmtest)
library(sandwich)

################### Importando os dados da tabela original de (colocar o nome dos autores)


Cruces_Trebesch_Haircuts_2014_update <- read_excel("~/Cruces-Trebesch-Haircuts-2014-update.xlsx", 
                                                   skip = 11)


#Algumas linhas precisam ser excluídas: 

Haircuts <- Cruces_Trebesch_Haircuts_2014_update[-c(1,189,190,191),]

# Renomeando algumas variáveis para ficar mais fácil

Haircuts <-  rename(Haircuts, HM =`Market Haircut        HM`, FVR = `Face Value Reduction (in %)`, HSZ = `Preferred Haircut HSZ`, DebtRestructured = `Debt Restructured   (m US$)`, DiscountRate = `Underlying Discount             Rate`)

#Alterando algumas variáveis para numéricas: 

Haircuts$DebtRestructured <- as.numeric(Haircuts$DebtRestructured)
Haircuts$DiscountRate <- as.numeric(Haircuts$DiscountRate)
Haircuts$HSZ <- as.numeric(Haircuts$HSZ)
Haircuts$HM <- as.numeric(Haircuts$HM)
Haircuts$FVR <- as.numeric(Haircuts$FVR)

# Removendo mês e dia da data

Haircuts$Date <- substr(Haircuts$Date,1,4)

# Achando as Razões para achar o prazo de pagamento para as dívidas: 

Haircuts$pvnfvn <- (1-Haircuts$HM )/(1-Haircuts$FVR)
Haircuts$pvofvo <- (1-Haircuts$HM )/(1-Haircuts$HSZ)

# Problema: raizes das equações

Haircuts$N_new <- 0
for (i in 1:187) {
  if(!is.na(Haircuts$DiscountRate[i])){
  Haircuts$N_new[i] <- uniroot(function(x){(1/(x*Haircuts$DiscountRate[i]))*(1-1/(1+ Haircuts$DiscountRate[i])^x) - 
      Haircuts$pvnfvn[i]},c(-100,100))$root
  } else (Haircuts$N_new[i] <- NA)
  }

# Em alguns casos a raiz é negativa, então trocamos esses valores para 0

Haircuts$N_new[Haircuts$N_new < 0] <- 0

# Baixando tabela com o início dos defaults

DEFAULT_DATABASE <- read_excel("~/Asonuma_Trebesch_DEFAULT_DATABASE.xlsx", 
                                                sheet = "DATASET Defaults & Restruct.", 
                                                skip = 5)

DEFAULT_DATABASE$`Start of default or restructuring process: default or announcement` <- substr(DEFAULT_DATABASE$`Start of default or restructuring process: default or announcement`,1,4)
DEFAULT_DATABASE$`End of restructuring: completion of exchange` <- substr(DEFAULT_DATABASE$`End of restructuring: completion of exchange`,1,4)

DEFAULT_DATABASE <- rename(DEFAULT_DATABASE, Case = `Case nr in Cruces/Trebesch database               (2014 update)`)

DEFAULT_DATABASE$`Start of default or restructuring process: default or announcement` <- as.numeric(DEFAULT_DATABASE$`Start of default or restructuring process: default or announcement`)
DEFAULT_DATABASE$`End of restructuring: completion of exchange` <- as.numeric(DEFAULT_DATABASE$`End of restructuring: completion of exchange`)

DEFAULT_DATABASE$dur <- DEFAULT_DATABASE$`End of restructuring: completion of exchange`- DEFAULT_DATABASE$`Start of default or restructuring process: default or announcement`

DEFAULT_DATABASE2 <- data.frame(Case = DEFAULT_DATABASE$Case, dur = DEFAULT_DATABASE$dur)

# Colocando tempo em calote no meu dataframe principal: 

Haircuts <- merge(DEFAULT_DATABASE2,Haircuts, by.x = "Case")

# Achando o prazo de maturidade anterior: 

Haircuts$No <- 0
for (i in 1:184){
  if(!is.na(Haircuts$DiscountRate[i])){
  Haircuts$No[i] <- uniroot(function(x){(1/x)*(Haircuts$dur[i]+
                                                    (1/Haircuts$DiscountRate[i])*
                                                    (1-(1/(1+Haircuts$DiscountRate[i]))^(x - Haircuts$dur[i]))) - 
                                                    Haircuts$pvofvo[i]}, c(-100,100))$root
  } else (Haircuts$No[i] <- NA)
}

# o prazo antigo de maturidade é:  

Haircuts$N_old <- Haircuts$No - Haircuts$dur

Haircuts$N_old[Haircuts$N_old < 0] <- 0

# A extensão de maturidade é, então: 

Haircuts$Ext <- Haircuts$N_new - Haircuts$N_old

# Baixando os dados da WDI (iso, ano, país , GDP per capita, consumo per capita): 

wdi <- WDI(country = "all", indicator = c("NY.GDP.PCAP.KD","NE.CON.PRVT.PC.KD",
                                                 "BN.CAB.XOKA.GD.ZS","DT.DOD.DECT.GN.ZS","GC.DOD.TOTL.GD.ZS"),
                  start = 1960, end = 2020, extra = T, cache = NULL) 

wdi <- select(wdi, iso3c, year, country, NY.GDP.PCAP.KD, NE.CON.PRVT.PC.KD,
              BN.CAB.XOKA.GD.ZS, DT.DOD.DECT.GN.ZS, GC.DOD.TOTL.GD.ZS)


# Pegando somente os países que reestruturaram sua dívida: 

wdi <- filter(wdi, iso3c %in% c(unique(Haircuts$Code)))

# Colocando pela ordem dos anos: 

wdi <-  arrange(wdi, iso3c, year)

Haircuts <- arrange(Haircuts, Code, Date)

wdi <- rename(wdi, Code = iso3c, Date = year)

# Juntando os dados da wdi com a base principal: 

Haircuts <-  merge(Haircuts, wdi, by = c("Code","Date"), all.x = T)

 # puxando dados do FMI 

Global_Debt_Database <- read_dta("~/Global Debt Database.dta")
                                 

# Mudando os códigos dos países

Global_Debt_Database$ifscode <- countrycode(Global_Debt_Database$ifscode, "imf", "iso3c")

Global_Debt_Database <-  filter(Global_Debt_Database, ifscode %in% c(unique(Haircuts$Code)))

Global_Debt_Database <- rename(Global_Debt_Database, Code = ifscode, Date = year)

Haircuts <- merge(Haircuts, Global_Debt_Database, by = c("Code","Date"), all.x = T)

# passando o  filtro HP

wdi2 <- wdi

wdi2$cycle <- c(0)
wdi2$trend <- c(0)

# Filtro HP nos dados da WDI: 

for(i in unique(c(wdi2$Code))){

HP <- hpfilter(!is.na(filter(wdi2, Code == i )$NY.GDP.PCAP.KD), freq = 6,25, type = "lambda")

wdi2$cycle[wdi$Code == i] <- HP$cycle
wdi2$trend[wdi$Code == i] <- HP$trend

}

wdi2 <- select(wdi2, Code, Date, cycle, trend)

# Selecionando as variáveis mais interessantes:

Haircuts <-  select(Haircuts, Code, Date,`Country / Case`, DebtRestructured,
                    HSZ, DiscountRate, HM, FVR, Ext, NY.GDP.PCAP.KD,
                    NE.CON.PRVT.PC.KD, BN.CAB.XOKA.GD.ZS, 
                    DT.DOD.DECT.GN.ZS, GC.DOD.TOTL.GD.ZS,
                    gg, cg)

# Juntando no dataframe principal: 

Haircuts <- merge(Haircuts, wdi2, by = c("Code","Date"), all.x = T)

# Renomeando para ficar melhor: 

Haircuts <- rename(Haircuts, "GDP/L" = NY.GDP.PCAP.KD, 
                   "C/L" = NE.CON.PRVT.PC.KD,
                   "CC/GDP" = BN.CAB.XOKA.GD.ZS,
                   "ED/GDP" = DT.DOD.DECT.GN.ZS, 
                   "D/GDP" = GC.DOD.TOTL.GD.ZS)
                   
# Colocando legendas no dataframe: 

label(Haircuts$Code)              <- "ISO 3"
label(Haircuts$Date)              <- "Ano"
label(Haircuts$`Country / Case`)  <- "País"
label(Haircuts$DebtRestructured)  <- "Dívida Reestruturada em milhões US$"
label(Haircuts$HSZ)               <- "% de Corte em valor presente"
label(Haircuts$DiscountRate)      <- "Taxa de Desconto"
label(Haircuts$HM)                <- "Corte em Valor de Mercado"
label(Haircuts$FVR)               <- "% de redução de valor de Face"
label(Haircuts$Ext)               <- "Extensão da Maturidade"
label(Haircuts$`GDP/L`)           <- "PIB per capita (US$ 2010)"
label(Haircuts$`C/L`)             <- "Consumo per capita (US$ 2010)"
label(Haircuts$`CC/GDP`)          <- "Saldo de Conta Corrente % do PIB"
label(Haircuts$`ED/GDP`)          <- "Dívida Externa em % RNB"
label(Haircuts$`D/GDP`)           <- "Dívida governo central % PIB (WDI)"
label(Haircuts$gg)                <- "Dívida Governo Geral % PIB (FMI)"
label(Haircuts$cg)                <- "Dívida Governo Central % PIB (FMI)"
label(Haircuts$cycle)             <- "Ciclo"
label(Haircuts$trend)             <- "Tendência"

# Comparando as regressões dessa base com a base do artigo:
# Dados do artigo:

data_for_histograms <- read_dta("~/data_for_histograms.dta")

# Modelo com dados do código:

Haircuts$ln_cg <- log(Haircuts$cg)

modelo1 <- lm(HSZ ~ ln_cg + cycle + Ext , data = Haircuts)

coeftest(modelo1,vcov = vcovHC(modelo1, type = "HC0"))

coef1 <- as.data.frame(summary(modelo1)$coefficients)

rownames(coef1) <- c("Intercepto","ln(D/PIB)","Ciclo")
colnames(coef1) <- c("Estimativa","Desvio padrão","Estatística t","p-valor")

# Modelo com os dados do artigo: 

modelo2 <- lm(haircutsz ~ l_debtGDP + cycle,data = data_for_histograms)

coef2 <- as.data.frame(summary(modelo2)$coefficients)

rownames(coef2) <- c("Intercepto","ln(D/PIB)","Ciclo")
colnames(coef2) <- c("Estimativa","Desvio padrão","Estatística t","p-valor")

# Fazendo o Gráfico: 

Haircuts$`Período` <- c(0)

Haircuts$`Período`[as.numeric(Haircuts$Date) >= 1990] <- "A partir de 1990"
Haircuts$`Período`[as.numeric(Haircuts$Date) < 1990] <- "Antes de 1990"

ggplot(data = as_tibble(Haircuts)) +
  geom_point(mapping = aes(x = HSZ, y = Ext, color = `Período`, shape = `Período`)) +
  ggtitle("Reestruturação x Maturidade") +
  labs(x = "Corte de Sturzenegger e Zettelmeyer", y = "Extensão de Maturidade")
  
# Estimando para a Ext

mod <- lm(Ext ~ ln_cg + cycle + DiscountRate,data = Haircuts)

coef3 <- coeftest(mod, vcov = vcovHC(mod, type = "HC0"))

coef3 <- as.data.frame(t(rbind(coef3[,1],coef3[,2],coef3[,3],coef3[,4])))

colnames(coef3) <- c("Estimativa","Desvio padrão","Estatística t","p-valor")
rownames(coef3) <- c("Intercepto","ln(D/PIB)","Ciclo","Tx %")

summary(mod)


