#RE-INFECTION SCREENING ALGORITHM
#Algorithm created by Timothy Cavazzotto 
#Started 22/10/2022 final version at: 04/12/2022
#Purpose:  screening covid19 reinfection by personal document and valid exam
#excluding duplication and errors 

#### Packages #########
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

#### Data #############
data1 <- read_excel("amostra1_d1.xlsx")
data2 <- read_excel("amostra1_d2.xlsx")
data3 <- read_excel("1_10fev_22.xlsx")
data4 <- read_excel("1_15ABRIL_22.xlsx")
data5 <- read_excel("1_15jan_2022.xlsx")
data6 <- read_excel("1_15maio_2021.xlsx")
data7 <- read_excel("11_20FEV_22.xlsx")
data8 <- read_excel("16_30ABRIL_22.xlsx")
data9 <- read_excel("16_31jan_2022.xlsx")
data10 <- read_excel("16_31maio_2021.xlsx")
data11 <- read_excel("21_29fev_22.xlsx")
data12 <- read_excel("27_31Dez_2021.xlsx")
data13 <- read_excel("31jul_2021.xlsx")
data14 <- read_excel("abr_2021.xlsx")
data15 <- read_excel("ago_22.xlsx")
data16 <- read_excel("ago_2021.xlsx")
data17 <- read_excel("dez_2020.xlsx")
data18 <- read_excel("dez_2022.xlsx")
data19 <- read_excel("fev_2021.xlsx")
data20 <- read_excel("jan_2021.xlsx")
data21 <- read_excel("jul_22.xlsx")
data22 <- read_excel("jul_2021.xlsx")
data23 <- read_excel("jun_2021.xlsx")
data24 <- read_excel("junho_22.xlsx")
data25 <- read_excel("maio_22.xlsx")
data26 <- read_excel("mar_2021.xlsx")
data27 <- read_excel("marc_22.xlsx")
data28 <- read_excel("nov_2022.xlsx")
data29 <- read_excel("nov_dez_2021.xlsx")
data30 <- read_excel("out_2022.xlsx")
data31 <- read_excel("out_nov_2020.xlsx")
data32 <- read_excel("set_22.xlsx")
data33 <- read_excel("set_out_2021.xlsx")




data <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9,
              data10, data11, data12, data13, data14, data15, data16, data17,
              data18, data19, data20, data21, data22, data23, data24, data25,
              data26, data27, data28, data29, data30, data31, data32, data33) #merge data to 1 df

#write.csv2(data, file = "all_data_covid.csv")


#frequencies of duplicates
# max(table(data$cpf))

#create subset to detect duplicates and covid_positives
#attributing classes of data
data$cpf<-as.numeric(data$cpf)
data$cns<-as.numeric(data$cns)

#### clean data #######

#clean rows with missing in selected variables
df <- data[!is.na(data$cpf),]
df <- df[!is.na(df$data_coleta),]
df <- df[!is.na(df$resultado),]
df <- df[!is.na(df$exame),]
df <- df[order(df$cpf),]
df$cpf[df$cpf==0]<-NA
df<- df[!is.na(df$cpf),]

df$regional_clean= ifelse(df$regional_saude == "Não informado", 
                              0,df$regional_saude)
df$regional_clean= ifelse(df$regional_saude == "Outra UF", 
                              0, df$regional_clean)

datab1 <- df%>%
  filter(regional_clean > 0)


# #frequencies of duplicates
# # max(table(df$cpf))
# 
# datab1<-df

table(datab1$raca_cor, useNA = "always")


#### Create Boolean variables ####

#positive cases by results
datab1$resultado_pos<-0
datab1$resultado_pos[datab1$resultado== "Coronavírus (SARS-COV2)"]<-1
datab1$resultado_pos[datab1$resultado== "Reagente"]<-1
datab1$resultado_pos[datab1$resultado_pos!= 1]<-0

#positive cases by list of valid exam
datab1$exame_pos<-0
datab1$exame_pos[datab1$exame== "Celer Sansure Kit de Detecção por PCR em Tempo Real para SARS-CoV-2"]<-1
datab1$exame_pos[datab1$exame== "COBAS SARS-CoV-2"]<-1
datab1$exame_pos[datab1$exame== "COVID-19 Ag ECO Teste"]<-1
datab1$exame_pos[datab1$exame== "COVID-19 Ag Rapid Test Device - Panbio"]<-1
datab1$exame_pos[datab1$exame== "COVID-19 Real-Time PCR Kit"]<-1
datab1$exame_pos[datab1$exame== "COVID-19, Biologia Molecular"]<-1
datab1$exame_pos[datab1$exame== "DETECT SARS-CoV-2 RT-PCR"]<-1
datab1$exame_pos[datab1$exame== "Diagnostic Kit for Novel-Coronavirus(2019-nCoV) RNA"]<-1
datab1$exame_pos[datab1$exame== "Diagnóstico Molecular CORONÁVIRUS COVID-19"]<-1
datab1$exame_pos[datab1$exame== "ECO F COVID-19 Ag"]<-1
datab1$exame_pos[datab1$exame== "EURORealTime SARS-CoV-2	RT-PCR"]<-1
datab1$exame_pos[datab1$exame== "Família Abbott RealTime SARS-CoV-2"]<-1
datab1$exame_pos[datab1$exame== "Família Abbott RealTime SARS-CoV-2 EUA"]<-1
datab1$exame_pos[datab1$exame== "FAMÍLIA BIO GENE COVID-19 PCR"]<-1
datab1$exame_pos[datab1$exame== "Família cobas SARS-CoV-2"]<-1
datab1$exame_pos[datab1$exame== "Família Kit de Detecção por PCR em Tempo Real VIASURE SARS-CoV-2"]<-1
datab1$exame_pos[datab1$exame== "FAMÍLIA KIT XGEN MASTER COVID-19"]<-1
datab1$exame_pos[datab1$exame== "GeneFinderTM COVID-19 PLUS RealAmp Kit"]<-1
datab1$exame_pos[datab1$exame== "Kit MOLECULAR SARS-CoV2 (E/P1)"]<-1
datab1$exame_pos[datab1$exame== "Kit Molecular SARS-CoV2 (E/RP)"]<-1
datab1$exame_pos[datab1$exame== "KIT XGEN MASTER COVID-19"]<-1
datab1$exame_pos[datab1$exame== "Novel Coronavirus (2019-nCoV) Nucleic Acid Detection Kit PCR-Fluorescence Probing"]<-1
datab1$exame_pos[datab1$exame== "Panbio COVID-19 Ag Rapid Test"]<-1
datab1$exame_pos[datab1$exame== "RealStar® SARS-CoV-2 RT-PCR Kit 1.0"]<-1
datab1$exame_pos[datab1$exame== "SARS-COV-2 RT-PCR KIT"]<-1
datab1$exame_pos[datab1$exame== "SARS-CoV-2 S gene for BD Max"]<-1
datab1$exame_pos[datab1$exame== "TaqPath™ COVID-19 CE-IVD RT PCR Kit"]<-1
datab1$exame_pos[datab1$exame== "Teste Molecular (RT-PCR)"]<-1
datab1$exame_pos[datab1$exame== "Teste Rápido (Molecular)"]<-1
datab1$exame_pos[datab1$exame== "Teste Rápido Antígeno (Imunocromatografia)"]<-1
datab1$exame_pos[datab1$exame== "Teste Rápido Antígeno (Imunofluorescência)"]<-1
datab1$exame_pos[datab1$exame== "TR COVID-19 Ag - Bio-Manguinhos"]<-1
datab1$exame_pos[datab1$exame== "TR DPP® COVID-19 AG- Bio-Manguinhos"]<-1
datab1$exame_pos[datab1$exame== "TR SARS COV 2 AG - Bio-Manguinhos"]<-1
datab1$exame_pos[datab1$exame== "Xpert Xpress SARS-CoV-2"]<-1
datab1$exame_pos[datab1$exame== "COVID-19 Ag Rapid Test Device - Panbio"] <-1
datab1$exame_pos[datab1$exame== "COVID-19 Real-Time PCR Kit"] <-1
datab1$exame_pos[datab1$exame== "COVID-19, Biologia Molecular"] <-1
datab1$exame_pos[datab1$exame== "ECO F COVID-19 Ag"] <-1
datab1$exame_pos[datab1$exame== "Panbio COVID-19 Ag Rapid Test"] <-1
datab1$exame_pos[datab1$exame== "Teste Molecular (RT-PCR)"] <-1
datab1$exame_pos[datab1$exame== "Teste Rápido (Molecular)"] <-1
datab1$exame_pos[datab1$exame== "Teste Rápido Antígeno (Imunocromatografia)"] <-1
datab1$exame_pos[datab1$exame== "Teste Rápido Antígeno (Imunofluorescência)"] <-1
datab1$exame_pos[datab1$exame== "TR COVID-19 Ag - Bio-Manguinhos"] <-1
datab1$exame_pos[datab1$exame== "TR COVID-19 AG - IBMP"] <-1
datab1$exame_pos[datab1$exame== "TR DPP® COVID-19 AG- Bio-Manguinhos"] <-1
datab1$exame_pos[datab1$exame== "TR SARS COV 2 AG - Bio-Manguinhos"] <-1

datab1$resultado_pos<-as.numeric(datab1$resultado_pos)
datab1$exame_pos<-as.numeric(datab1$exame_pos)
datab1$covid_positivo_valido<-datab1$exame_pos + datab1$resultado_pos
datab1$covid_positivo_valido<-ifelse(datab1$covid_positivo_valido==2,
                                     1,0)


datab1$data_coleta_n<-as.Date(datab1$data_coleta, format = "%d/%m/%Y")

#frequencies of duplicates
# max(table(datab1$cpf))

#### reinfection screening algorithm ########

save.image("~/ARQUIVOS TIMOTHY/CONSULTORIAS/Consultorias/CONSULTORIAS/UNICENTRO/Luialís/Workspace1.RData")

datab1$data_coleta_n<-as.Date(datab1$data_coleta_n, format = "%d/%m/%Y")
datab1 <- datab1[order(datab1$cpf, datab1$data_coleta_n),]
datab1$coleta1<-datab1$data_coleta_n
datab1$cpf1<-datab1$cpf

# #### create variable with cases of >1 exam
# datab1$cpf_dupli<-0
# datab1$cpf1<-datab1$cpf
# for (i in 1:length(datab1$cpf1)){
#   ifelse (datab1$cpf1[i]==datab1$cpf1[i-1] | datab1$cpf1[i]==datab1$cpf1[i+1],
#           datab1$cpf_dupli[i]<- 1,0)}


datab1 <- datab1[order(datab1$cpf, datab1$data_coleta_n),]

save.image("~/ARQUIVOS TIMOTHY/CONSULTORIAS/Consultorias/CONSULTORIAS/UNICENTRO/Luialís/Workspace1.RData")

#Creating the wave variable
datab1$onda<-0
datab1 <- datab1 %>%
  arrange(data_coleta_n) %>%
  mutate(
    onda=ifelse(
      data_coleta_n %within% interval(as.Date("2020-03-11"), 
                                      as.Date("2020-10-12")), 1,
      ifelse(
        data_coleta_n %within% interval(as.Date("2020-10-17"),
                                        as.Date("2021-12-27")), 2,
        
        ifelse(
          data_coleta_n %within% interval(as.Date("2021-12-26"),
                                          as.Date("2023-03-18")),3,onda
        ))))%>%
  arrange(cpf1) 

#creating Covid database based on first wave 
datab1 <- datab1 %>%
  filter(onda > 0)

#create variable with order of exam
datab1$ordemcoleta<-1
datab1 <- datab1 %>%
  group_by(cpf1) %>%
  arrange(cpf1) %>%
  mutate(
    ordemcoleta=cumsum(ordemcoleta)
  )

#create variable with infection order for separate cases by waves
datab1$ordem_infec<-ifelse(datab1$onda==1,
                           datab1$resultado_pos,
                           datab1$covid_positivo_valido)


#create variable with infection order
datab1<-datab1 %>%
  group_by(cpf1) %>%
  arrange(cpf1, ordemcoleta) %>%
  mutate(
    ordem_infec= ifelse(covid_positivo_valido==1, 
                        cumsum(ordem_infec), ordem_infec),
    infect_m1 = if_else(ordem_infec>0, 1, 0))

table(datab1$ordem_infec)


#create difference between date of infections in days
datab1<-datab1 %>%
  group_by(cpf1,infect_m1) %>%
  arrange(cpf1, ordem_infec) %>%
  mutate(
    diff_time_infec = as.numeric(difftime(data_coleta_n, lag(data_coleta_n), units = "days")),
    diff_time_infec = if_else(is.na(diff_time_infec), 0, diff_time_infec),
    diff_time_infec = if_else(infect_m1==0, 0, diff_time_infec),
    diff_time_infec1 = if_else(infect_m1==1, cumsum(diff_time_infec), 0)
  )

datab1<-datab1 %>%
  group_by(cpf1,infect_m1) %>%
  arrange(cpf1, ordem_infec) %>%
  mutate(
    diff_time_infec_certo = as.numeric(difftime(data_coleta_n, lag(data_coleta_n), units = "days")),
    diff_time_infec_certo = if_else(is.na(diff_time_infec_certo), 0, diff_time_infec_certo),
  )

datab1<-datab1 %>%
  mutate(
    diff_time_infec_certo2 = if_else(diff_time_infec_certo<0, diff_time_infec_certo*(-1), diff_time_infec_certo)
  )



table(datab1$diff_time_infec_certo2)

#dplyr form of reinfection cases
datab1<-datab1 %>%
  arrange(ordem_infec>0) %>%
  mutate(
    reinfectec2=ifelse(ordem_infec>1 & diff_time_infec>90,1,0))

table(datab1$reinfectec2)

#create a valid reinfection order
datab1<-datab1 %>%
  group_by(cpf1) %>%
  arrange(cpf1, ordem_infec) %>%
  mutate(
    ordem_infec_valid= ifelse(reinfectec2==1, 
                        cumsum(reinfectec2), 0))


#create a valid infection - primoinfection - reinfection order
# specifying prime-infection
datab1$reinf1<-ifelse(datab1$ordem_infec_valid==1,1,0)

datab1<-datab1 %>%
  group_by(cpf1, infect_m1) %>%
  arrange(ordem_infec) %>%
  mutate(
    diff_time_infec_b= lead(reinf1),
    primo_infec2= ifelse(diff_time_infec_b == 1,1,0)
  )

table(datab1$primo_infec2)
table(datab1$ordem_infec_valid, datab1$primo_infec2)
table(datab1$reinfectec2)

datab1<-datab1 %>%
  replace_na(list(
    primo_infec2=0,
    ordem_infec_valid=0))

datab1$primo_reinf<-ifelse(datab1$primo_infec2==1,1,
                           ifelse(datab1$ordem_infec_valid>0,
                                  datab1$ordem_infec_valid+1,0))

datab1$infec_primo_reinf<-datab1$primo_reinf
datab1$infec_primo_reinf<-ifelse(datab1$infect_m1==1 & 
                                   datab1$primo_reinf==0, 9, 
                                 datab1$infec_primo_reinf)
                           



# ##### DESCRIPTIVE ANALYSYS #####
# 
# #levels of variable infec_primo_reinfec
# # 0=no infection
# # 1=primo-infection
# # 2=reinfection 1
# # 3=reinfection 2
# # 4=reinfection 3
# # 5=reinfection 4
# # 9= infection excluding reinfections and primo-infections


datab1<-datab1 %>%
  mutate(
    faixaEtaria = case_when(
      idade >= 0 & idade <= 5 ~ "0 a 5 anos",
      idade >= 6 & idade <= 10 ~ "6 a 10 anos",
      idade >= 11 & idade <= 15 ~ "11 a 15 anos",
      idade >= 16 & idade <= 20 ~ "16 a 20 anos",
      idade >= 21 & idade <= 25 ~ "21 a 25 anos",
      idade >= 26 & idade <= 30 ~ "26 a 30 anos",
      idade >= 31 & idade <= 35 ~ "31 a 35 anos",
      idade >= 36 & idade <= 40 ~ "36 a 40 anos",
      idade >= 41 & idade <= 45 ~ "41 a 45 anos",
      idade >= 46 & idade <= 50 ~ "46 a 50 anos",
      idade >= 51 & idade <= 55 ~ "51 a 55 anos",
      idade >= 56 & idade <= 60 ~ "56 a 60 anos",
      idade >= 61 & idade <= 65 ~ "61 a 65 anos",
      idade >= 66 & idade <= 70 ~ "66 a 70 anos",
      idade >= 71 & idade <= 75 ~ "71 a 75 anos",
      idade >= 76 & idade <= 80 ~ "76 a 80 anos",
      idade >= 81 & idade <= 85 ~ "81 a 85 anos",
      idade >= 86 & idade <= 90 ~ "86 a 90 anos",
      idade >= 91 & idade <= 95 ~ "91 a 95 anos",
      idade >= 96 ~ ">95 anos"  ))
    

table(datab1$infec_primo_reinf)

write.csv2(datab1, file = "Reinfec_allWaves.csv")

#creating Covid database with reinfection cases
datab2 <- datab1 %>%
  filter(infec_primo_reinf > 0) %>%
  filter(infec_primo_reinf < 9)

write.csv2(datab2, file = "Reinfec_data.csv")


save.image("~/ARQUIVOS TIMOTHY/CONSULTORIAS/Consultorias/CONSULTORIAS/UNICENTRO/Luialís/Workspace1.RData")












#Predictive Analysis - testing models

table(datab1$sexo, datab1$faixaEtaria, datab1$infec_primo_reinf, useNA = "always")   
    
m1<-glm(reinfectec2~sexo+faixaEtaria, family = binomial(link = "logit"), data = datab1)    

require(mfx)
or<-logitor(reinfectec2~sexo+faixaEtaria, data = datab1)    
exp(coef(m1))



