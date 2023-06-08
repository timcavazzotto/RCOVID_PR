#RE-INFECTION SCREENING ALGORITHM
#Algorithm created by Timothy Cavazzotto 
#Started 22/10/2022 final version at: 04/12/2022
#Purpose:  screening covid19 reinfection by personal document and valid exam
#excluding duplication and errors 
#descriptive analysis

#### Packages #########
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)



##### DESCRIPTIVE ANALYSYS LUALIS ALLWAVES#####

#levels of variable infec_primo_reinfec
# 0=no infection
# 1=primo-infection
# 2=reinfection 1
# 3=reinfection 2
# 4=reinfection 3
# 5=reinfection 4
# 9= infection excluding reinfections and primo-infections



tabl1<-table(datab1$data_coleta_n,datab1$infec_primo_reinf)
tabl2<-table(datab1$onda,datab1$infec_primo_reinf)
tabl3<-table(datab1$data_coleta_n)
tabl4<-table(datab1$infec_primo_reinf,datab1$sexo)
tabl5<-table(datab1$infec_primo_reinf,datab1$raca_cor, useNA = "always")
tabl6<-table(datab1$faixaEtaria, datab1$infec_primo_reinf, useNA = "always")
tabl6.1<-table(datab1$idade, datab1$infec_primo_reinf, useNA = "always")
tabl7<-table(datab1$sexo, datab1$faixaEtaria, datab1$infec_primo_reinf, useNA = "always")
tabl8<-table(datab1$diff_time_infec, datab1$infec_primo_reinf, useNA = "always")  
tabl9<-table(datab1$diff_time_infec_certo2, datab1$infec_primo_reinf, datab1$sexo, useNA = "always")  
tabl11<-table(datab1$sexo, datab1$infec_primo_reinf, useNA = "always")
tabl12<-table(datab1$regional_saude, datab1$infec_primo_reinf, useNA = "always")
tabl13<-table(datab1$diff_time_infec_certo2, datab1$sexo, datab1$infec_primo_reinf)  
 
tabl10_mean<-xtabs(diff_time_infec_certo2~., 
      aggregate(diff_time_infec_certo2~sexo + infec_primo_reinf, 
                datab1, mean))

tabl10_sd<-xtabs(diff_time_infec_certo2~., 
              aggregate(diff_time_infec_certo2~sexo + infec_primo_reinf, 
                        datab1, sd))

write.csv2(tabl7, file = "tabl7.csv")


tabl14<- table(datab1$febre, datab1$infec_primo_reinf, useNA = "always")
tabl15<- table(datab1$tosse, datab1$infec_primo_reinf, useNA = "always")
tabl16<- table(datab1$dor_garganta, datab1$infec_primo_reinf, useNA = "always")
tabl17<- table(datab1$mialgia, datab1$infec_primo_reinf, useNA = "always")
tabl18<- table(datab1$artralgia, datab1$infec_primo_reinf, useNA = "always")
tabl19<- table(datab1$diarreia, datab1$infec_primo_reinf, useNA = "always")
tabl20<- table(datab1$nausea_vomitos, datab1$infec_primo_reinf, useNA = "always")
tabl21<- table(datab1$cefaleia, datab1$infec_primo_reinf, useNA = "always")
tabl22<- table(datab1$coriza, datab1$infec_primo_reinf, useNA = "always")
tabl23<- table(datab1$irritabilidade_confusao, datab1$infec_primo_reinf, useNA = "always")
tabl24<- table(datab1$adinamia, datab1$infec_primo_reinf, useNA = "always")
tabl25<- table(datab1$escarro, datab1$infec_primo_reinf, useNA = "always")
tabl26<- table(datab1$calafrios, datab1$infec_primo_reinf, useNA = "always")
tabl27<- table(datab1$congestao_nasal, datab1$infec_primo_reinf, useNA = "always")
tabl28<- table(datab1$congestao_conjuntiva, datab1$infec_primo_reinf, useNA = "always")
tabl29<- table(datab1$dificuldade_deglutir, datab1$infec_primo_reinf, useNA = "always")
tabl30<- table(datab1$manchas_vermelhas, datab1$infec_primo_reinf, useNA = "always")
tabl31<- table(datab1$ganglios_linfaticos, datab1$infec_primo_reinf, useNA = "always")
tabl32<- table(datab1$asas_nasais, datab1$infec_primo_reinf, useNA = "always")
tabl33<- table(datab1$saturacao_o2, datab1$infec_primo_reinf, useNA = "always")
tabl34<- table(datab1$cianose, datab1$infec_primo_reinf, useNA = "always")
tabl35<- table(datab1$tiragem_intercostal, datab1$infec_primo_reinf, useNA = "always")
tabl36<- table(datab1$dispneia, datab1$infec_primo_reinf, useNA = "always")
tabl37<- table(datab1$perda_olfato_paladar, datab1$infec_primo_reinf, useNA = "always")



tabl38<- table(datab1$doenca_cardiovascular, datab1$infec_primo_reinf, useNA = "always")
tabl39<- table(datab1$hipertensao, datab1$infec_primo_reinf, useNA = "always")
tabl40<- table(datab1$diabetes, datab1$infec_primo_reinf, useNA = "always")
tabl41<- table(datab1$doenca_hepatica, datab1$infec_primo_reinf, useNA = "always")
tabl42<- table(datab1$doenca_neurologica, datab1$infec_primo_reinf, useNA = "always")
tabl43<- table(datab1$sindrome_down, datab1$infec_primo_reinf, useNA = "always")
tabl44<- table(datab1$imunodeficiencia, datab1$infec_primo_reinf, useNA = "always")
tabl45<- table(datab1$infeccao_hiv, datab1$infec_primo_reinf, useNA = "always")
tabl46<- table(datab1$doenca_renal, datab1$infec_primo_reinf, useNA = "always")
tabl47<- table(datab1$doenca_pulmonar, datab1$infec_primo_reinf, useNA = "always")
tabl48<- table(datab1$neoplasia, datab1$infec_primo_reinf, useNA = "always")
tabl49<- table(datab1$puerperio, datab1$infec_primo_reinf, useNA = "always")
tabl50<- table(datab1$obesidade, datab1$infec_primo_reinf, useNA = "always")
tabl51<- table(datab1$tabagismo, datab1$infec_primo_reinf, useNA = "always")



tabl52<- table(datab1$hospitalizado, datab1$infec_primo_reinf, useNA = "always")
tabl52.1<-table(datab1$hospitalizado, datab1$infec_primo_reinf,datab1$sexo, useNA = "always")
tabl53<- table(datab1$evolucao, datab1$infec_primo_reinf, useNA = "always")
tabl53.1<-table(datab1$evolucao, datab1$infec_primo_reinf, datab1$sexo, useNA = "always")
tabl53.2<-table(datab1$evolucao, datab1$infec_primo_reinf, 
                datab1$sexo, datab1$faixaEtaria, useNA = "always")



write.csv2(tabl53.2, file = "tabl53_2.csv")

