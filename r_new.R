install.packages("tidyverse")
install.packages("gridExtra")
install.packages("grid")
install.packages("ineq")

library(tidyverse)
library(gridExtra)
library(grid)
library(ineq)

# http://finmin.lrv.lt/uploads/finmin/documents/files/LT_ver/%C5%BDiniasklaida/Konsultacinis_MPD_04_17_final.pdf
# http://finmin.lrv.lt/lt/aktualus-valstybes-finansu-duomenys/ekonomines-raidos-scenarijus
# https://www.delfi.lt/verslas/verslas/infografikas-kaip-ir-kam-keisis-mokesciu-nasta.d?id=77745685

# reading in income data 2018-04
data <- read.csv("apdraustuju_pajamu_analize.csv",
                 sep=";",
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 dec=".",
                 fileEncoding="ISO-8859-13")

# global variables
GPM_1 <- 0.20
GPM_2 <- 0.27
SODRA <- 0.195

f_new_2021 <- function(x) {
        MMA <- 600
        NPD <- 500
        NPD_coef <- 0.17
        VDU <- 1068.8*1.289
        bruto <- x*1.289
        DVK <- bruto*1.0124
        npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
        lubos <- 5*1.289*VDU
        mok_baz <- pmax(0,(bruto-npd))
        gpm <- ifelse(bruto<=lubos, mok_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
        sodra <- pmax(bruto*SODRA, MMA*SODRA)
        db <- bruto*0.0124
        neto <- bruto - gpm - sodra
        list(bruto=x, new_bruto_2021=bruto, npd_2021=npd, gpm_2021 = gpm, sodra_2021 = sodra+db, neto_2021 = neto, ITR_2021=((DVK-neto)/DVK), tax_2021=(DVK-neto))
}

f_new_2020 <- function(x) {
        MMA <- 582
        NPD <- 400
        NPD_coef <- 0.17
        VDU <- 1008.4*1.289
        bruto <- x*1.289
        DVK <- bruto*1.0124
        npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
        lubos <- 7*1.289*VDU
        mok_baz <- pmax(0,(bruto-npd))
        gpm <- ifelse(bruto<=lubos, mok_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
        sodra <- pmax(bruto*SODRA, MMA*SODRA)
        db <- bruto*0.0124
        neto <- bruto - gpm - sodra
        list(bruto=x, new_bruto_2020=bruto, npd_2020=npd, gpm_2020 = gpm, sodra_2020 = sodra+db, neto_2020 = neto, ITR_2020=((DVK-neto)/DVK), tax_2020=(DVK-neto))
}

f_new_2019 <- function(x) {
        MMA <- 516
        NPD <- 300
        NPD_coef <- 0.15
        VDU <- 951.2*1.289
        bruto <- x*1.289
        DVK <- bruto*1.0124
        npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
        lubos <- 10*1.289*VDU
        mok_baz <- pmax(0,(bruto-npd))
        gpm <- ifelse(bruto<=lubos, mok_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
        sodra <- pmax(bruto*SODRA, MMA*SODRA)
        db <- bruto*0.0124
        neto <- bruto - gpm - sodra
        list(bruto=x, new_bruto_2019=bruto, npd_2019=npd, gpm_2019 = gpm, sodra_2019 = sodra+db, neto_2019 = neto, ITR_2019=((DVK-neto)/DVK), tax_2019=(DVK-neto))
}

f_2018 <- function(x) {
        MMA <- 400
        NPD <- 380
        NPD_coef <- 0.5
        bruto <- x
        db <- pmax(bruto*0.3118, MMA*0.3118)
        DVK <- bruto+db
        npd <- pmax(NPD - 0.5*pmax(0,(bruto - MMA)),0)
        tax_base <- pmax(0,(bruto-npd))
        gpm <- tax_base*0.15
        sodra <- bruto*0.09
        neto <- bruto - gpm - sodra
        list(bruto=x, new_bruto_2018=bruto, npd_2018=npd, gpm_2018 = gpm, sodra_2018 = sodra+db, neto_2018 = neto, ITR_2018=((DVK-neto)/DVK), tax_2018=(DVK-neto))
}

#====================== ITR
x <- seq(200, 20000, by=10)
df_2018 <- f_2018(x)
df_2019 <- f_new_2019(x)
df_2020 <- f_new_2020(x)
df_2021 <- f_new_2021(x)
df <- data.frame(bruto=df_2018$bruto,
                 ITR_2018=df_2018$ITR_2018,
                 ITR_2019=df_2019$ITR_2019,
                 ITR_2020=df_2020$ITR_2020,
                 ITR_2021=df_2021$ITR_2021)
df <- df %>% gather(var, values,2:5)

jpeg(".figures/ITR_new.jpeg", width = 9, height = 5, units = 'in', res = 600)
itr <- ggplot(data=df, aes(x=bruto,y=values, color=var))+
        geom_line()+
        scale_x_continuous(breaks = seq(0,20000, by=1000))+
        scale_y_continuous(breaks = seq(0,.8, by=0.02))+
        scale_color_brewer(palette="Set1")+
        theme(plot.title = element_text(hjust = 0.0, face="bold"))+
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              axis.text.x = element_text(angle=90, hjust=1))+
        labs(#title="Mokestinė dalis procentais nuo visų darbo vietos kaštų (angl.: \"Imputed Tax Rate - ITR\")",
                #subtitle= "Lithuanian-Economy.net",
                x="Bruto atlyginimas 2018m.",
                y="ITR")
itr
dev.off()

#====================== net
x <- seq(0, 25000, by=200)
df_2018 <- f_2018(x)
df_2019 <- f_new_2019(x)
df_2020 <- f_new_2020(x)
df_2021 <- f_new_2021(x)
df <- data.frame(bruto=df_2018$bruto,
                 net_2018=df_2018$neto_2018,
                 net_2019=df_2019$neto_2019,
                 net_2020=df_2020$neto_2020,
                 net_2021=df_2021$neto_2021)
df$ID <- seq.int(nrow(df))
df <- df %>% gather(var, values,2:5)

jpeg(".figures/neto_new.jpeg", width = 9, height = 5, units = 'in', res = 600)
neto <- ggplot(data=df, aes(x=bruto,y=values, color=var))+
        geom_line()+
        scale_x_continuous(breaks = seq(0,15000, by=1000))+
        scale_y_continuous(breaks = seq(0,15000, by=1000))+
        scale_color_brewer(palette="Set1")+
        theme(plot.title = element_text(hjust = 0.0, face="bold"))+
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              axis.text.x = element_text(angle=90, hjust=1))+
        labs(
                #title="Neto",
                #subtitle= "Skaičiavimas: Lithuanian-Economy.net",
                x="Bruto atlyginimas 2018m.",
                y="Neto atlyginimas")
neto
dev.off()

#====================== sodra
x <- seq(0, 15000, by=200)
df_2018 <- f_2018(x)
df_2019 <- f_new_2019(x)
df_2020 <- f_new_2020(x)
df_2021 <- f_new_2021(x)
df <- data.frame(bruto=df_2018$bruto,
                 VSD_2018=df_2018$sodra_2018,
                 VSD_2019=df_2019$sodra_2019,
                 VSD_2020=df_2020$sodra_2020,
                 VSD_2021=df_2021$sodra_2021)
df <- df %>% gather(var, values,2:5)

jpeg(".figures/sodra_new.jpeg", width = 9, height = 5, units = 'in', res = 600)
sodra <- ggplot(data=df, aes(x=bruto,y=values, color=var))+
        geom_line()+
        scale_x_continuous(breaks = seq(0,15000, by=1000))+
        scale_y_continuous(breaks = seq(0,15000, by=1000))+
        scale_color_brewer(palette="Set1")+
        theme(plot.title = element_text(hjust = 0.0, face="bold"))+
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              axis.text.x = element_text(angle=90, hjust=1))+
        labs(
                #title="Neto",
                #subtitle= "Skaičiavimas: Lithuanian-Economy.net",
                x="Bruto atlyginimas 2018m.",
                y="Sodra įmokos (suma)")
sodra
dev.off()

#====================== gpm
x <- seq(0, 15000, by=200)
df_2018 <- f_2018(x)
df_2019 <- f_new_2019(x)
df_2020 <- f_new_2020(x)
df_2021 <- f_new_2021(x)
df <- data.frame(bruto=df_2018$bruto,
                 GPM_2018=df_2018$gpm_2018,
                 GPM_2019=df_2019$gpm_2019,
                 GPM_2020=df_2020$gpm_2020,
                 GPM_2021=df_2021$gpm_2021)
df <- df %>% gather(var, values,2:5)

jpeg(".figures/gpm_new.jpeg", width = 9, height = 5, units = 'in', res = 600)
gpm <- ggplot(data=df, aes(x=bruto,y=values, color=var))+
        geom_line()+
        scale_x_continuous(breaks = seq(0,15000, by=1000))+
        scale_y_continuous(breaks = seq(0,15000, by=1000))+
        scale_color_brewer(palette="Set1")+
        theme(plot.title = element_text(hjust = 0.0, face="bold"))+
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              axis.text.x = element_text(angle=90, hjust=1))+
        labs(
                #title="Neto",
                #subtitle= "Skaičiavimas: Lithuanian-Economy.net",
                x="Bruto atlyginimas 2018m.",
                y="GPM įmokos (suma)")
gpm
dev.off()

jpeg(".figures/all_new.jpeg", width = 9, height = 6, units = 'in', res = 600)
grid.arrange(gpm, sodra, neto, itr, ncol=2, top=textGrob("Mokesčių reformos pasekmės 2018-2021", gp=gpar(fontsize=15,font=8)))
dev.off()



#====================== Tax effects

df_2018 <- f_2018(data$Mėnesio.pajamos)
df_2019 <- f_new_2019(data$Mėnesio.pajamos)
df_2020 <- f_new_2020(data$Mėnesio.pajamos)
df_2021 <- f_new_2021(data$Mėnesio.pajamos)


tax_loss <- data.frame(
        tax_2019= df_2019$tax_2019 - df_2018$tax_2018,
        tax_2020=df_2020$tax_2020 - df_2018$tax_2018,
        tax_2021=df_2021$tax_2021 - df_2018$tax_2018) %>% 
        summarise_all(funs(sum))*12/1000000



tax_loss <- data.frame(bruto=df_2018$bruto,
                       sodra_2019=df_2019$sodra_2019-df_2018$sodra_2018,
                       sodra_2020=df_2020$sodra_2020-df_2018$sodra_2018,
                       sodra_2021=df_2021$sodra_2021-df_2018$sodra_2018,
                       tax_2019=df_2019$tax_2019-df_2018$tax_2018,
                       tax_2020=df_2020$tax_2020-df_2018$tax_2018,
                       tax_2021=df_2021$tax_2021-df_2018$tax_2018,
                       gpm_2019=df_2019$gpm_2019-df_2018$gpm_2018,
                       gpm_2020=df_2020$gpm_2020-df_2018$gpm_2018,
                       gpm_2021=df_2021$gpm_2021-df_2018$gpm_2018)
tax_loss$ID <- seq.int(nrow(tax_loss))
tax_loss <- tax_loss %>% gather(var, values,2:10)
tax_loss %>% group_by(var) %>%  summarise(sums = sum(values)*12/1000000)


#====================== Wage inequality calculation based on SODRA data set

ineq(df_2018$neto_2018)
ineq(df_2019$neto_2019)
ineq(df_2020$neto_2020)
ineq(df_2021$neto_2021)
