#Last update: 2018-06-28

# Loading or installing missing packages
if(!require(tidyverse)){
        install.packages("tidyverse")
        library(tidyverse)
}

if(!require(gridExtra)){
        install.packages("gridExtra")
        library(gridExtra)
}

if(!require(grid)){
        install.packages("grid")
        library(grid)
}

if(!require(ineq)){
        install.packages("ineq")
        library(ineq)
}


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
PSD <- 0.0698

# Sodra 
SODRA <- 0.195 - PSD

f_2021 <- function(x){
        MMA <- 600
        NPD <- 500
        NPD_coef <- 0.23
        VDU <- 1068.8*1.289
        bruto <- x*1.289
        lubos <- 5*VDU
        npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
        mok_baz <- pmax(0,(bruto-npd))
        gpm <- ifelse(bruto<=lubos, mok_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
        sodra <- pmin(bruto*SODRA, lubos * SODRA)
        psd <- bruto*PSD
        neto <- bruto - gpm - sodra - psd
        db <- ifelse(bruto<MMA, MMA*0.0124 + (MMA-bruto)*(SODRA+PSD),bruto*0.0124)
        DVK <- bruto+db
        list(bruto=x, new_bruto_2021=bruto, npd_2021=npd, gpm_2021 = gpm, sodra_2021 = sodra+db+psd, neto_2021 = neto, ITR_2021=((DVK-neto)/DVK), tax_2021=(DVK-neto))
} 


f_2020 <- function(x) {
        MMA <- 582
        NPD <- 400
        NPD_coef <- 0.2
        VDU <- 1008.4*1.289
        bruto <- x*1.289
        lubos <- 7*VDU
        npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
        mok_baz <- pmax(0,(bruto-npd))
        gpm <- ifelse(bruto<=lubos, mok_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
        sodra <- pmin(bruto*SODRA, lubos * SODRA)
        psd <- bruto*PSD
        neto <- bruto - gpm - sodra - psd
        db <- ifelse(bruto<MMA, MMA*0.0124 + (MMA-bruto)*(SODRA+PSD),bruto*0.0124)
        DVK <- bruto+db
        list(bruto=x, new_bruto_2020=bruto, npd_2020=npd, gpm_2020 = gpm, sodra_2020 = sodra+db+psd, neto_2020 = neto, ITR_2020=((DVK-neto)/DVK), tax_2020=(DVK-neto))
}

f_2019 <- function(x) {
        MMA <- 516
        NPD <- 300
        NPD_coef <- 0.15
        VDU <- 951.2*1.289
        bruto <- x*1.289
        lubos <- 10*VDU
        npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
        mok_baz <- pmax(0,(bruto-npd))
        gpm <- ifelse(bruto<=lubos, mok_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
        sodra <- pmin(bruto*SODRA, lubos * SODRA)
        psd <- bruto*PSD
        neto <- bruto - gpm - sodra - psd
        db <- ifelse(bruto<MMA, MMA*0.0124+(MMA-bruto)*(SODRA+PSD),bruto*0.0124)
        DVK <- bruto+db
        list(bruto=x, new_bruto_2019=bruto, npd_2019=npd, gpm_2019 = gpm, sodra_2019 = sodra+db+psd, neto_2019 = neto, ITR_2019=((DVK-neto)/DVK), tax_2019=(DVK-neto))
}

f_2018 <- function(x) {
        MMA <- 400
        NPD <- 380
        NPD_coef <- 0.5
        bruto <- x
        npd <- pmax(NPD - 0.5*pmax(0,(bruto - MMA)),0)
        tax_base <- pmax(0,(bruto-npd))
        gpm <- tax_base*0.15
        sodra <- bruto*0.03
        psd <- bruto * 0.06 
        neto <- bruto - gpm - sodra - psd
        db <- pmax(bruto*0.3118, MMA*0.3118)
        DVK <- bruto+db
        list(bruto=x, 
             new_bruto_2018=bruto, 
             npd_2018=npd, 
             gpm_2018 = gpm, 
             sodra_2018 = sodra+db+psd, 
             neto_2018 = neto, 
             ITR_2018=((DVK-neto)/DVK), 
             tax_2018=(DVK-neto))
}


#===========================================================================================================================

#====================== CHARTS x values for the functions
x <- seq(200, 15000, by=10)

#====================== ITR
df <- data.frame(bruto=f_2018(x)$bruto,
                 ITR_2018=f_2018(x)$ITR_2018,
                 ITR_2019=f_2019(x)$ITR_2019,
                 ITR_2020=f_2020(x)$ITR_2020,
                 ITR_2021=f_2021(x)$ITR_2021)%>%
        gather(var, values,2:5)

jpeg("./figures/ITR_new.jpeg", width = 9, height = 5, units = 'in', res = 600)
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
df <- data.frame(bruto=f_2018(x)$bruto,
                 net_2018=f_2018(x)$neto_2018,
                 net_2019=f_2019(x)$neto_2019,
                 net_2020=f_2020(x)$neto_2020,
                 net_2021=f_2021(x)$neto_2021)%>%
        gather(var, values,2:5)

jpeg("./figures/neto_new.jpeg", width = 9, height = 5, units = 'in', res = 600)
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
df <- data.frame(bruto=f_2018(x)$bruto,
                 VSD_2018=f_2018(x)$sodra_2018,
                 VSD_2019=f_2019(x)$sodra_2019,
                 VSD_2020=f_2020(x)$sodra_2020,
                 VSD_2021=f_2021(x)$sodra_2021) %>%
        gather(var, values,2:5)

jpeg("./figures/sodra_new.jpeg", width = 9, height = 5, units = 'in', res = 600)
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
df <- data.frame(bruto=f_2018(x)$bruto,
                 GPM_2018=f_2018(x)$gpm_2018,
                 GPM_2019=f_2019(x)$gpm_2019,
                 GPM_2020=f_2020(x)$gpm_2020,
                 GPM_2021=f_2021(x)$gpm_2021) %>%
        gather(var, values,2:5)

jpeg("./figures/gpm_new.jpeg", width = 9, height = 5, units = 'in', res = 600)
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

#====================== mixed chart 

jpeg("./figures/all_new.jpeg", width = 9, height = 6, units = 'in', res = 600)
grid.arrange(gpm, sodra, neto, itr, ncol=2, top=textGrob("Mokesčių reformos pasekmės 2018-2021", gp=gpar(fontsize=15,font=8)))
dev.off()


#===========================================================================================================================

#====================== Real effects using SODRA dataset

#====================== Reading income data 2018-04
data <- read.csv("apdraustuju_pajamu_analize.csv",
                 sep=";",
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 dec=".",
                 fileEncoding="ISO-8859-13")

#====================== Tax effects
tax_loss <- data.frame(
        tax_2019= f_2019(data$Mėnesio.pajamos)$tax_2019 - f_2018(data$Mėnesio.pajamos)$tax_2018,
        tax_2020=f_2020(data$Mėnesio.pajamos)$tax_2020 - f_2018(data$Mėnesio.pajamos)$tax_2018,
        tax_2021=f_2021(data$Mėnesio.pajamos)$tax_2021 - f_2018(data$Mėnesio.pajamos)$tax_2018) %>% 
        summarise_all(funs(sum))*12/1000000

tax_loss <- tax_loss %>% mutate(BASELINE=tax_2019+tax_2020+tax_2021)
tax_loss 
#====================== Wage inequality calculation based on SODRA data set

ineq(f_2018(data$Mėnesio.pajamos)$neto_2018)
ineq(f_2019(data$Mėnesio.pajamos)$neto_2019)
ineq(f_2020(data$Mėnesio.pajamos)$neto_2020)
ineq(f_2021(data$Mėnesio.pajamos)$neto_2021)



#====================== Net benefit from tax reform
net_d <- data.frame(
        bruto_2018=f_2018(data$Mėnesio.pajamos)$bruto,
        net_2018=f_2018(data$Mėnesio.pajamos)$neto_2018,
        net_2021=f_2021(data$Mėnesio.pajamos)$neto_2021)
net_d <- net_d%>%  mutate(tax_change=round(net_2021-net_2018, digits=0))%>%
        mutate(net_change_p=round(((net_2021/net_2018-1)*100), digits=0)) 

#====================== charts
jpeg("./figures/net_change_p.jpeg", width = 9, height = 6, units = 'in', res = 600)
ggplot(data=net_d %>% filter(net_d$bruto_2018>=200, net_d$bruto_2018<50000), aes(x=bruto_2018, y=net_change_p))+
        geom_line()+
        scale_x_continuous(breaks = seq(0,50000, by=1000))+
        scale_y_continuous(breaks = seq(0,15, by=1))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        scale_color_brewer(palette="Set1")+
        theme(plot.title = element_text(hjust = 0.0, face="bold"))+
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              axis.text.x = element_text(angle=90, hjust=1))+
        labs(   title="Procenintinis neto pajamų pokytis (2021m. vs 2018m.) [mokesčių reformos nauda]",
                subtitle= "Duomenys: Sodra, Skaičiavimas: Lithuanian-Economy.net",
                x="Bruto atlyginimas 2018m.",
                y="Procentinis neto pajamų pokytis")
dev.off()        

#====================== charts
jpeg("./figures/net_change_p_full.jpeg", width = 9, height = 6, units = 'in', res = 600)
ggplot(data=net_d, aes(x=bruto_2018, y=tax_change_p))+
        geom_line()+
        scale_x_continuous(breaks = seq(0,250000, by=10000))+
        scale_y_continuous(breaks = seq(0,15, by=1))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        scale_color_brewer(palette="Set1")+
        theme(plot.title = element_text(hjust = 0.0, face="bold"))+
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              axis.text.x = element_text(angle=90, hjust=1))+
        labs(   title="Procenintinis neto pajamų pokytis (2021m. vs 2018m.) [mokesčių reformos nauda]",
                subtitle= "Duomenys: Sodra, Skaičiavimas: Lithuanian-Economy.net",
                x="Bruto atlyginimas 2018m.",
                y="Procentinis neto pajamų pokytis")
dev.off()     
