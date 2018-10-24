if(!require(tidyverse)){
        install.packages("tidyverse")
        library(tidyverse)
}

# INFO
# Mokestinio kredito (tax_credit) formulė: https://e-seimas.lrs.lt/portal/legalAct/lt/TAD/TAIS.171369/XPXienJYqo

f_2018 <- function(x) {
        MMA <- 400
        VDU <- 808.7
        SODRA <- 0.289
        PSD <- 0.09
        bruto <- x
        tax_base <- bruto*0.7
        tax_credit <- pmax(0,ifelse(tax_base<20000, tax_base*0.1, tax_base*(0.1-2/300000*(tax_base-20000))))
        gpm <- tax_base*0.15-tax_credit
        sodra <- pmin(ifelse(tax_base<MMA, MMA*SODRA, tax_base*0.5*SODRA), 28*VDU*SODRA)
        psd <- pmin(ifelse(tax_base<MMA, MMA*PSD, tax_base*0.5*PSD), 28*VDU*PSD)
        neto <-bruto - gpm - sodra - psd
        list(bruto=x, 
             new_bruto_2018=bruto, 
             gpm_2018 = gpm, 
             sodra_2018 = sodra,
             psd_2018=psd,
             VSD_2018=sodra+psd,
             tax_credit_2018=tax_credit,
             neto_2018 = neto, 
             ITR_2018=(sodra+psd+gpm)/bruto, 
             tax_2018=sodra+psd+gpm)
}

f_2018_k <- function(x) {
        MMA <- 400
        VDU <- 808.7
        SODRA <- 0.289+0.02
        PSD <- 0.09
        bruto <- x
        tax_base <- bruto*0.7
        tax_credit <- pmax(0,ifelse(tax_base<20000, tax_base*0.1, tax_base*(0.1-2/300000*(tax_base-20000))))
        gpm <- tax_base*0.15-tax_credit
        sodra <- pmin(ifelse(tax_base<MMA, MMA*SODRA, tax_base*0.5*SODRA), 28*VDU*SODRA)
        psd <- pmin(ifelse(tax_base<MMA, MMA*PSD, tax_base*0.5*PSD), 28*VDU*PSD)
        neto <-bruto - gpm - sodra - psd
        list(bruto=x, 
             new_bruto_2018_k=bruto, 
             gpm_2018_k = gpm, 
             sodra_2018_k = sodra,
             psd_2018_k=psd,
             VSD_2018_k=sodra+psd,
             tax_credit_2018_k=tax_credit,
             neto_2018_k = neto, 
             ITR_2018_k=(sodra+psd+gpm)/bruto, 
             tax_2018_k=sodra+psd+gpm)
}

f_2019 <- function(x) {
        MMA <- 400*1.289
        VDU <- 808.7*1.289
        SODRA <- 0.195-0.069
        PSD <- 0.069
        bruto <- x
        tax_base <- bruto*0.7
        tax_credit <- pmax(0,ifelse(tax_base<20000, tax_base*0.1, tax_base*(0.1-2/300000*(tax_base-20000))))
        gpm <- tax_base*0.15-tax_credit
        sodra <- pmin(ifelse(tax_base<MMA, MMA*SODRA, tax_base*SODRA), 43*VDU*SODRA)
        psd <- pmin(ifelse(tax_base<MMA, MMA*PSD, tax_base*PSD), 43*VDU*PSD)
        neto <-bruto - gpm - sodra - psd
        list(bruto=x, 
             new_bruto_2019=bruto, 
             gpm_2019 = gpm, 
             sodra_2019 = sodra,
             psd_2019=psd,
             VSD_2019=sodra+psd,
             tax_credit_2019=tax_credit,
             neto_2019 = neto, 
             ITR_2019=(sodra+psd+gpm)/bruto, 
             tax_2019=sodra+psd+gpm)
}
f_2019_k <- function(x) {
        MMA <- 400*1.289
        VDU <- 808.7*1.289
        SODRA <- 0.195-0.069
        PSD <- 0.069
        bruto <- x
        tax_base <- bruto*0.7
        tax_credit <- pmax(0,ifelse(tax_base<20000, tax_base*0.1, tax_base*(0.1-2/300000*(tax_base-20000))))
        gpm <- tax_base*0.15-tax_credit
        sodra <- pmin(ifelse(tax_base<MMA, MMA*SODRA, tax_base*SODRA), 43*VDU*SODRA)
        psd <- pmin(ifelse(tax_base<MMA, MMA*PSD, tax_base*PSD), 43*VDU*PSD)
        PII <- 0.03*tax_base
        neto <-bruto - gpm - sodra - psd-PII
        list(bruto=x, 
             new_bruto_2019_k=bruto, 
             gpm_2019_k = gpm, 
             sodra_2019_k = sodra+PII,
             psd_2019_k=psd,
             VSD_2019_k=sodra+psd+PII,
             tax_credit_2019_k=tax_credit,
             neto_2019_k = neto, 
             ITR_2019_k=(sodra+psd+gpm+PII)/bruto, 
             tax_2019_k=sodra+psd+gpm+PII)
}



# Graph the functions for values x
x <- seq(400, 200000, by=100)

df <- data.frame(
        bruto=f_2018(x)$bruto,
        ITR_2018=f_2018(x)$ITR_2018,
        ITR_2018_k=f_2018_k(x)$ITR_2018_k,
        ITR_2019=f_2019(x)$ITR_2019,
        ITR_2019_k=f_2019_k(x)$ITR_2019_k)%>%
        gather(var, values,2:5)


jpeg("./figures/IV_ITR_new.jpeg", width = 9, height = 5, units = 'in', res = 100)
ggplot(data=df, aes(x=bruto,y=values, color=var))+
        geom_line()+
        scale_x_continuous(breaks = seq(0,200000, by=10000))+
        scale_y_continuous(breaks = seq(0,.8, by=0.02))+
        scale_color_brewer(palette="Set1")+
        theme(plot.title = element_text(hjust = 0.0, face="bold"))+
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              axis.text.x = element_text(angle=90, hjust=1))+
        labs(title="individualios veiklos mokestinė dalis procentais nuo visų gautų pajamų (angl.: \"Imputed Tax Rate - ITR\")",
             subtitle= "Lithuanian-Economy.net",
             x="Pajamos 2018m.",
             y="ITR")
dev.off()

