#!/usr/bin/env Rscript

install.packages("Hmisc")
library(Hmisc)
install.packages("dplyr")
library(dplyr)


args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Nalezy podac co najmniej jeden argument wejsciowy")
}

#daneMedyczne <- read.csv2(file=args[1], sep=";")
daneMedyczne <- read.csv2("C:\\Users\\Igor\\Desktop\\R\\przykladoweDane-Projekt.csv", sep=";",header=TRUE)
daneMedyczne
grupy<-unique(daneMedyczne$grupa)
grupy

#wypisanie o podmiance wartosci
for(i in 2:ncol(daneMedyczne)){
  if(is.numeric(daneMedyczne[[i]])){
    zastapioneKomurki<-which(is.na(daneMedyczne[[i]]))
    c<-any(is.na(daneMedyczne[[i]]))
    if(c==TRUE){
      cat("komorki zastapione w kolumnie nr:", i, ":", zastapioneKomurki, "\n")
    }else{
      next
    }
  } else {
    next
  }
}

#zastapienie wartosci na srednia
for(j in length(grupy)){
  tempMed<-daneMedyczne[daneMedyczne[[1]]==grupy[i],]
  for(i in 2:ncol(tempMed)){
    if(is.numeric(tempMed[[i]])){
      srednia<- mean(tempMed[[i]], na.rm=TRUE)
      tempMed[[i]]<- ifelse(is.na(tempMed[[i]]),srednia,tempMed[[i]])
   } else {
      next
    }
  }
  daneMedyczne2 <- daneMedyczne[daneMedyczne[[1]] != grupy[j], ]
  daneMedyczne2 <- rbind(daneMedyczne, tempMed)
  rm(tempMed)
}
daneMedyczne2
daneMedyczne<-na.omit(daneMedyczne2)
print(daneMedyczne)

#powiadomienie o danych odstajacych
for(j in length(grupy)){
  tempMed<-daneMedyczne[daneMedyczne[[1]]==grupy[j],]
    for(i in 2:ncol(tempMed)){
      if(is.numeric(tempMed[[i]])){
        q1 <- quantile(tempMed[[i]], 0.25, na.rm = TRUE)
        q3 <- quantile(tempMed[[i]], 0.75, na.rm = TRUE)  
        iqr <- q3 - q1
        lower <- q1 - 1.5 * iqr
        upper <- q3 + 1.5 * iqr
        outliers <- which(tempMed[[i]] < lower || tempMed[[i]] > upper)
        #outliers <- tempMed[[i]][abs(tempMed[[i]] - mean(tempMed[[i]])) > 3*sd(tempMed[[i]])]         #test trzech sigma
        #outliers <-which(outliers)
        cat("kolumna:",i,"komorka",outliers+1,"\n")
    }   
  }
}

#charakterystyki grup
for(i in 2:ncol(daneMedyczne)){                                     #jest to samo
  if(is.numeric(daneMedyczne[[i]])){
    podumowanie <- group_by(daneMedyczne, grupa) %>%
      summarise(
        min = format(round(min(daneMedyczne[[i]], na.rm = TRUE), 2), nsmall = 2),
        max = format(round(max(daneMedyczne[[i]], na.rm = TRUE), 2), nsmall = 2),
        mean = format(round(mean(daneMedyczne[[i]], na.rm = TRUE), 2), nsmall = 2),
        sd = format(round(sd(daneMedyczne[[i]], na.rm = TRUE), 2), nsmall = 2),
        median = format(round(median(daneMedyczne[[i]], na.rm = TRUE), 2), nsmall = 2),
        quantile1 = format(round(quantile(daneMedyczne[[i]], 0.25, na.rm = TRUE), 2), nsmall = 2),
        quantile3 = format(round(quantile(daneMedyczne[[i]], 0.75, na.rm = TRUE), 2), nsmall = 2)
      )#%>%paste(col_names[i],collapse="\n")
    print(podumowanie)                                    #zapisac gdzie do pliku
  }
}

podumowanie_wiek <- group_by(daneMedyczne, grupa) %>%
  summarise(
    count = n(),
    mean = format(round(mean(daneMedyczne$wiek, na.rm = TRUE), 2), nsmall = 2),
    sd = format(round(sd(daneMedyczne$wiek, na.rm = TRUE), 2), nsmall = 2),
    median = format(round(median(daneMedyczne$wiek, na.rm = TRUE), 2), nsmall = 2)
  )
print(podumowanie_wiek)
