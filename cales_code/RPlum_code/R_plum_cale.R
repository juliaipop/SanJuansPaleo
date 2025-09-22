

#Libraries needed
library("rbacon")
library("IntCal")
library("rplum")

#Set working directory 
setwd("C:/Users/PhD/Documents/Paper_3")


Plum(core="CALE",
     acc.mean = 15,
     thick = 0.5,
     BCAD=TRUE,
     date.sample = 2021.69,
     ra.case = 2, #"226Ra(Bq/kg)" used as supported activity
     remove.tail=FALSE,
     depths.file = TRUE)


accrate.depth.ghost()

accrate.age.ghost()



################################################################################

#accumulation 

accrate.d35 <- accrate.depth(d = 35) # change D

plot(density(accrate.d35), main= 'Accumulation rate at depth 35 cm',
     xlab= 'cm/yr',
     ylab='')


accrate.depth.ghost(cmyr=T, acc.lim = c(0,1.5))


accrate.age.ghost(acc.lim = c(0,40))

accrate.depth.ghost(acc.lim = c(0,40))





