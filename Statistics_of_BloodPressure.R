#Büşra_GÖKMEN_150116027
HW1_Data_v1 = read.csv("Downloads/HW1_Data_v1.csv")
manSYSBP = HW1_Data_v1[HW1_Data_v1$GENDER == 0, "SYSBP"]
womenSYSBP = HW1_Data_v1[HW1_Data_v1$GENDER == 1, "SYSBP"]
manDIASBP = HW1_Data_v1[HW1_Data_v1$GENDER == 0, "DIASBP"]
womenDIASBP = HW1_Data_v1[HW1_Data_v1$GENDER == 1, "DIASBP"]
#a
manMean1 = mean(manSYSBP)
womenMean1 = mean(womenSYSBP)
manMean2 = mean(manDIASBP)
womenMean2 = mean(womenDIASBP)
#b
mansvar1 = var(manSYSBP)
mansvar2 = var(manDIASBP)
womenvar1 = var(womenSYSBP)
womenvar2 = var(womenDIASBP)
#c
mansd1 = sd(manSYSBP)
mansd2 = sd(manDIASBP)
womensd1 = sd(womenSYSBP)
womensd2 = sd(womenDIASBP)
#d
manq1=quantile(manSYSBP,0.25)
manq2=quantile(manSYSBP,0.75)
manq3=quantile(manDIASBP,0.25)
manq4=quantile(manDIASBP,0.75)

womenq1=quantile(womenSYSBP,0.25)
womenq2=quantile(womenSYSBP,0.75)
womenq3=quantile(womenDIASBP,0.25)
womenq4=quantile(womenDIASBP,0.75)
#e
menmin1 = min(womenSYSBP)
menmax1 = max(womenSYSBP)
menmin2 = min(womenDIASBP)
menmax2 = max(womenDIASBP)

womenmin1 = min(womenSYSBP)
womenmax1 = max(womenSYSBP)
womenmin2 = min(womenDIASBP)
womenmax2 = max(womenDIASBP)
#f
menran1 = range(manSYSBP)
menran2 = range(manDIASBP)

womenran1 = range(womenSYSBP)
womenran2 = range(womenDIASBP)
#g
menrate1 = menran1/mansd1 
menrate2 = menran2/mansd2 

womenrate1 = womenran1/womensd1
womenrate2 = womenran2/womensd2
#h
menmed1 = median(manSYSBP)
menmed2 = median(manDIASBP)

womenmed1 = median(womenSYSBP)
womenmed2 = median(womenDIASBP)
#i
menIQR1 = IQR(manSYSBP)
menIQR2 = IQR(manDIASBP)

womenIQR1 = IQR(womenSYSBP)
womenIQR2 = IQR(womenDIASBP)
#j
mensummary1 = fivenum(manSYSBP)
mensummary2 = fivenum(manDIASBP)

womensummary1 = fivenum(womenSYSBP)
womensummary2 = fivenum(womenDIASBP)
#k
boxplot(HW1_Data_v1$SYSBP ~ HW1_Data_v1$GENDER)

boxplot(HW1_Data_v1$DIASBP ~ HW1_Data_v1$GENDER)
#l
stem(HW1_Data_v1$SYSBP[HW1_Data_v1$GENDER==0])
stem(HW1_Data_v1$DIASBP[HW1_Data_v1$GENDER==0])
stem(HW1_Data_v1$SYSBP[HW1_Data_v1$GENDER==1])
stem(HW1_Data_v1$DIASBP[HW1_Data_v1$GENDER==1])
#m
hist(HW1_Data_v1$SYSBP[HW1_Data_v1$GENDER==0])
hist(HW1_Data_v1$DIASBP[HW1_Data_v1$GENDER==0])
hist(HW1_Data_v1$SYSBP[HW1_Data_v1$GENDER==1])
hist(HW1_Data_v1$DIASBP[HW1_Data_v1$GENDER==1])
#n
dotchart(HW1_Data_v1$SYSBP[HW1_Data_v1$GENDER==0])
dotchart(HW1_Data_v1$DIASBP[HW1_Data_v1$GENDER==0])
dotchart(HW1_Data_v1$SYSBP[HW1_Data_v1$GENDER==1])
dotchart(HW1_Data_v1$DIASBP[HW1_Data_v1$GENDER==1])





#q
plot(HW1_Data_v1$SYSBP[HW1_Data_v1$GENDER==0],HW1_Data_v1$DIASBP[HW1_Data_v1$GENDER==0])
plot(HW1_Data_v1$SYSBP[HW1_Data_v1$GENDER==1],HW1_Data_v1$DIASBP[HW1_Data_v1$GENDER==1])

#r
library(HistogramTools)

PlotRelativeFrequency(hist(HW1_Data_v1$SYSBP[HW1_Data_v1$GENDER==0],plot = F))
PlotRelativeFrequency(hist(HW1_Data_v1$DIASBP[HW1_Data_v1$GENDER==0],plot = F))

PlotRelativeFrequency(hist(HW1_Data_v1$SYSBP[HW1_Data_v1$GENDER==1],plot = F))
PlotRelativeFrequency(hist(HW1_Data_v1$DIASBP[HW1_Data_v1$GENDER==1],plot = F))

#s
zscoremenlargestSYSBP = (menmin1-manMean1)/mansd1
zscoremenlargestDIASBP = (menmin2-manMean2)/mansd2
zscoremensmallestSYSBP = manMean1
zscoremensmallestDIASBP = manMean2

zscorewomenlargestSYSBP = (womenmin1-womenMean1)/womensd1
zscorewomenlargestDIASBP = (womenmin2-womenMean2)/womensd2
zscorewomensmallestSYSBP = womenMean1
zscorewomensmallestDIASBP = womenMean2




