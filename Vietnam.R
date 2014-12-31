
## 1. Total of publication in Vietnam (type=Article,Source=Journal, Language
## = English)
#Purely dosmetic articles
dos <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/overview/dos.csv")
dos <- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/overview/dos.csv")
#dos<-subset(dos,dos$Year>1995)
names(dos)

nrow(subset(dos,dos$Year<2005))
nrow(subset(dos,dos$Year>2004))

# Total article
total <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/overview/total.csv")
total <- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/overview/total.csv")
#total<-subset(total,total$Year>1995)

# Find the collaboration articles by subtract total to dosmetic

#Build a function to find posititon of pure dosmetic articles from total file
#by EID number
totalcha<-as.character(total$EID)
doscha<-as.character(dos$EID)
col<-function(x,y){
        pos<-NULL
        for (i in 1:length(x)){
                for (j in 1:length(y)){
                        if (x[i]==y[j]){
                                pos<-c(pos,i)
                        }
                }
        }
        pos
}
pos<-col(totalcha,doscha)
colla<-total[-pos,] 
write.csv(colla,"collaboration.csv")
#File already
collabo <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/overview/collaboration.csv")


nrow(subset(collabo,collabo$Year<2005))
nrow(subset(collabo,collabo$Year>2004))

##2. Building data frame for article  by year       

output<-data.frame(table(total$Year))
output
#dosmetic papers
dosoutput<-data.frame(table(dos$Year))
dosoutput

#Combine into output
colnames(output)<-c("year","total")
output$dosmetic<-dosoutput$Freq
output$collaboration<-output$total-output$dosmetic
# draw plot
str(output)
output$year<-as.integer(as.character(output$year))
library(reshape2)
Meltoutput<-melt(output,id=c("year"),measure.vars=c("dosmetic","collaboration","total"))
Meltoutput

library(ggplot2)
p=ggplot(Meltoutput,aes(year,value,color=variable))
p+geom_point(size=6)+geom_line(lwd=1)+labs(title="Scientific output in Viet Nam") +
        labs(x=expression("Year"),y=expression("Number of papers"))+
        theme_bw()+scale_x_continuous(breaks= seq(1996, 2013, 1))+
        theme(axis.text=element_text(size=15))+
        theme(axis.title=element_text(size=25))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=25))+
        theme(legend.position=c(0.7,0.8))+
        theme(plot.title=element_text(size=30,vjust=3,face="bold"))
        
        
        
        theme(panel.grid.major.x=element_blank())

#Building a growth rate model in two period (1996-2001) and 2002-2013
out1<-subset(output,output$year<2002)
out2<-subset(output,output$year>2001)
md1<-lm(total~I(year-1996),data=out1)
md2<-glm(total~I(year-2002),family="poisson",data=out2)
summary(md1)# y=248+16*t, R2=0.86, t=0 at 1996
summary(md2)# y=336.5*exp(0.18*t). R2=0.997, t=0 at 1996, increase rate at about ~20%
#make plot
plot(output$year,output$total,pch=19,xlab="Year",ylab="Number of publications",at=output$year)
title(main="Total yearly publication of Vietnam from 1996-2013")
lines(out1$year,md1$fitted,lwd=2)
lines(out2$year,md2$fitted,lwd=2)

# make csv data
write.csv(output,"data.csv")




## 3. Popular journal published
#domestic
dosJour<-sort(table(dos$Source.title))
dosJour
table(dos$Source.title=="Advances in Natural Sciences: Nanoscience and Nanotechnology",dos$Year)
write.csv(dosJour,"topdosJour.csv")
#result: Advances in Natural Sciences: Nanoscience and Nanotechnology (publish by VAST)
#since 2010, around 30 articles per year

## collaboration result
colJour<-sort(table(collabo$Source.title))
colJour
# Plos One is largest paper (139)
#number of VN corresponding author in this journal
length(grep("Viet Nam",plosone7Jul$Correspondence.Address))
39/142
write.csv(colJour,"colJour.csv")
## Total journal
totalJour<-sort(table(total$Source.title))
totalJour
write.csv(totalJour,"toptotalJour.csv")
#result:Livestock Research for Rural Development (201 articles)
table(total$Source.title=="Livestock Research for Rural Development",total$Year)








## 4. Country collaboration calculated from 1996-2013

# 4.1 Japan
#Rate of Vietnam/Japan corresponding authors 

JP <- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Japan/JP.csv")
#school computer
JP <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/Japan/JP.csv", na.strings=0)

nrow(subset(JP,JP$Year<2005 & JP$Year>1995))
nrow(subset(JP,JP$Year>2004))

jp<-data.frame(table(JP$Year))
jp
# Build a function to find correspoding author's country (no need more)
jpcorr<-NULL
vncorr<-NULL

for (i in 1:length(jp$Var1)) {
        sub<-subset(JP,JP$Year==jp[i,1])
        # Japan corresponding author
        num<-length(grep("Japan",sub$Correspondence.Address))
        jpcorr<-c(jpcorr,num)
        # Vietnam corresponding author
        vnnum<-length(grep("Viet Nam",sub$Correspondence.Address))
        vncorr<-c(vncorr,vnnum)
       
}
jpcorr
jp$jp<-jpcorr
jp$vn<-vncorr
jp$ratiovnjp<-jp$vn/jp$jp
jp
colnames(jp)<-c("year","total","japan","vietnam","ratio_VN/JP")
write.csv(jp,"jpcoll.csv")

# Japan collaboration with VN since 1996.
jpcoll <- read.csv("C:/Users/MANH/Dropbox/My research/Scientific outputs in VN/jpcoll.csv")
str(jpcoll)





#4.1.2 Japan citation by year

JP$Cited.by[is.na(JP$Cited.by)] <- c(0)

JP05<-subset(JP,JP$Year>=2005)
summary(JP05$Cited.by)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   4.000   9.645  11.000 491.000 

#4.1.3 Number of co-authors
au<-"Nguyen-Thoi, T., Phung-Van, P., Nguyen-Hoang, S., Lieu-Xuan, Q."
au<-"Lai, N.A." 
num<-length(strsplit(au,",")[[1]])/2
num

#4.1.4 Separating article with corresponding author from JP.
# position which corresponding author from JP
posjp<-grep("Japan",JP$Correspondence.Address)
JPcor<-JP[posjp,]
write.csv(JPcor,"JPcor.csv")

#One problem with vienamese first author, if they only keep japan address, it was not included in this data.
# in 2013, JP has 157 corres author. IN 157 articles, 88 firts author are Vietnamese.

#4.1.5 First authors are Vietnamese estimation (usally with more than 1 dot. .)
# Since Vietnamese name usally have 3 words, therefore more than 1., we can check to 
#compare with reality (for example year 2013, 2010)
JPau<-strsplit(as.character(JP$Authors),",")
JPau<-as.character(JP$Authors)
JPau[1]
length(JPau)
for (i in 1:length(JPau)){if JPau[i]}
te<-"Linh N.V."




## 4.2. US collaboration

US <- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/US/US.csv")
US <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/US/US.csv")
us<-data.frame(table(US$Year))
us<-us[14:31,] # year from 1996 to 2013
us
# Build a function to find correspoding author's country
uscorr<-NULL
vncorr<-NULL

for (i in 1:length(us$Var1)) {
        sub<-subset(US,US$Year==us[i,1])
        # Japan corresponding author
        num<-length(grep("United States",sub$Correspondence.Address))
        uscorr<-c(uscorr,num)
        # Vietnam corresponding author
        vnnum<-length(grep("Viet Nam",sub$Correspondence.Address))
        vncorr<-c(vncorr,vnnum)
        
}
uscorr
us$us<-uscorr
us$vn<-vncorr
us$ratiovnus<-us$vn/us$us
us
colnames(us)<-c("year","total","us","vietnam","ratio_VN/US")
write.csv(us,"uscoll.csv")
plot(jp$Var1,jp$ratiovnjp,xlab="year",ylab="VN/JP",pch=20)
title(main="ratio of correspoding author of VN and JP collaboration")

#4.2.2 US citation by year

US$Cited.by[is.na(US$Cited.by)] <- c(0)

US05<-subset(US,US$Year>=2005)
summary(US05$Cited.by)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0     1.0     5.0    17.2    14.0   693.0

#4.1.3 Number of co-authors
USau <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/US/USau.csv")
USau13<-subset(USau,USau$Year=="2013")
USau13<-as.character(USau13$Authors)
#au<-"Nguyen-Thoi, T., Phung-Van, P., Nguyen-Hoang, S., Lieu-Xuan, Q."
#au<-"Lai, N.A." 
#num<-length(strsplit(USau13[1],",")[[1]])
#num
USall<-NULL
for (i in 1:length(USau13)){
      num<-length(strsplit(USau13[i],",")[[1]])
      USall<-c(USall,num)}


summary(USall)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1.00    4.00    8.00   85.55   14.00  659.00 

#4.2.4 Separating article with corresponding author from US
# position which corresponding author from US
posus<-grep("United States",US$Correspondence.Address)
UScor<-US[posus,]
write.csv(UScor,"UScor.csv")
# in 2013, US has 149 corres author. IN 149 articles, 44 firts author are Vietnamese.

## 4.3 South Korea

SK <- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/South Korea/SK.csv")

SK <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/South Korea/SK.csv")

sk<-data.frame(table(SK$Year))
sk<-sk[2:19,]
sk
# Build a function to find correspoding author's country
skcorr<-NULL
vncorr<-NULL

for (i in 1:length(sk$Var1)) {
        sub<-subset(SK,SK$Year==sk[i,1])
        # Japan corresponding author
        num<-length(grep("South Korea",sub$Correspondence.Address))
        skcorr<-c(skcorr,num)
        # Vietnam corresponding author
        vnnum<-length(grep("Viet Nam",sub$Correspondence.Address))
        vncorr<-c(vncorr,vnnum)
        
}
skcorr
sk$sk<-skcorr
sk$vn<-vncorr
sk$ratiovnus<-sk$vn/sk$sk
sk
colnames(sk)<-c("year","total","sk","vietnam","ratio_VN/SK")
write.csv(sk,"skcoll.csv")
#4.3.2 South Korea SK citation by year

SK$Cited.by[is.na(SK$Cited.by)] <- c(0)

SK05<-subset(SK,SK$Year>=2005)
summary(SK05$Cited.by)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   3.000   9.401   9.000 522.000 

#4.3.3 Number of co-authors
SKau <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/South Korea/SKau.csv")
SKau13<-subset(SKau,SKau$Year=="2013")
SKau13<-as.character(SKau13$Authors)
#au<-"Nguyen-Thoi, T., Phung-Van, P., Nguyen-Hoang, S., Lieu-Xuan, Q."
#au<-"Lai, N.A." 
#num<-length(strsplit(USau13[1],",")[[1]])
#num
SKall<-NULL
for (i in 1:length(SKau13)){
        num<-length(strsplit(SKau13[i],",")[[1]])
        SKall<-c(SKall,num)}


summary(SKall)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
2.000   4.000   5.000   6.663   8.750  30.000 

#4.3.4 Separating article with corresponding author from South Korea
# position which corresponding author from SK
possk<-grep("South Korea",SK$Correspondence.Address)
SKcor<-SK[possk,]
write.csv(SKcor,"SKcor.csv")
# in 2013, SK has 200 corres author. IN 200 articles, 128 firts author are Vietnamese.


## 4.4 France

FR <- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/France/FR.csv")
FR <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/France/FR.csv")
fr<-data.frame(table(FR$Year))
fr<-fr[13:30,]
fr
# Build a function to find correspoding author's country
frcorr<-NULL
vncorr<-NULL

for (i in 1:length(fr$Var1)) {
        sub<-subset(FR,FR$Year==fr[i,1])
        # Japan corresponding author
        num<-length(grep("France",sub$Correspondence.Address))
        frcorr<-c(frcorr,num)
        # Vietnam corresponding author
        vnnum<-length(grep("Viet Nam",sub$Correspondence.Address))
        vncorr<-c(vncorr,vnnum)
        
}
frcorr
fr$fr<-frcorr
fr$vn<-vncorr
fr$ratiovnfr<-fr$vn/fr$fr
fr
colnames(fr)<-c("year","total","fr","vietnam","ratio_VN/FR")
write.csv(fr,"frcoll.csv")
#4.4.2 France FR citation by year

FR$Cited.by[is.na(FR$Cited.by)] <- c(0)

FR05<-subset(FR,FR$Year>=2005)
summary(FR05$Cited.by)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    1.00    4.00   13.37   12.00  522.00 

#4.4.4 Separating article with corresponding author from France.
# position which corresponding author from FR
posfr<-grep("France",FR$Correspondence.Address)
FRcor<-FR[posfr,]
write.csv(FRcor,"FRcor.csv")

# 4.5. 1.China
CN <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/China/CN.csv")
CN <- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/China/CN.csv")
cn<-data.frame(table(CN$Year))
cn<-cn[4:21,]
cn
# Build a function to find correspoding author's country
cncorr<-NULL
vncorr<-NULL

for (i in 1:length(cn$Var1)) {
        sub<-subset(CN,CN$Year==cn[i,1])
        # China corresponding author
        num<-length(grep("China",sub$Correspondence.Address))
        cncorr<-c(cncorr,num)
        # Vietnam corresponding author
        vnnum<-length(grep("Viet Nam",sub$Correspondence.Address))
        vncorr<-c(vncorr,vnnum)
        
}
cncorr
cn$cn<-cncorr
cn$vn<-vncorr
cn$ratiovncn<-cn$vn/cn$cn
cn
colnames(cn)<-c("year","total","cn","vietnam","ratio_VN/CN")
write.csv(cn,"cncoll.csv")
#4.5.2 CN citation by year

CN$Cited.by[is.na(CN$Cited.by)] <- c(0)

CN05<-subset(CN,CN$Year>=2005)
summary(CN05$Cited.by)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.00    1.00    4.00   15.85   16.00  387.00 


# 4.6. 1.Thailand
TH <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/Thailand/TH.csv")
TH <- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Thailand/TH.csv")

th<-data.frame(table(TH$Year))
th<-th[6:23,]
th
# Build a function to find correspoding author's country
thcorr<-NULL
vncorr<-NULL

for (i in 1:length(th$Var1)) {
        sub<-subset(TH,TH$Year==th[i,1])
        # Thailand corresponding author
        num<-length(grep("Thailand",sub$Correspondence.Address))
        thcorr<-c(thcorr,num)
        # Vietnam corresponding author
        vnnum<-length(grep("Viet Nam",sub$Correspondence.Address))
        vncorr<-c(vncorr,vnnum)
        
}
thcorr
th$th<-thcorr
th$vn<-vncorr
th$ratiovnth<-th$vn/th$th
th
colnames(th)<-c("year","total","th","vietnam","ratio_VN/TH")
write.csv(th,"thcoll.csv")
#4.6.2 Thailand citation by year

TH$Cited.by[is.na(TH$Cited.by)] <- c(0)

TH05<-subset(TH,TH$Year>=2005)
summary(TH05$Cited.by)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    2.00    6.00   18.01   18.00  522.00 

# 4.7. 1.United Kingdom
UK <- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/United Kingdom/UK.csv")
UK <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/United Kingdom/UK.csv")
uk<-data.frame(table(UK$Year))
uk<-uk[6:23,]
uk
# Build a function to find correspoding author's country
ukcorr<-NULL
vncorr<-NULL

for (i in 1:length(uk$Var1)) {
        sub<-subset(UK,UK$Year==uk[i,1])
        # UK corresponding author
        num<-length(grep("United Kingdom",sub$Correspondence.Address))
        ukcorr<-c(ukcorr,num)
        # Vietnam corresponding author
        vnnum<-length(grep("Viet Nam",sub$Correspondence.Address))
        vncorr<-c(vncorr,vnnum)
        
}
ukcorr
uk$uk<-ukcorr
uk$vn<-vncorr
uk$ratiovnuk<-uk$vn/uk$uk
uk
colnames(uk)<-c("year","total","uk","vietnam","ratio_VN/UK")
write.csv(uk,"ukcoll.csv")
#4.7.2 UK citation by year

UK$Cited.by[is.na(UK$Cited.by)] <- c(0)

UK05<-subset(UK,UK$Year>=2005)
summary(UK05$Cited.by)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    2.00    7.00   21.03   21.00  483.00

#4.7.4 Separating article with corresponding author from United Kingdom.
# position which corresponding author from UK
posuk<-grep("United Kingdom",UK$Correspondence.Address)
UKcor<-UK[posuk,]
write.csv(UKcor,"UKcor.csv")

# 5. Text analysis with title of articles
JPcorr = read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Japan/JPcor.csv", stringsAsFactors=FALSE)
JP <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/Japan/JP.csv", stringsAsFactors=FALSE)
UScor <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/US/UScor.csv", stringsAsFactors=FALSE)
FRcor <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/France/FRcor.csv", stringsAsFactors=FALSE)
SKcor <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/South Korea/SKcor.csv", stringsAsFactors=FALSE)
UKcor <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Country/United Kingdom/UKcor.csv", stringsAsFactors=FALSE)
# Install new packages
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
#Create corpus
corpus = Corpus(VectorSource(JP$Title))
corpus = Corpus(VectorSource(JPcorr$Title))
corpus = Corpus(VectorSource(UScor$Title))
corpus = Corpus(VectorSource(FRcor$Title))
corpus = Corpus(VectorSource(SKcor$Title))
corpus = Corpus(VectorSource(UKcor$Title))
# Look at corpus
corpus
corpus[[1]]

# Stem document 
corpus <- tm_map(corpus, stemDocument, language = "english") 
corpus[[1]]

# Convert to lower-case
corpus = tm_map(corpus, tolower)
corpus[[1]]

# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

# Look at stop words 
stopwords("english")[1:10]
# Remove stopwords 
corpus = tm_map(corpus, removeWords, c( stopwords("english")))
corpus[[1]]
# Create matrix
corpus <- Corpus(VectorSource(corpus))
frequencies = DocumentTermMatrix(corpus)
frequencies 

# Look at matrix 
inspect(frequencies[1000:1005,505:515])
# Check for sparsity
findFreqTerms(frequencies, lowfreq=98)
findFreqTerms(frequencies, highfreq=98)
findFreqTerms(frequencies, 4,10)# for Viet and Nam separated

# Remove sparse terms
sparse = removeSparseTerms(frequencies, 0.97)
sparse
# Convert to a data frame
CountrySparse = as.data.frame(as.matrix(sparse))
summary(CountrySparse)
str(CountrySparse)
# Make all variable names R-friendly
colnames(CountrySparse) = make.names(colnames(CountrySparse))

#Results: for US the word Vietnam (488) or Viet nam (47) in title, account for 28.8%


# Field collaboration

#Rate of Vietnam/Japan corresponding authors 

MED <- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Medicine 24Dec/Medfrom08.csv")
#school computer
MED <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/Medicine 24Dec/Medfrom08.csv", na.strings=0)
MED <- read.csv("C:/Users/Manh/Desktop/Dropbox/My research/Scientific outputs in VN/Data/AgriBio/AgriBio.csv", na.strings=0)

#nrow(subset(JP,JP$Year<2005 & JP$Year>1995))
#nrow(subset(JP,JP$Year>2004))

Med<-data.frame(table(MED$Year))
Med
# Build a function to find correspoding author's country (no need more)
jpcorr<-NULL
vncorr<-NULL

for (i in 1:length(Med$Var1)) {
        sub<-subset(MED,MED$Year==Med[i,1])
        # Japan corresponding author
        #num<-length(grep("Japan",sub$Correspondence.Address))
        #jpcorr<-c(jpcorr,num)
        # Vietnam corresponding author
        vnnum<-length(grep("Viet Nam",sub$Correspondence.Address))
        vncorr<-c(vncorr,vnnum)
        
}
#jpcorr
#jp$jp<-jpcorr
Med$vn<-vncorr
Med$perVn<-round(Med$vn*100/Med$Freq,1)
#jp
colnames(Med)<-c("year","total","vietnam")
write.csv(jp,"jpcoll.csv")





#6. Coressponding by country and time
corres <- read.csv("C:/Users/MANH/Dropbox/My research/Scientific outputs in VN/countrycoressponding.csv")
percorres <- read.csv("C:/Users/MANH/Dropbox/My research/Scientific outputs in VN/percorres.csv")

library(reshape2)
correspond<-melt(corres,id.vars=c("year"))
percorrespond<-melt(percorres,id.vars=c("year"))

library(ggplot2)
#graph1
g<-ggplot(correspond,aes(x=year,y=value,shape=variable,color=variable))
g+geom_point(size=5)+theme_bw()+
        geom_line(lwd=1)+theme(legend.position = c(0.2, 0.7))+
        theme(legend.title = element_blank())+#remove legend title
        theme(legend.text = element_text(size = 15, face = 'bold'))+
        xlab("Year")+ylab("Number of papers from foreign corresponding authors")+
        theme(title=element_text(size=17))+
        theme(axis.text=element_text(size=13))+
        theme(axis.)

#percent of corresponding author from VN with other countries.

g1<-ggplot(percorrespond,aes(x=year,y=value,shape=variable,color=variable))
g1+geom_point(size=5)+theme_bw()+
        geom_line(lwd=1)+theme(legend.position = c(0.4, 0.8))+
        theme(legend.title = element_blank())+#remove legend title
        theme(legend.text = element_text(size = 15, face = 'bold'))+
        xlab("Year")+ylab("Percentage from foreign corresponding authors")+
        theme(title=element_text(size=17))+
        theme(axis.text=element_text(size=13))

g2<-ggplot(percorrespond,aes(x=variable,y=value, color=variable))
g2+geom_boxplot()+
        ggtitle("Percentage average of corresponding author from Vietnam")+
        xlab("")+ylab("percentage of Vietnam")+
        theme_bw()+theme(legend.position="NONE")+
        theme(axis.title=element_text(size=15))+
             

summary(percorres)       




GER<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Germany/GER.csv")
AU<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Australia/AU.csv")
NETH<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Nertherland/NETH.csv")
BEL<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Belgium/BEL.csv")
SW<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Sweden/SW.csv")
RUS<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Russia/RUS.csv")
TAI<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Taiwan/TAI.csv")
ITA<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Italy/ITA.csv")
SWI<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Switzeland/SWI.csv")
CA<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Canada/CA.csv")
IND<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/India/IND.csv")
DEN<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Denmark/DEN.csv")
JP<- read.csv("C:/Users/Manh/Dropbox/My research/Scientific outputs in VN/Data/Country/Poland/PO.csv")
