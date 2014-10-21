mydata<- read.table("readme.md",
                   skip=4,header=TRUE, sep="\t")

###ORIGINAL CODE
#Finds the distance values associated with the maximum force in the plot, which is the area in which I am interested

Weg_Values_at_Fmax <- weg[which(kraft == max(kraft))]


#the next sets of lines initiate the values which will be changed in the while loop
#one to zero, the other to a range of distance values on either side of the distance #values associated with the maximum force
n <- 0

Weg.index <- which((weg >= Weg_Values_at_Fmax  - 
                      Weg_Values_at_Fmax *(1/7)  + n) & 
                     weg <= (Weg_Values_at_Fmax  + 
                               Weg_Values_at_Fmax*(1/7) - n))      

#finally the while loop which increase n (thereby decreasing Weg.index) until the #Weg.index falls below a certain value, in this case 80 points #                                                                                                                                                                     
while(length(Weg.index) > 80){
  
  n <- n + .0005
  Weg.index <- which((weg >= Weg_Values_at_Fmax  - 
                        Weg_Values_at_Fmax *(1/6)  + n) & 
                       weg <= (Weg_Values_at_Fmax +
                                 Weg_Values_at_Fmax *(1/6) - n))
  
}


plot(weg, kraft, 
     xlim=c(weg[Weg.index[1]], weg[Weg.index[length(Weg.index)]]), 
     ylim=c(min(kraft[Weg.index[1:length(Weg.index)]]),
            max(kraft[Weg.index[1:length(Weg.index)]])),
     main = "file")


### vpipkt SOLUTION ##########################################################

## want the graph to contain 80 points around the peak or any arbitrary kraft value.

plot.points=80

#index of row to highlight
spotlight.kraft <-   max(mydata$Kraft_N)
spotlight <- which(mydata$Kraft_N == spotlight.kraft)[1]

#compute percentile of weg (x-axis) for the spotlight point
x.p = c(ecdf(mydata$Weg_mm)(mydata$Weg_mm[spotlight]) - 0.5 *plot.points/nrow(mydata),
          ecdf(mydata$Weg_mm)(mydata$Weg_mm[spotlight]) + 0.5 *plot.points/nrow(mydata))

if(x.p[1]<0) x.p <- x.p -x.p[1]
if(x.p[2]>1) x.p <- x.p - x.p[2] + 1

xlims <- quantile(mydata$Weg_mm,x.p)
#possibly not exactly 80 points shown
nrow(subset(mydata, Weg_mm > xlims[1] & Weg_mm<xlims[2]))
#get corresponding kraft values
ylims <- range(subset(mydata, Weg_mm > xlims[1] & Weg_mm<xlims[2])$Kraft_N)
 
plot(weg, kraft,  xlim=xlims, ylim=ylims)
