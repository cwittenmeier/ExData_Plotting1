
#Function reads in the data from the file.
readData <- function(){
        read.table("household_power_consumption.txt",sep=";",head=TRUE)
}

#Filters the data for the relevant dates.
filterPData <- function(data){
        
        date=as.Date(data$Date,format="%d/%m/%Y")
        begin=as.Date("2007-02-01",format="%Y-%m-%d")
        end=as.Date("2007-02-02",format="%Y-%m-%d")
        vec1=date==begin
        vec2=date==end
        vec3=vec1|vec2
        subset(data,vec3)
        
} 

#Converts the values to numeric
makeNum <-function(vec){
        as.numeric(as.character(vec))
}


#Calulates the time in seconds for plotting
ctime <-function(date, time){
        
        l<-length(date)
        result <- vector("character",length=0)
        for(i in 1:l){
                
                dat<-as.character(date[i])
                tm<-as.character(time[i])
                con<-paste(dat,",",tm)
                res <-gsub(" ","",con)
                parse<-strptime(res,"%d/%m/%Y,%H:%M:%S")
                result<-c(result,as.POSIXct(parse))
                
        }
        result
        
}

#Generates plot3.png
plot3 <-function(){
        
        
        orig <- readData()
        filter<-filterPData(orig)
        
        sub1 <-makeNum(filter$Sub_metering_1)
        
        sub2 <-makeNum(filter$Sub_metering_2)
        
        sub3 <-filter$Sub_metering_3
        
        ct<-as.numeric(ctime(filter$Date,filter$Time))
        
        thursday<-strptime("01/02/2007,00:00:00","%d/%m/%Y,%H:%M:%S")
        thursday<-unclass(as.POSIXct(thursday))
        
        friday<-strptime("02/02/2007,00:00:00","%d/%m/%Y,%H:%M:%S")
        friday<-unclass(as.POSIXct(friday))
        
        saturday<-strptime("03/02/2007,00:00:00","%d/%m/%Y,%H:%M:%S")
        saturday<-unclass(as.POSIXct(saturday))
        
        png(file="plot3.png", height=480, width=480)
        
        plot(ct,sub1,xaxt="n",type="l",xlab="",ylab="Energy sub metering", yaxt="n")
        axis(1,at=c(thursday,friday,saturday),c("Thu","Fri","Sat"))
        axis(2,at=c(0,10,20,30),c(0,10,20,30))
        lines(ct,sub2,col="red")
        lines(ct,sub3,col="blue")
        legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),col=c("black","red","blue"))
        dev.off()
        
}