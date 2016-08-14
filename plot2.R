
#Function reads in the data from the file.
readData <- function(){
        read.table("household_power_consumption.txt",sep=";",head=TRUE)
}

# Filters the data for the two dates, that are relevant.
filterPData <- function(data){
        
        date=as.Date(data$Date,format="%d/%m/%Y")
        begin=as.Date("2007-02-01",format="%Y-%m-%d")
        end=as.Date("2007-02-02",format="%Y-%m-%d")
        vec1=date==begin
        vec2=date==end
        vec3=vec1|vec2
        subset(data,vec3)
        
} 

#Converts and scales the values
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

#Generates plot2.png
plot2 <- function(){
        
        orig <- readData()
        filter<-filterPData(orig)
        vecnum=makeNum(filter$Global_active_power)
        
        ct<-as.numeric(ctime(filter$Date,filter$Time))
        
        thursday<-strptime("01/02/2007,00:00:00","%d/%m/%Y,%H:%M:%S")
        thursday<-unclass(as.POSIXct(thursday))
        
        friday<-strptime("02/02/2007,00:00:00","%d/%m/%Y,%H:%M:%S")
        friday<-unclass(as.POSIXct(friday))
        
        saturday<-strptime("03/02/2007,00:00:00","%d/%m/%Y,%H:%M:%S")
        saturday<-unclass(as.POSIXct(saturday))
        
        png(file="plot2.png", height=480, width=480)
        
        plot(ct,vecnum,xaxt="n",type="l",xlab="",ylab="Global Active Power (kilowatts)")
        axis(1,at=c(thursday,friday,saturday),c("Thu","Fri","Sat"))
        dev.off()
}
