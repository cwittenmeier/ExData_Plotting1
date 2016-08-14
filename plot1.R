#Function reads in the data from the file.
readData <- function(){
        read.table("household_power_consumption.txt",sep=";",head=TRUE)
}

#Filters the data for the two dates, that are relevant.
filterPData <- function(data){
        
        date=as.Date(data$Date,format="%d/%m/%Y")
        begin=as.Date("2007-02-01",format="%Y-%m-%d")
        end=as.Date("2007-02-02",format="%Y-%m-%d")
        vec1=date==begin
        vec2=date==end
        vec3=vec1|vec2
        subset(data,vec3)
        
}

#Convert the factor to numeric
makeNum <-function(vec){
        as.numeric(as.character(vec))
}

# Generates plot1.png

plot1 <- function(){
        
        orig <- readData()
        filter<-filterPData(orig)
        
        vec <-makeNum(filter$Global_active_power)
        
        png(file="plot1.png",height=480, width=480)
        hist(vec,breaks=12, col="red",main="Global Active Power", xlab="Global Active Power (kilowatts)")
        dev.off()
}