library(shiny)
library(lattice)
library(ggplot2)
shinyServer(function(input, output) {
 
  St <- reactive({
    input$update
    ElecCost<-796711000*(1-input$cut)
    NatGasCost<-151058000*(1-input$cut)
    MiscCost<-298124000*(1-input$cut)
    WaterCost<-69500000*(1-input$cut)
    Reimb<-(-97400000)
    Labor<-46200000
    ESPCs<-50000000
    OAltF<-131200000-ESPCs
    UPCost<-197500000
    Installation<-6000000000
    # Reactive expression to compose a data frame containing all of the values
    
    isolate({
  Elec<-rnorm(10000,mean=input$decimal1.1,sd=.016)
  NatGas<-rnorm(10000,mean=input$decimal1.2,sd=.1218)
  Misc<-rnorm(10000,mean=input$decimal1.3,sd=.0331)
  Water<-rnorm(10000,mean=input$decimal2,sd=.0093)
  Reimbursment<-rnorm(10000,mean=input$decimal3,sd=.0093)
  Manpower<-rnorm(10000,mean=input$decimal4,sd=.0093)
  #no growth model
  ESPC<-rnorm(10000,mean=input$decimal1,sd=.0143)
  O.AltFin<-rnorm(10000,mean=input$decimal5,sd=.0143)
  UP<-rnorm(10000,mean=input$decimal6,sd=.0093)
  Inflation<-rnorm(10000,mean=input$decimal7,sd=.0093)
    })
  n<-1
  Energy<-ElecCost*(1+Elec)^n+NatGasCost*(1+NatGas)^n+MiscCost*(1+Misc)^n
  EnergyBudget<-Energy+WaterCost*(1+Water)^n+Reimb*(1+Reimbursment)^n+
    Labor*Manpower+ESPCs*(1+ESPC)^n+OAltF*(1+O.AltFin)^n+UPCost*(1+UP)^n
  InstBudget<-Installation*(1+Inflation)^n
  Saturation1<-EnergyBudget/InstBudget
  
  Saturation<-data.frame(Sat=Saturation1,Year=n)
  
  n<-3
  Energy<-ElecCost*(1+Elec)^n+NatGasCost*(1+NatGas)^n+MiscCost*(1+Misc)^n
  EnergyBudget<-Energy+WaterCost*(1+Water)^n+Reimb*(1+Reimbursment)^n+
    Labor*Manpower+ESPCs*(1+ESPC)^n+OAltF*(1+O.AltFin)^n+UPCost*(1+UP)^n
  InstBudget<-Installation*(1+Inflation)^n
  Saturation3<-EnergyBudget/InstBudget
  Saturation3<-data.frame(Sat=Saturation3,Year=n)
  
  n<-5
  Energy<-ElecCost*(1+Elec)^n+NatGasCost*(1+NatGas)^n+MiscCost*(1+Misc)^n
  EnergyBudget<-Energy+WaterCost*(1+Water)^n+Reimb*(1+Reimbursment)^n+
    Labor*Manpower+ESPCs*(1+ESPC)^n+OAltF*(1+O.AltFin)^n+UPCost*(1+UP)^n
  InstBudget<-Installation*(1+Inflation)^n
  Saturation5<-EnergyBudget/InstBudget
  Saturation5<-data.frame(Sat=Saturation5,Year=n)
  
  n<-10
  Energy<-ElecCost*(1+Elec)^n+NatGasCost*(1+NatGas)^n+MiscCost*(1+Misc)^n
  EnergyBudget<-Energy+WaterCost*(1+Water)^n+Reimb*(1+Reimbursment)^n+
    Labor*Manpower+ESPCs*(1+ESPC)^n+OAltF*(1+O.AltFin)^n+UPCost*(1+UP)^n
  InstBudget<-Installation*(1+Inflation)^n
  Saturation10<-EnergyBudget/InstBudget
  Saturation10<-data.frame(Sat=Saturation10,Year=n)
  
  Saturation<-rbind(Saturation,Saturation3,Saturation5,Saturation10)
  Saturation$Year<-factor(Saturation$Year)
  return(Saturation)
  })
    # Show the values using an HTML table
    output$values <- renderPlot({
      Saturation<-St()
      print(ggplot(Saturation, aes(x=Sat))+
            geom_histogram(fill="red",)+
        facet_grid(Year~.))
      })  
output$probs<-renderTable({
  Saturation<-St()
  Hold<-Saturation[Saturation$Year==1,]
  my.cdf<-ecdf(Hold$Sat)
  a<-1-my.cdf(.2)
  b<-1-my.cdf(.25)
  c<-1-my.cdf(.3)
  d<-1-my.cdf(.35)
  new.tab<-data.frame(c(a,b,c,d))
  colnames(new.tab)<-c("YearOne")
  rownames(new.tab)<-c("Greater Than 20%","Greater Than 25%","Greater Than 30%","Greater Than 35%")
  Hold<-Saturation[Saturation$Year==3,]
  my.cdf<-ecdf(Hold$Sat)
  a<-1-my.cdf(.2)
  b<-1-my.cdf(.25)
  c<-1-my.cdf(.3)
  d<-1-my.cdf(.35)
  new.tab$YearThree<-c(a,b,c,d)
  Hold<-Saturation[Saturation$Year==5,]
  my.cdf<-ecdf(Hold$Sat)
  a<-1-my.cdf(.2)
  b<-1-my.cdf(.25)
  c<-1-my.cdf(.3)
  d<-1-my.cdf(.35)
  new.tab$YearFive<-c(a,b,c,d)
  Hold<-Saturation[Saturation$Year==10,]
  my.cdf<-ecdf(Hold$Sat)
  a<-1-my.cdf(.2)
  b<-1-my.cdf(.25)
  c<-1-my.cdf(.3)
  d<-1-my.cdf(.35)
  new.tab$YearTen<-c(a,b,c,d)
  
  print(new.tab)
})
output$current<-renderTable({
  input$update
  ElecCost<-796711000*(1-input$cut)
  NatGasCost<-151058000*(1-input$cut)
  MiscCost<-298124000*(1-input$cut)
  WaterCost<-69500000*(1-input$cut)
  Reimb<-(-97400000)
  Labor<-46200000
  ESPCs<-50000000
  OAltF<-131200000-ESPCs
  UPCost<-197500000
  Installation<-6000000000
  x<- (ElecCost+NatGasCost+MiscCost+WaterCost+Reimb+Labor+OAltF+UPCost+ESPCs)/Installation
  new.tab<-data.frame(x)
  colnames(new.tab)<-c("")
  rownames(new.tab)<-c("Current Saturation Level")
  print(new.tab)
})

growth <- reactive({  
  ElecCost<-796711000*(1-input$cut)
  NatGasCost<-151058000*(1-input$cut)
  MiscCost<-298124000*(1-input$cut)
  WaterCost<-69500000*(1-input$cut)
  Reimb<-(-97400000)
  Labor<-46200000
  ESPCs<-50000000
  OAltF<-131200000-ESPCs
  UPCost<-197500000
  Installation<-6000000000
  x<- (ElecCost+NatGasCost+MiscCost+WaterCost+Reimb+Labor+OAltF+UPCost+ESPCs)
  growth<-data.frame(Year=0,Total=x,Elec=ElecCost,NatGas=NatGasCost,Misc=MiscCost,Water=WaterCost,
                     Labor=Labor,Reimb=Reimb,ESPC=ESPCs,
                     O.AltFin=OAltF,UtilPriv=UPCost)
  
  ElecCost<-ElecCost*(1+input$decimal1.1)^1
  NatGasCost<-NatGasCost*(1+input$decimal1.2)^1
  MiscCost<-MiscCost*(1+input$decimal1.3)^1
  WaterCost<-WaterCost*(1+input$decimal2)^1
  Reimb<-Reimb*(1+input$decimal3)^1
  Labor<-Labor*(1+input$decimal4)^1
  ESPCs<-ESPCs*(1+input$decimal1)^1
  OAltF<-OAltF*(1+input$decimal5)^1
  UPCost<-UPCost*(1+input$decimal6)^1
 
  x<- (ElecCost+NatGasCost+MiscCost+WaterCost+Reimb+Labor+OAltF+UPCost+ESPCs)
  growth1<-data.frame(Year=1,Total=x,Elec=ElecCost,NatGas=NatGasCost,Misc=MiscCost,Water=WaterCost,
                      Labor=Labor,Reimb=Reimb,ESPC=ESPCs,
                     O.AltFin=OAltF,UtilPriv=UPCost)
  
  ElecCost<-ElecCost*(1+input$decimal1.1)^2
  NatGasCost<-NatGasCost*(1+input$decimal1.2)^2
  MiscCost<-MiscCost*(1+input$decimal1.3)^2
  WaterCost<-WaterCost*(1+input$decimal2)^2
  Reimb<-Reimb*(1+input$decimal3)^2
  Labor<-Labor*(1+input$decimal4)^2
  ESPCs<-ESPCs*(1+input$decimal1)^2
  OAltF<-OAltF*(1+input$decimal5)^2
  UPCost<-UPCost*(1+input$decimal6)^2
  x<- (ElecCost+NatGasCost+MiscCost+WaterCost+Reimb+Labor+OAltF+UPCost+ESPCs)
  growth3<-data.frame(Year=3,Total=x,Elec=ElecCost,NatGas=NatGasCost,Misc=MiscCost,Water=WaterCost,
                      Labor=Labor,Reimb=Reimb,ESPC=ESPCs,
                     O.AltFin=OAltF,UtilPriv=UPCost)
  
  ElecCost<-ElecCost*(1+input$decimal1.1)^2
  NatGasCost<-NatGasCost*(1+input$decimal1.2)^2
  MiscCost<-MiscCost*(1+input$decimal1.3)^2
  WaterCost<-WaterCost*(1+input$decimal2)^2
  Reimb<-Reimb*(1+input$decimal3)^2
  Labor<-Labor*(1+input$decimal4)^2
  ESPCs<-ESPCs*(1+input$decimal1)^2
  OAltF<-OAltF*(1+input$decimal5)^2
  UPCost<-UPCost*(1+input$decimal6)^2
  x<- (ElecCost+NatGasCost+MiscCost+WaterCost+Reimb+Labor+OAltF+UPCost+ESPCs)
  growth5<-data.frame(Year=5,Total=x,Elec=ElecCost,NatGas=NatGasCost,Misc=MiscCost,Water=WaterCost,
                      Labor=Labor,Reimb=Reimb,ESPC=ESPCs,
                     O.AltFin=OAltF,UtilPriv=UPCost)
  
  ElecCost<-ElecCost*(1+input$decimal1.1)^5
  NatGasCost<-NatGasCost*(1+input$decimal1.2)^5
  MiscCost<-MiscCost*(1+input$decimal1.3)^5
  WaterCost<-WaterCost*(1+input$decimal2)^5
  Reimb<-Reimb*(1+input$decimal3)^5
  Labor<-Labor*(1+input$decimal4)^5
  ESPCs<-ESPCs*(1+input$decimal1)^5
  OAltF<-OAltF*(1+input$decimal5)^5
  UPCost<-UPCost*(1+input$decimal6)^5
  x<- (ElecCost+NatGasCost+MiscCost+WaterCost+Reimb+Labor+OAltF+UPCost+ESPCs)
  growth10<-data.frame(Year=10,Total=x,Elec=ElecCost,NatGas=NatGasCost,Misc=MiscCost,Water=WaterCost,
                       Labor=Labor,Reimb=Reimb,ESPC=ESPCs,
                      O.AltFin=OAltF,UtilPriv=UPCost)
  print(growth10)
  growth<-rbind(growth,growth1,growth3,growth5,growth10)
  growth<-(growth/1000000)
  growth$Year<-growth$Year*1000000
  
  return(growth)})

output$costgrowth<-renderPlot({
growth<-growth()
input$update
isolate({  
print({ plot(growth$Total,type="b",lwd=2,
              xaxt="n",col="black",
              xlab="Year",ylim=c(0,2000),ylab="Cost Growth (in Million's)",
              main="Utilites Cost Growth Over Next Ten Years")
         axis(1,at=1:length(growth$Year),labels=growth$Year)
         lines(growth$Elec,col="red",type="b",lwd=2)
         lines(growth$NatGas,col="orange",type="b",lwd=2)
         lines(growth$Misc,col="purple",type="b",lwd=2)
         
         legend("topright",legend=c("Total Utilites Cost (QDPW)","Electricity","Natural Gas","Misc Utlities"),
                lty=1,lwd=2,pch=21,col=c("black","red","orange","purple"),
                ncol=2,bty="n",cex=0.8,
                text.col=c("black","red","orange","purple"),
                inset=0.01)
         
         
         
  })

})})
output$othercostgrowth<-renderPlot({
  growth<-growth()
  input$update
  isolate({
  print({ plot(growth$ESPC,type="b",lwd=2,
               xaxt="n",col="black",
               xlab="Year",ylim=c(-120,300),ylab="Cost Growth (in Million's)",
               main="Other Utilites Related Cost Growth Over Next Ten Years")
          axis(1,at=1:length(growth$Year),labels=growth$Year)
          lines(growth$Labor,col="red",type="b",lwd=2)
          lines(growth$Reimb,col="orange",type="b",lwd=2)
          lines(growth$Water,col="purple",type="b",lwd=2)
          lines(growth$O.AltFin,col="grey",type="b",lwd=2)
          lines(growth$UtilPriv,col="green3",type="b",lwd=2)
          
          legend("topright",legend=c("ESPCs","Labor","Reimbursement","Water",
                                     "Other Alternative Finance","Utilities Privatization"),
                 lty=1,lwd=2,pch=21,col=c("black","red","orange","purple","grey","green3"),
                 ncol=2,bty="n",cex=0.8,
                 text.col=c("black","red","orange","purple","grey","green3"),
                 inset=0.01)
          
          
          
  })
  
})})
  })