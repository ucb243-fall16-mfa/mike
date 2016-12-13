# server2. AFM
source("bootstrap.R")
source("contributions.R")
source("GSVD.R")
source("Lg_table.R")
source("Lg.R")
source("MFA.R")
source("plot.R")
source("Rv_table.R")
source("Rv.R")
source("summary.R")
library(ggplot2)
wines<-read.csv("wines.csv")
X<-as.matrix(wines[1:12,2:54])

shinyServer(
  function(input, output) {
    
    values=reactive({
      nbfreq=0
      nbquali=0
      nbquanti=0
      quantisup=0
      listquali=c()
      gsup=c()
      dataselec=x[,input$variables1]
      groupe=length(input$variables1)
      nbgroupe=1
      
      
      insets <- input$insets
      sets=list()
      for (i in 1:length(insets))
        sets[i] = gsets[ as.numeric(insets[i]) ]
      ncomp=input$inncomp
      center=input$incenter
      scale=input$inscale
      if(is.numeric(ncomp))
      MFA<- mfa(X,sets,ncomp,center,scale)
      else MFA<-mfa(X,sets,NULL,center=center,scale=scale)
      contrib1 <- contributions(X,sets,1)
      contrib2 <- contributions(X,sets,2)
      contrib3 <- contributions(X,sets,3)
      list(res.MFA=MFA,res.insets=insets,res.sets=sets,res.ctb1=contrib1,res.ctb2=contrib2,res.ctb3=contrib3)
    })
    
    error=function(){
      if (TRUE) { ##if(length(input$variables1)!=0 && length(input$variables2)!=0){
        etat="ok"
      }
      else{
        etat="not"
      }
      return(etat)
    }
    
    # scatterplot of the common factor scores
    Plot1 <- function(){
      figdm <- values()$res.MFA$MatrixF
      x<- figdm[,1]
      y<- figdm[,2]
      a<-data.frame(x,y)
      a$place=as.factor(substr(glabels,1,2))
      a$number=substr(glabels,3,3)
      p=ggplot(a,aes(x=x,y=y,color=place))+geom_point(size=3.8)+geom_text(aes(label = number, vjust = 1.1, hjust = -0.5))
      p <- p+theme(plot.title = element_text(size=20, face="bold", margin=margin(10,0,10,0)))
      p <- p+ggtitle("scatterplot of the common factor scores")
      
      print(p)
    }
    
    output$map <- renderPlot({
      p <- Plot1()
      
    })
    
    output$map2 <- renderPlot({
      
      if(input$colorgroup==TRUE){
        habi="group"
      }
      if(input$colorgroup==FALSE){
        habi="none"
      }
      if(input$selection==gettext("No selection")){
        selec=NULL
      }
      if(input$selection=="contrib"){
        selec=paste("contrib ",input$slider2)
      }
      if(input$selection=="cos2"){
        if(input$slider3!=1){
          selec=paste("cos2 ",input$slider3)
        }
        else{
          selec="cos2 0.999"
        }
      }
      #      invi="none"
      if(is.null(input$hides)){
        invi="none"
      }else{
        if (input$hides==gettext("Nothing")) invi="none"
        if (input$hides==gettext("Active variables")) invi="quanti"
        if (input$hides==gettext("Supplementary variables")) invi="quanti.sup"
      }
      # if(!is.null(input$hides)){
      # validate(need(!(input$hides==gettext("Active variables")&&values()$QUANTISUP==1),"Impossible with only one supplementary group"))
      # }
      plot.MFA(values()$res.MFA,choix="var",axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),title=input$title3,habillage=habi,select=selec,invisible=invi)
    })
    

    Plot5 <- function(){
      mt <- values()$res.MFA$MatrixA
      
      dt = data.frame(comp=rownames(mt), val=mt[,1])
      p=ggplot(data=dt, aes(x=comp, y=val)) + geom_bar(stat="identity")
      
      p <- p+theme(plot.title = element_text(size=20, face="bold", margin=margin(10,0,10,0)))
      p <- p+ggtitle("chart of eigenvalues")
      print(p)
    }
    
    output$map5 <- renderPlot({
      p <- Plot5()
    })
    
    # scatterplot of the partial factors scores
    Plot4 <- function(){
      nrows <- dim(values()$res.MFA$MatrixEFG[[1]])[1]
      fig <- matrix(unlist(values()$res.MFA$MatrixEFG), ncol=2, byrow=TRUE)
      fig <- round(fig,2)
      colass = c()  # column assessor
      colplc = c()  # column place
      for (i in 1:length(input$insets)) {
        colass <- c(colass, rep( sprintf("%02d",as.numeric(input$insets[i])),nrows))
        colplc <- c(colplc, gplaces)
      }
      figd <- cbind(fig, colass, colplc)
      
      colnames(figd) <- c("axis1","axis2","assessor","place")
      
      fig3d <- as.data.frame(figd)
      p <- ggplot(data=fig3d, aes(x=axis1,y=axis2,color=place)) + geom_point() + facet_wrap(~ assessor, ncol=5)
      p <- p+theme(plot.title = element_text(size=20, face="bold", margin=margin(10,0,10,0)))
      p <- p+ggtitle("Scatterplot of the partial factors scores")
      #aes, axis.text.y=element_blank()
      p <- p+scale_y_discrete(breaks=seq(-1,1,0.05))+scale_x_discrete(breaks=seq(-1,1,0.05))
      #p <- p+theme(axis.text.x=element_blank(),axis.ticks.x = element_blank())
      print(p)
    }
    
    output$map4 <- renderPlot({
      p <- Plot4()
    })
    ## scatterplot of loadings 
    Plot7 <- function(){
      ecol=c()
      plsets <- values()$res.sets
      for (i in 1:length(plsets)) {
        assessor <- as.numeric(values()$res.insets[i])
        ecol <- c(ecol, rep(assessor,length(plsets[i])))
      }
      figd <- cbind(values()$res.MFA$MatrixV, ecol)
      dim2 <- dim(values()$res.MFA$MatrixV)[2] + 1
      colnames(figd) <- seq(1,dim2)
      colnames(figd)[1] <- "Comp1"
      colnames(figd)[2] <- "Comp2"
      colnames(figd)[dim2] <- "col"
      fig3d <- as.data.frame(figd)
      print(plsets)
      print(fig3d)
      # aes,color=as.factor(col)
      p <- ggplot(data=fig3d, aes(Comp1,Comp2,color=as.factor(col))) + geom_point(size=3.8)
      p <- p+theme(plot.title = element_text(size=20, face="bold", margin=margin(10,0,10,0)))
      p <- p+ggtitle("Scatterplot of the loadings")
      print(p)
      
    }
    output$map22 <- renderPlot({
      p <- Plot7()
    })
    
    output$plot <- renderPlot({
      switch(input$pType, "eigenvalues"= Plot5(), 
             "common factor scores" = Plot1(),
             "partial factor scores" = Plot4(),
             "loadings" = Plot7())
    })
    
    output$drawindiv=renderUI({
      if(input$choixpartial==gettext("None")){
        #        return(radioButtons("drawind",gettext("Drawing by"),choices=list("No selection"= "a","By individual"="b","By categorical variable"="c"),inline=TRUE))
        return(radioButtons("drawind",gettext("Drawing by"),choices=list(gettext("No selection"),gettext("individual"),gettext("categorical variable")),inline=TRUE))
      }
      else{
        #       return(radioButtons("drawind",gettext("Drawing by"),choices=list("By individual"= "a","By group"="b","By categorical variable"="c"),inline=TRUE))
        return(radioButtons("drawind",gettext("Drawing by"),choices=list(gettext("individual"),gettext("group"),gettext("categorical variable")),inline=TRUE))
      }
    })
    
    
    
    output$habillagequali=renderUI({
      if(input$activeG2==TRUE && length(input$variables2)>0){
        if(!(is.null(values()$LISTQUALI))){
          choix=values()$LISTQUALI
          if(length(choix)==1){
            return(selectInput("habiquali"," ",choices=choix))
          }
          else{
            num=c(1:length(choix))
            return(selectInput("habiquali"," ",choices=list(num=choix)))
          }
        }
      }
      else{
        p(gettext("No groups of categorical variables"))
      }
    })
    
    
    ### Recuperation des parametres
    observe({
      if(input$Quit==0){
      }
      else{
        isolate({
          stopApp(returnValue=valeuretour())
        })
      }
    })
    
    valeuretour=function(){
      res=list()
      res$code=values()$res.MFA
      res$axe1=input$nb1
      res$axe2=input$nb2
      res$ind1=input$meanind1
      res$ind2=input$meanind
      res$ind3=input$qualind1
      res$ind4=input$qualind
      res$drawing=input$drawind
      res$drawing2=input$habiquali
      res$partial=input$choixpartial
      res$partial2=input$indivpartiel
      res$partial3=input$partind
      res$selectvar=input$selection
      sel=NULL
      if(input$selection=="contrib"){
        sel=input$slider2
      }
      if(input$selection=="cos2"){
        sel=input$slider3
      }
      res$selectvar2=sel
      res$hide=input$hides
      res$colorvar=input$colorgroup
      res$freq1=input$affichind
      res$freq2=input$affichcol
      res$partaxe=input$coloraxe
      res$nom=nomData
      res$code2=codeGraph2()
      res$code3=codeGraph3()
      res$code4=codeGraph4()
      res$code5=codeGraph5()
      res$title1=input$title1
      res$title2=input$title2
      res$title3=input$title3
      res$title4=input$title4
      res$title5=input$title5
      class(res)="MFAshiny"
      return(res)
    }
    
    output$slider1=renderUI({
      etat2=error()
      validate(
        need(etat2!="not"," ")
      )
      maxlength=dim(values()$res.MFA$quanti.var$coord)[1]
      if(input$selection=="contrib"){
        return(sliderInput("slider2",gettext("Number of the most contributive variables"),min=1, max=maxlength, value=maxlength, step=1))
      }
      if(input$selection=="cos2"){
        return(sliderInput("slider3",gettext("Number of variables with highest cos2"),min=0, max=maxlength, value=maxlength, step=1))
      }
    })
    

    output$sorties=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$eig))
    })
    
    output$map3=renderPlot({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(barplot(values()$res.MFA$eig[,1],names.arg=rownames(values()$res.MFA$eig),las=2))
    })
    output$JDD=renderDataTable({
      cbind(Names=rownames(x),x)},
      options = list(    "orderClasses" = TRUE,
                         "responsive" = TRUE,
                         "pageLength" = 10))
    output$summary=renderPrint({
      summary(x)
    })
    output$summaryMFA=renderPrint({
      ##summary.MFA(values()$res.MFA)
      print(values()$res.MFA)
    })  
    output$compromise=renderPrint({
      ##summary.MFA(values()$res.MFA)
      print(values()$res.MFA$MatrixF)
    })
    output$partial=renderPrint({
      ##summary.MFA(values()$res.MFA)
      print(values()$res.MFA$MatrixEFG)
    })
    
    output$histo=renderPlot({
      par(mfrow=c(1,2))
      boxplot(x[,input$bam])
      plot(density(x[,input$bam]),main="",xlab="")
    })
    
    
    output$downloadData = downloadHandler(
      filename = function() { 
        paste('graph1','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot1()
        dev.off()
      },
      contentType='image/png')
    
    output$downloadData3 = downloadHandler(
      filename = function() { 
        paste('graph2','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot2()
        dev.off()
      },
      contentType='image/png')
    
    output$download3=renderUI({
      etat2=error()
      validate(
        need(etat2!="not","")
      )
      if(values()$NBQUANTI==0){
        return()
      }
      else{
        return(downloadButton("downloadData3",gettext("Download as png")))
      }
    })
    
    output$downloadData11 = downloadHandler(
      filename = function() { 
        paste('graph3','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot5()
        dev.off()
      },
      contentType='image/png')
    
    output$downloadData12 = downloadHandler(
      filename = function() { 
        paste('graph3','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot5()
        dev.off()
      },
      contentType='image/jpg')
    
    output$downloadData13 = downloadHandler(
      filename = function() { 
        paste('graph3','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot5()
        dev.off()
      },
      contentType=NA)
    
    
    output$downloadData15 = downloadHandler(
      filename = function() { 
        paste('graph4','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot4()
        dev.off()
      },
      contentType='image/png')
    
    output$downloadData16 = downloadHandler(
      filename = function() { 
        paste('graph4','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot4()
        dev.off()
      },
      contentType='image/jpg')
    
    output$downloadData17 = downloadHandler(
      filename = function() { 
        paste('graph4','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot4()
        dev.off()
      },
      contentType=NA)
    
    
    output$downloadData19 = downloadHandler(
      filename = function() { 
        paste('graph5','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot6()
        dev.off()
      },
      contentType='image/png')
    
    output$download19=renderUI({
      etat2=error()
      validate(
        need(etat2!="not","")
      )
      if(values()$NBFREQ==0){
        return()
      }
      else{
        return(downloadButton("downloadData19",gettext("Download as png")))
      }
    })
    
    output$downloadData20 = downloadHandler(
      filename = function() { 
        paste('graph5','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot6()
        dev.off()
      },
      contentType='image/jpg')
    
    output$download20=renderUI({
      etat2=error()
      validate(
        need(etat2!="not","")
      )
      if(values()$NBFREQ==0){
        return()
      }
      else{
        return(downloadButton("downloadData20",gettext("Download as jpg")))
      }
    })
    
    output$downloadData21 = downloadHandler(
      filename = function() { 
        paste('graph5','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot6()
        dev.off()
      },
      contentType=NA)
    
    output$download21=renderUI({
      etat2=error()
      validate(
        need(etat2!="not","")
      )
      if(values()$NBFREQ==0){
        return()
      }
      else{
        return(downloadButton("downloadData21",gettext("Download as pdf")))
      }
    })
    
    output$downloadData22 = downloadHandler(
      filename = function() { 
        paste('graph5','.emf', sep='') 
      },
      content = function(file) {
        emf(file)
        Plot6()
        dev.off()
      },
      contentType=NA)
    
    output$downloadData1 = downloadHandler(
      filename = function() { 
        paste('graph1','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot1()
        dev.off()
      },
      contentType='image/jpg')
    
    output$downloadData2 = downloadHandler(
      filename = function() { 
        paste('graph1','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot1()
        dev.off()
      },
      contentType=NA)
    
    output$downloadData4 = downloadHandler(
      filename = function() { 
        paste('graph2','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot2()
        dev.off()
      },
      contentType='image/jpg')
    
    output$download4=renderUI({
      etat2=error()
      validate(
        need(etat2!="not","")
      )
      if(values()$NBQUANTI==0){
        return()
      }
      else{
        return(downloadButton("downloadData4",gettext("Download as jpg")))
      }
    })
    
    output$downloadData5 = downloadHandler(
      filename = function() { 
        paste('graph2','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot2()
        dev.off()
      },
      contentType=NA)
    
    output$download5=renderUI({
      etat2=error()
      validate(
        need(etat2!="not","")
      )
      if(values()$NBQUANTI==0){
        return()
      }
      else{
        return(downloadButton("downloadData5",gettext("Download as pdf")))
      }
    })
    
    output$downloadData6 = downloadHandler(
      filename = function() { 
        paste('graph2','.emf', sep='') 
      },
      content = function(file) {
        emf(file)
        Plot2()
        dev.off()
      },
      contentType=NA)
    
    output$downloadData7 = downloadHandler(
      filename = function() { 
        paste('graph1','.emf', sep='') 
      },
      content = function(file) {
        emf(file)
        Plot1()
        dev.off()
      },
      contentType=NA)
    
    ### Sorties
    
    output$sorties1=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$ind$coord))
    })
    
    output$sorties2=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$ind$contrib))
    })
    
    output$sorties3=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$ind$cos2))
    })
    
    output$sorties4=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$ind$within.inertia))
    })
    
    output$sorties5=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$ind$coord.partiel))
    })
    
    output$sorties6=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$ind$within.partial.inertia))
    })
    
    output$sorties11=renderTable({
      return(as.data.frame(values()$res.ctb1))
    },digits=-2)
    
    output$sorties22=renderTable({
      
      return(as.data.frame(values()$res.ctb2))
    })
    
    output$sorties44=renderTable({
      return(as.data.frame(values()$res.ctb3))
    })
    
    output$sorties12=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$partial.axes$coord))
    })
    
    output$sorties23=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$partial.axes$cor))
    })
    
    output$sorties34=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$partial.axes$contrib))
    })
    
    output$sorties45=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(values()$res.MFA$partial.axes$cor.between))
    })
    
    
    
    output$sortiegroup=renderTable({
      etat2=error()
      validate(
        need(etat2!="not",gettext("Please select at least 2 groups"))
      )
      write.infile(X=values()$res.MFA$group,file=paste(getwd(),"fichgroup.csv"),sep=";",nb.dec=5)
      baba=read.csv(paste(getwd(),"fichgroup.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      file.remove(paste(getwd(),"fichgroup.csv"))
      baba
    },
    include.rownames=FALSE)
    
    
    
  }
)


