#utilityFunctions.R

getDetLim <- function(input, groupno="censQ1"){
  
if(input$distribution =="Log-Normal"){
  dist="qlnorm"
  qargs = list(meanlog=input$meanlog, sdlog=input$sdlog)
}else{ 
  if(input$distribution=="Gamma"){
  dist="qgamma"
  qargs = list(shape=input$shape, scale=input$scale)
  }else{
    if(input$distribution=="Normal"){
      dist="qnorm"
      qargs = list(input$mu, sd =input$sd)
      
    }
  }
}

input1 <- c(input$censQ1, input$censQ2, input$censQ3)
qargs$p <- input1[c("censQ1", "censQ2", "censQ3") %in% groupno]
x <- do.call(dist, qargs)
return(x)
}