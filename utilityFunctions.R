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


createData <- function(input) {
    if(input$distribution =="Log-Normal"){
  dist="lnorm"
  qargs = list(meanlog=input$meanlog, sdlog=input$sdlog)
  pop.mean <- exp(input$meanlog + 0.5 *input$sdlog ^2)
  pop.var <- (exp(input$sdlog^2)-1) * exp(2 *input$meanlog +input$sdlog ^2)

}else{
  if(input$distribution=="Gamma"){
  dist="gamma"
  qargs = list(shape=input$shape, scale=input$scale)
  pop.mean <- input$shape * input$scale
  pop.var <- input$shape * input$scale^2

  }else{
    if(input$distribution=="Normal"){
      dist="norm"
      qargs = list(input$mu, sd =input$sd)
      pop.mean <- input$mu
      pop.var <- input$sd^2


    }
  }
}

    totalObs <- input$obs1 + input$obs2 +input$obs3
    rdist <- paste("r", dist, sep="")
    qdist <- paste("q", dist, sep="")

    qargs$p <- c(input$censQ1, input$censQ2, input$censQ3)
    cenValues <- do.call(qdist, qargs)
    cenValues <- rep(cenValues, times = c(input$obs1, input$obs2, input$obs3))

    cenValue <- sample(cenValues, totalObs, replace=FALSE)
    cenValue <- rep(cenValue, times = input$simNum)

    # create sample from distribution
    qargs$p <- NULL
    qargs$n <- totalObs * input$simNum
    dist <- do.call(rdist, qargs)
    cen = cenValue > dist


    dfx <- data.frame(value=dist, cen=cenValue>dist, cenValue=cenValue)
    dfx$obs <- dfx$value
    dfx$obs[dfx$cen] <- cenValue[dfx$cen]
    dfx$obs_zero <- dfx$value
    dfx$obs_zero[dfx$cen] <- 0
    dfx$obs_half <-dfx$value
    dfx$obs_half[dfx$cen] <- 0.5 * cenValue[dfx$cen]
    dfx$obs_full <- dfx$value
    dfx$obs_full[dfx$cen] <- cenValue[dfx$cen]
    dfx$obs_sqrt2 <- dfx$value
    dfx$obs_sqrt2[dfx$cen] <- cenValue[dfx$cen]/sqrt(2)
    dfx$run <- input$myValue
    dfx$sampleNum <- rep(1:input$simNum, each=totalObs)
    return(list(dfx = dfx, pop.mean = pop.mean, pop.var = pop.var))
}


