library(shiny)
library(rgdal)
#library(rsconnect)
#rsconnect::deployApp('path/to/your/app')

# The fillmap2 function is not mine. It's from the repository: carrollrm/fillmap, a package available on Github
fillmap2<-function(map, figtitle, y , leg.loc="beside", y.scl=NULL,main.cex=1.5,main.line=0,map.lty=1,leg.rnd=0,leg.cex=1){
  # 0: dark 1: light light Current shading ranges from darkest to light gray white (to distinguish with lakes).
  y.uq=sort(unique(c(y,y.scl)))
  cols<-viridis(length(y.uq),direction=-1)
  shading=y
  for (i in 1:length(y)){
    shading[i]<-cols[which(y.uq==y[i])]
  }
  par(mar=c(0,0,2,0))
  if (leg.loc=="beside"){
    layout(matrix(1:2,ncol=2),width=c(.8,.2))
  } else if (leg.loc=="below"){layout(matrix(1:2,nrow=2),height=c(.6,.4))
  } else (print("leg.loc options are below or beside"))
  plot(map,col=shading,axes=F, lty=map.lty)
  title(main=figtitle,cex.main=main.cex,line=main.line)
  par(mar=c(5, 4, 4, 2) + 0.1)
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
  cols.5=cols[seq(1,length(y.uq),length.out=5)]
  lab.5=cols.5
  for (i in 1:5){
    lab.5[i]=y.uq[which(cols==cols.5[i])[1]]
  }
  lab.5=round(as.numeric(lab.5),leg.rnd)
  par(mar=c(0,0,0,0))
  if (leg.loc=="beside"){
    legend_image <- as.raster(matrix(cols, ncol=1))
    text(x=1.6,y = seq(0,length(y.uq),length.out=5)/length(y.uq),labels = rev(lab.5), cex=leg.cex)
    rasterImage(legend_image, 0, 0, 1,1)
  } else{
    legend_image <- as.raster(matrix(cols, nrow=1))
    text(y=-0.25,x = seq(0,length(y.uq),length.out=5)/(length(y.uq)*.5),labels = lab.5, cex=leg.cex)
    rasterImage(legend_image, 0, 0, 2,1)}
}

NHtracts <- readOGR("Data\\NHtracts\\NHtracts.shp")
data <- read.csv("Data\\INLAdata_LTS.csv")
effectsTable <- read.csv("Data\\effectsTable.csv")

ui <- fluidPage(
  titlePanel(h1(id="header", "Case Study 4: WPD Arrests from 2010-2018")),
  tags$style(HTML("#header{color: teal;}")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Choose the year of interest: ", 
                  value=2010, min=2010, max=2018, sep=""),
      radioButtons("pop", "Select the population of interest: ", 
                   choices=c("Total"="arrests_total", 
                             "Black"="arrests_B", 
                             "White"="arrests_W")),
      radioButtons("calculation", "Select the calculation of interest: ", 
                   choices=c("None"="none", 
                             "Arrests by percent of the population"="popPercent", 
                             "Arrests by percent of total arrests"="arrestPercent", 
                             "Standardized Incidence Ratio"="sir", 
                             "Poisson regression"="poisson"))
    ),
    mainPanel(
      plotOutput("fillmap2"),
      tableOutput("poissonTable")
    )
  )
)



server <- function(input, output){
  output$fillmap2 <- renderPlot({
    
    # This conditional block is to establish variables to feed into dataframe columns and for map title outputs
    if(input$pop=="arrests_total"){
      arrestpop <- "total_total"
      percentpop <- "totalarrestsPop"
      titlepop <- "Total Arrests"
      SIR_col <- "SIR_total"
      poisson_col <- "poisson_total"
      
    } else if(input$pop=="arrests_W"){
      arrestpop <- "white_total"
      percentpop <- "whitearrestsPop"
      titlepop <- "White Only Arrests"
      SIR_col <- "SIR_white"
      poisson_col <- "poisson_white"
      
    }else if( input$pop=="arrests_B"){
      arrestpop <- "black_total"
      percentpop <- "blackarrestsPop"
      titlepop <- "Black Only Arrests"
      SIR_col <- "SIR_black"
      poisson_col <- "poisson_black"
    }

    # Creation of map data, based on the calculation type selected
    if(input$calculation=="none"){
      map <- data[which(data$year==input$year), input$pop]
      title <- paste(titlepop,"(",input$year,")")
      scl <- data[, input$pop]

    } else if(input$calculation == "popPercent"){
      map <- data[which(data$year==input$year),percentpop]
      title <- paste(titlepop,"By Total Population","(",input$year,")")
      scl <- data[, percentpop]

    } else if(input$calculation=="arrestPercent"){
      map <- data[which(data$year==input$year),arrestpop]
      title <- paste(titlepop,"By Total Arrests","(",input$year,")")
      scl <- seq(0, 100, 0.1)

    } else if(input$calculation=="sir"){
      map <- data[which(data$year == input$year), SIR_col]
      title <- paste("Standardized Incidence Ratio of", titlepop, "(",input$year,")")
      scl <- data[,SIR_col]

    } else if(input$calculation=="poisson"){
      map <- exp(data[which(data$poisson_year == input$year), poisson_col])
      title <- paste("Poisson Regression of", titlepop, "(",input$year,")")
      scl <- exp(data[, poisson_col])
    }
    fillmap2(NHtracts, title, map, y.scl=scl, map.lty=0, leg.rnd=2)
  })
  
  #fixed effects table for poisson regression values
  output$poissonTable <- renderTable({
    
    if(input$calculation == 'poisson'){
      if(input$pop == "arrests_total"){
        tableOut <- effectsTable[, c("mean_total", "lower_total", "upper_total")]
      } else if(input$pop == "arrests_W"){
        tableOut <- effectsTable[, c("mean_white", "lower_white", "upper_white")]
      } else if(input$pop == "arrests_B"){
        tableOut <- effectsTable[, c("mean_black", "lower_black", "upper_black")]
      }
      colnames(tableOut) <- c("Mean", "Lower 95% CI", "Upper 95% CI")
      rownames(tableOut) <- c("% Black", "% Poverty", "% Undergrad", 
                                "% Male", "% Secondary Homes", "% Aged 18-24", "% Vacant Homes")
      tableOut
      }
  },
  rownames = TRUE,
  colnames = TRUE, 
  digits = 3,
  width="100%"
  )
}

shinyApp(ui=ui, server=server)
