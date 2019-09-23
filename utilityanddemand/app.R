library(shiny)
library(latex2exp)
wmax <- 100
wmin <- 10
pxmin <- 1 
pxmax <- 10
pymin <- 1
pymax <- 10
amin <- .01
amax <- .99

ui <- fluidPage(
    # Application title
    titlePanel("utility maximization and demand function"),

   sidebarLayout(
        sidebarPanel(
            sliderInput("W",
                        "Wealth:",
                        min = wmin,
                        max = wmax,
                        value = (wmin+wmax)/2),
            sliderInput("px",
                        "price of xylophones:",
                        min = pxmin,
                        max = pxmax,
                        value = (pxmin+pxmax)/2),
            sliderInput("py",
                        "price of yaks:",
                        min = pymin,
                        max = pymax,
                        value = (pymin+pymax)/2),
            sliderInput("a",
                        "alpha:",
                        min = amin,
                        max = amax,
                        value = (amin+amax)/2)
                 ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plots")
        )
    )
)

server <- function(input, output) {
    output$plots <- renderPlot({
        par(mfrow=c(2,2))
        x <- seq(0,input$W/input$px,length=101)#x grid
        y <- seq(0,input$W/input$py,length=101)#y grid
        a <- input$a
        u <- function(x,y,a) x^a*y^(1-a)#utility function
        u1 <- outer(x,y,u,a)#player 1's utility
        x1s <- a*input$W/input$px
        y1s <- (1-a)*input$W/input$py
        plot(x,y,type="n",xaxs="i",yaxs="i",xlab="xylophones",ylab="yaks",main=TeX('$U=x^{\\alpha}y^{1-\\alpha}$'),xlim=c(0,wmax/pxmin),ylim=c(0,wmax/pymin))
        points(x1s,y1s,pch = 19,col= rgb(0, 0, 1, 0.5))# the optimizing bundle for player 1 
        contour(x=x, y=y, z=u1,levels= u(x1s,y1s,a),add=TRUE,drawlabels = FALSE,lwd=2,col= rgb(0, 0, 1, 0.5))#1's IC through optimizing bundle
        abline(a=input$W/input$py,b=-input$px/input$py,lwd=2,col= rgb(0, 0, 0, 0.5))
        text(80,150,paste("price ratio=",input$px/input$py,sep=""))
        
        plot(range(x),c(pxmin,pxmax),type="n",xaxs="i",yaxs="i",xlab="xylophones",ylab="price xylophones",main="Demand for xylophones",xlim=c(0,100),ylim=c(0,pymax))
        points(a*input$W/input$px,input$px,pch = 19,col= rgb(1, 0, 0, 0.5))
        pxgrid <- seq(pxmin,pxmax,length=101)
        xd <- a*input$W/pxgrid
        lines(xd,pxgrid,col=rgb(1, 0, 0,.5))
        
        plot(range(y),c(pymin,pymax),type="n",xaxs="i",yaxs="i",xlab="yaks",ylab="price yaks",main="Demand for yaks",xlim=c(0,100),ylim=c(0,pymax))
        points((1-a)*input$W/input$py,input$py,pch = 19,col= rgb(1, 0, 0, 0.5))
        pygrid <- seq(pymin,pymax,length=101)
        yd <- (1-a)*input$W/pygrid
        lines(yd,pygrid,col=rgb(1, 0, 0,.5))
        
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
