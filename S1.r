library(shiny)
library(shinydashboard)
library(cluster)
library(DT)
library(MASS)

tab<-read.csv("gpw_d.csv",header=TRUE,sep=",")
tab.features = tab[2:6]


ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Nawigacja"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dane", tabName = "W0", icon = icon("database")),
      menuItem("Hierarchiczna", tabName = "W1", icon = icon("stats", lib = "glyphicon")),
      menuItem("K-srednich", tabName = "W3", icon = icon("stats", lib = "glyphicon")),
      menuItem("Sylwetka/separowalnosc", tabName = "W4", icon = icon("stats", lib = "glyphicon"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "W0",
              ui <- basicPage(
                h2("Dane historyczne: Gielda Papierow wrtosciowych "),
                DT::dataTableOutput("mytable")
              ) 
      ),
      tabItem(tabName = "W1",
              
              tabsetPanel(type = "tabs",
                          tabPanel("Euclidean", box(
                            title = "Single euclidean", width = NULL, solidHeader = TRUE, status = "warning",
                            plotOutput("plot1"))),
                          tabPanel("Manhattan", box(
                            title = "Complete manhattan", width = NULL, solidHeader = TRUE, status = "warning",
                            plotOutput("plot4")))
              ),
      ),
      
      
      tabItem(tabName = "W3",
              ui <- fluidPage(
                selectInput("select", label = h3("Wybierz ilosc klastrow"), 
                            choices = list("1" = 1, "2" = 2, "3" = 3), 
                            selected = 1),
                radioButtons("rb2", "Wybierz argumenty:",
                             choiceNames = list(
                               "Otwarcie i Zamkniecie",
                               "Najwyzszy i Najnizszy"
                             ),
                             choiceValues = list(
                               "2", "3"
                             )),
              box( 
                title = "K-srednich", width = NULL, solidHeader = TRUE, status = "warning",
                plotOutput("plot5")),
      ),
    ),
    
    tabItem(tabName = "W4",
            box(title = "Sylwetka/separowalnosc", width = NULL, solidHeader = TRUE, status = "warning", plotOutput("plot6")
                        
            ),
    )
    
    
    )
  ),
)

server <- function(input, output) { 
  output$mytable = DT::renderDataTable({
    tab
  })
  
  output$plot1 <- renderPlot({
    kl.h<-agnes(x=tab.features,metric="euclidean",method="single");
    plot(kl.h,which.plots=2,main="");
    
  })
  output$plot2 <- renderPlot({
    kl.h<-agnes(x=tab.features,metric="euclidean",method="complete");
    plot(kl.h,which.plots=2,main="");
    
  })
  output$plot3 <- renderPlot({
    kl.h<-agnes(x=tab.features,metric="manhattan",method="single");
    plot(kl.h,which.plots=2,main="");
    
  })
  output$plot4 <- renderPlot({
    kl.h<-agnes(x=tab.features,metric="manhattan",method="complete");
    plot(kl.h,which.plots=2,main="");
    
  })
  output$plot5 <- renderPlot({
    results <- kmeans( tab.features,input$select)
    if (input$rb2==1){
    plot(tab[c("Otwarcie","Zamkniecie")], col = results$cluster,main="")
    } else {
      plot(tab[c("Najwyzszy","Najnizszy")], col = results$cluster,main="")
  }
  })
  
  output$plot6 <- renderPlot({
    X=as.matrix(tab[2:6])
    
    silK=rep(NA,10); silP=silK
    for(k in 1:10) {
      silK[k]=mean(silhouette(kmeans(X,k+1)$clust,dist(X))[,3])
      silP[k]=mean(silhouette(pam(X,k+1))[,3])
    }
    
    n0=nrow(X); tK=sum(diag(var(X)*(n0-1)/n0)); tP=sum(dist(X))
    for( k in 2:10) {
      tK=c(tK,sum(kmeans(X,k,iter.max=150)$with/n0))
      pa=pam(X,k)$clust; dd=rep(NA,k)
      for(i in 1:k) dd[i]=sum(dist(X[pa==i,]))
      tP=c(tP,sum(dd))
    }
    sepK=1-tK/tK[1]; sepP=1-tP/tP[1]
    
    par(mfrow=c(1,2),las=1)
    matplot(2:11,cbind(silK,silP),type="o",lty=1,pch=1,main="Sylwetka")
    matplot(1:10,cbind(sepK,sepP),type="o",lty=1,pch=1,main="Separowalnosc")
    
    
  })
  
  }


shinyApp(ui, server)