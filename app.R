#' @BINGO-DE-ANIMAIS
#' 
#' 
#' rsconnect::deployApp(account = "chicodias")

# Imagens dos animais
dados<-as.data.frame(matrix(c(
    "ABELHA.png",1,"https://imgur.com/UmGrpxB.png",
    "BICHO-PAU.png",2,"https://i.imgur.com/yE3ldMd.png",
    "BICHO-PREGUIÇA.png",3,"https://imgur.com/ISMnmlr.png",
    "BORBOLETA.png",4,"https://imgur.com/07loDKn.png",
    "CACHORRO.jpg",5,"https://i.imgur.com/ZhSoLPw.jpg",
    "CORUJA.jpg",6,"https://imgur.com/qqLFU8v.jpg",
    "CARANGUEJO.png",7,"https://imgur.com/40fZt2X.png",
    "CANGURU.png",8,"https://imgur.com/uiCr9Fh.png",
    "CAVALO.jpg",9,"https://imgur.com/KfeeVbA.png",
    "COBRA.jpg",10,"https://imgur.com/fiAmo52.jpg",
    "COELHO.png",11,"https://imgur.com/Wa1XIai.jpg",
    "DINOSSAURO.jpg",12,"https://i.imgur.com/pA1pTHo.jpg",
    "DRAGÃO.png",13,"https://imgur.com/KrmR0cT.png",
    "ELEFANTE.png",14,"https://imgur.com/SaAipDb.png",
    "ESQUILO.png",15,"https://imgur.com/lzDhJO0.png",
    "GALINHA.png",16,"https://imgur.com/JCCwzZX.png",
    "GATO.png",17,"https://imgur.com/Mh6Uz2N.png",
    "GIRAFA.jpg",18,"https://imgur.com/VvsChJ5.jpg",
    "GOLFINHO.png",19,"https://imgur.com/JdX2Wre.png",
    "HIPOPÓTAMO.jpg",20,"https://imgur.com/mrfPnLJ.jpg",
    "JACARÉ.jpg",21,"https://imgur.com/JKGe2Ej.jpg",
    "JOANINHA.png",22,"https://imgur.com/vsPHimi.png",
    "LAGARTA.png",23,"https://imgur.com/Xn5Q3qK.png",
    "LEÃO.png",24,"https://imgur.com/D2mjfgi.png",
    "MACACO.png",25,"https://imgur.com/FQ6bRvf.png",
    "MORCEGO.png",26,"https://imgur.com/fgeDMXA.png",
    "PAPAGAIO.jpg",27,"https://i.imgur.com/DcCRnJJ.jpg",
    "PATO.jpg",28,"https://i.pinimg.com/originals/20/7e/4d/207e4d49a226f33342e41d8aa0aec06c.jpg",
    "PEIXE.png",29,"https://imgur.com/Mk5U1Nf.png",
    "PINGUIM.png",30,"https://imgur.com/1d9HruF.png",
    "POLVO.png",31,"https://imgur.com/cDFY9r1.png",
    "PORCO-ESPINHO.png",32,"https://imgur.com/yR1mJr3.png",
    "PORCO.png",33,"https://imgur.com/G7W7qGP.png",
    "RATO.png",34,"https://imgur.com/ZIiupzv.png",
    "SAPO.png",35,"https://imgur.com/UFAG2nR.png",
    "TARTARUGA.png",36,"https://i.imgur.com/uoSL8rW.png",
    "TATU.png",37,"https://i.imgur.com/IMVo4WC.png",
    "URSO.png",38,"https://imgur.com/AxwsXfV.png",
    "VACA.jpg",39,"https://imgur.com/pZGXBF0.png",
    "ZEBRA.jpg",40,"https://i.imgur.com/rP3J2NK.jpg"),ncol=3,byrow=T))


library(shiny)
library(magrittr)
library(dplyr)


#removendo extensao
dados <- dados %>% mutate("Animais sorteados: " = tools::file_path_sans_ext(dados$V1))

# Define UI for application that draws a histogram
ui <- fluidPage(

    
    
    pageWithSidebar(
        headerPanel("Bingo de Bichos"),
        sidebarPanel(
            
            uiOutput("downCart"),
            
            uiOutput("downIndex"),
        
            #numericInput("n", "N:", min = 0, max = 100, value = 50),
            br(),br(),
            actionButton("goButton", "Iniciar", icon = icon("play", "fa")),
            actionButton("restartButton", "Sortear novamente", icon = icon("random", "fa")),
            #p("Click the button to update the value displayed in the main panel.")
        ),
        mainPanel(
#            imageOutput("nimg"),
            uiOutput(outputId = "image"),
            tableOutput("nText")
            #verbatimTextOutput
        )
    )
    
    
)

# Define server logic
server <- function(input, output) {
        
    v <- reactiveValues(data = NULL)
    v$data <- sample(1:40,1)
    
    observeEvent(input$goButton, {
        v$data <- sample(1:40,1)
    })
    
    observeEvent(input$restartButton, {
        if (is.null(v$data)) return
            
        sorteados <- v$data
        sorteio<-(1:40)[-sorteados]
        if(length(sorteio)==1) amostra=sorteio else
            amostra<-sample(c(sorteio),1)
        v$data <- c(sorteados,amostra)
        
    })  
    
        output$nText <- renderTable({
            #if (is.null(v$data))
            #    print("Que tal começar um novo jogo?")
            #else
            
            dados %>% filter(V2 %in% v$data) %>% select("Animais sorteados: ") #%>%  print
            
        })
        
        output$image <- renderUI({
                url_im <- dados[v$data[length(v$data)],1] %>% as.character()
                tags$img(src = url_im, width = 225)
        })
     
        
        output$downCart <- renderUI({
            tagList(a( "Baixar cartelas", href="https://github.com/chicodias/bingo-animais/raw/master/Cartelas.pdf", target="_blank"))
            
        })
        output$downIndex <- renderUI({
            tagList(a( "Índice de bichos", href="https://github.com/chicodias/bingo-animais/raw/master/Indice-de-bichos.pdf", target="_blank"))
        })
         
    }



# Run the application 
shinyApp(ui = ui, server = server)
