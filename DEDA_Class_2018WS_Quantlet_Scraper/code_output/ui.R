#QD &lt;- read.csv("Content.csv",row.names=1)
shinyUI(
  pageWithSidebar(
    headerPanel("Relevance plot for the question and media layer"),
    sidebarPanel(
      selectInput("Q",
                  "Choose a question's No.:",
                  choices = as.list(row.names(QD[QD[,1]=="MA",])),
                  selected = "Q15"
      ),
      selectInput("Lhs",
                  "Choose a arrow's direction:",
                  choices = c("M","A"),
                  selected = "M"
      ),
      sliderInput("Supp",
                  "Set a min supp:",
                  0,0.1,0.01,step=0.001
      ),
      sliderInput("Conf",
                  "Set a min conf:",
                  0,1,0.1,step=0.01
      ),
      sliderInput("Lift",
                  "Set a min lift:",
                  0,2,1.3,step=0.1
      ),
      sliderInput("Cex",
                  "Set a texts size:",
                  0,2,1,step=0.1
      ),
      sliderInput("As",
                  "Set a circles size:",
                  0,100,25
      ),
      sliderInput("Ms",
                  "Set a rhombus size:",
                  0,100,50
      )
    ),
    mainPanel(
      plotOutput("Aplot",width="100%", height="1000px")
    )
  )
)
