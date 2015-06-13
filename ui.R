#
# - User Interface
#

shinyUI(
    fluidPage(
        titlePanel("Iris Flower Dataset"),
        sidebarLayout(
            sidebarPanel(
                "Exploring the Iris Dataset",
                helpText(paste0("Select a tab on the right and anaysis type below. ",
                    "Dont forget to hit refresh when you make updates in the ",
                    "dropdown menu below.")),
                selectInput("data_plot_method", 
                    label = "Plot Method:",
                    choices = c("Box", "Density", "Pairs"),
                    selected = "Box"
                ),
                selectInput("unsup_method", 
                    label = "Unsupervised Learning Method:",
                    choices = c("Principal Components", "K-Means Clustering", "Hierarchical Clustering"),
                    selected = "Principal Components"
                ),
                submitButton("Refresh")
            ),
            mainPanel(
                tabsetPanel(type = "tabs",
                    tabPanel("Plot",
                        plotOutput('data_plot')
                    ),
                    tabPanel("Unsupervised",
                        plotOutput('unsup_learn'),
                        textOutput("unsup_learn_summary")
                    ),
                    tabPanel("Data",
                        DT::dataTableOutput('data_table')
                    )
                )
            )
        )
    )
)