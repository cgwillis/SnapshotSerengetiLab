# Overview Tab
overviewTab <- tabItem(
  tabName = 'overview',
box(title = "Instructions",
    status = "primary",
    solidHeader = F,
    collapsible = F,
    width = 12,
    fluidRow(column(width = 5,
      'This is an online application designed to let you explore and analyze data from the Snapshot Serengeti research site for your own lab project.', br(), br(),
      'The application uses real data from the Snapshot Serengeti camera trap site. The data you will be analyzing represents nearly 900,000 photos taken over 3 years (2010-2013).', br(), br(),
      'The application analyzes these data using the R, a powerful, statistical programming language that is one of the most commonly used research tools in biology (and elsewhere!).', br(), br(),
      'Fortunately, you won’t have to do any programming yourself! We have designed a collection of graphing tools that allow you to manipulate and analyze the data without having to modify the underlying code itself.', br(), br(),
      'The different graphing tools are listed on the left-hand menu. Each graph presents and summarizes the data in different ways. It is up to you to decide which graph or set of graphs are best suited to answering your research question. Click one to start your analytical exploration of the Snapshot Serengeti data!', br(), br(),
      'We recommend you have the list of variable descriptions for the Snapshot Serengeti dataset handy when working with these graphs. This will help you decide which variables and graphs will be useful for answering your question.', br()
      ),
            column(width = 5, align = "center",
              img(src="https://snapshotserengeti.s3.msi.umn.edu/S2/E09/E09_R3/S2_E09_R3_IMAG0229.JPG", width=500)
              )
      )
    )
  )
