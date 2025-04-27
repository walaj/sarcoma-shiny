library(shiny)
library(ggplot2)
library(data.table)
library(plotly)
library(GenomicRanges)

datadir <- "~/Dropbox/SS/data"

## LOAD THE data if not done yet
if (!exists("dt.gtex.melt") || 
    !exists("dt.pcawg.melt") || 
    !exists("gr.rna") || 
    !exists("rna.ra") || 
    !exists("dt.t") || 
    !exists("gr.t") || 
    !exists("gr.t.minor") || 
    !exists("gr.t.major") || 
    !exists("gr.t.flat") || 
    !exists("dt.mem") || 
    !exists("dt_cn_exp") || 
    !exists("memgtex")) {
  
  # Load GTEx data
  dt.gtex.melt <- readRDS(file.path(datadir, "gtex_melt.rds"))
  setkey(dt.gtex.melt, Description)
  
  # Load PCAWG data
  dt.pcawg.melt <- readRDS(file.path(datadir, "pcawg_data.rds"))
  setkey(dt.pcawg.melt, GeneID)
  
  # load SS data 
  gr.rna <- readRDS(file.path(datadir, "gr_rna.rds"))
  rna.ra <- readRDS(file.path(datadir, "rna_data.rds"))
  setkey(rna.ra, GeneID)
  
  # Load the copy number data
  dt.t <- fread(file.path(datadir, "tumor_vs_blood.cnvs.txt"))
  dt.t[, cn := nMajor + nMinor]
  gr.t <- with(dt.t, GRanges(seqnames=chr, IRanges(startpos, endpos), mcols=dt.t[,.(nMajor, nMinor, cn)]))
  
  ## set major and minor CN alleles
  gr.t.minor = gr.t
  gr.t.minor$cn = gr.t$mcols.nMinor
  gr.t.minor$colfield = "minor"
  gr.t.major = gr.t
  gr.t.major$cn = gr.t$mcols.nMajor
  gr.t.majorcolfield = "major"
  gr.t.flat = c(gr.t.minor, gr.t.major)
  
  # Load membrane protein database
  dt.mem <- readRDS(file.path(datadir, "dt_mem.rds"))
  
  ## load gTrack data if available
  if (requireNamespace("gdTrack", quietly = TRUE)) {
    
    library(gTrack)          # attach it only after we know it exists
    
    ## make the gene gTrack
    # td38 <- track.gencode(gencode="~/Dropbox/SS/gencode.v38.annotation.gtf.gz",
    #                       cached=FALSE, cached.dir="dummy")
    td38 <- readRDS(file.path(datadir, "td38_gencode_gtrack.rds"))
    td38.focus <- data.table::copy(td38)
    td38.focus@formatting$legend.maxitems <- 0
    td38.focus@formatting$legend <- FALSE
    # td38.focus@data[[1]] <- td38.focus@data[[1]][names(td38.focus@data[[1]]) %in% names(gns)]
    
    cnTrackMinor <- gTrack(gr.t.minor, y.field = "cn")
    cnTrackMajor <- gTrack(gr.t.major, y.field = "cn")
    cnTrack      <- gTrack(gr.t,        y.field = "mcols.cn")
    
    dt.raw <- data.table::fread(file.path(datadir,
                                          "tumor_vs_blood.tumour_tumourLogR.txt"))
    gr.raw <- with(dt.raw,
                   GenomicRanges::GRanges(seqnames = Chromosome,
                                          IRanges::IRanges(Position, Position),
                                          mcols = dt.raw[, .(tumor_vs_blood.tumour)]))
    cnRaw <- gTrack(gr.raw, y.field = "mcols.tumor_vs_blood.tumour")
  }
  
  ## overlap CN with expression
  # dt_cn_exp <- gr2dt(gr.findoverlaps(gr.t, gr.rna))
  # dt_cn_exp[, bg24 := gr.rna$bostongene_2024.09[subject.id]]
  # dt_cn_exp[, GeneID := gr.rna$GeneID[subject.id]]
  # dt_cn_exp[, cn := gr.t$mcols.cn[query.id]]
  # dt_cn_exp[, jittercn := cn + runif(.N, min = -0.2, max = 0.2)]
  # ## overlap with membrane data
  # dt_cn_exp[, membrane := ifelse(GeneID %in% dt.mem$gene_name, "Membrane","non-membrane")]

  ## load copy-number v expression
  dt_cn_exp <- readRDS(file.path(datadir, "dt_cn_expression.rds"))
  
  ## load membrane data combined with SS data 
  memgtex <- readRDS(file.path(datadir, "membrane_gtex.rds"))
}

# Define UI
ui <- fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    # Shared Sidebar for Gene Selection
    sidebarPanel(
      textInput("gnn", "Enter Gene Name:", value = "GAPDH"),
      actionButton("update", "Update Plot"),
      radioButtons("dataset_choice", "Select Expr Data:", 
                   choices = c("Boston Gene 2024" = "bg24", "Boston Gene 2022" = "bg22"),
                   selected = "bg24")
    ),
    
    # Main Panel with Tabs
    mainPanel(
      tabsetPanel(
        tabPanel(
          "GTEx",
          plotOutput("gtexPlot", height = "600px")
        ),
        tabPanel(
          "PCAWG",
          plotOutput("pcawgPlot", height = "600px")
        ),
        tabPanel(
          "Copy-Number / Expression",
          plotlyOutput("cnePlot", height = "600px")
        ),
        tabPanel(
          "Copy-Number",
          plotOutput("cnPlot", height="300px")
        ),
        tabPanel(
          "Membrane GTEx",
          plotlyOutput("membraneGTExPlot", height = "600px")
        ),
        #tabPanel("WGS Mutations", DTOutput("mut_table"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive gene name (updates only when button is clicked)
  selected_gene <- eventReactive(input$update, {
    req(input$gnn)
    input$gnn
  })
  
  # Reactive filtered data for GTEx
  filtered_gtex <- reactive({
    req(selected_gene())
    gnn_selection = selected_gene()
    ab <- dt.gtex.melt[gnn_selection]
    validate(need(nrow(ab) > 0, "Gene not found in GTEx dataset."))
    return(ab)
  })
  
  # Reactive filtered data for PCAWG
  filtered_pcawg <- reactive({
    req(selected_gene())
    gnn_selection = selected_gene()
    ab <- dt.pcawg.melt[gnn_selection]
    validate(need(nrow(ab) > 0, "Gene not found in PCAWG dataset."))
    return(ab)
  })
  
  # Reactive expression levels
  expr_levels <- reactive({
    req(selected_gene(), input$dataset_choice)
    if (input$dataset_choice == "bg24") {
      return(2^rna.ra[selected_gene()]$bostongene_2024.09)
    } else {
      return(2^rna.ra[selected_gene()]$bostongene_2022.11)
    }
  })
  
  
  # Render GTEx Plot
  output$gtexPlot <- renderPlot({
    ab <- filtered_gtex()
    expr_vals <- expr_levels()
    
    ggplot(ab, aes(x = value)) + 
      geom_histogram() + 
      geom_vline(xintercept = expr_vals, color = "#756bb1", linetype = "dashed", linewidth=1.5) +
      theme_bw() + 
      facet_wrap(~SMTS, scales = "free") + 
      xlab("Expression (TPM)") + 
      ylab("GTEx sample count")
  })
  
  # Render PCAWG Plot
  output$pcawgPlot <- renderPlot({
    ab <- filtered_pcawg()
    expr_vals <- expr_levels()
    
    ggplot(ab, aes(x = tpm)) + 
      geom_histogram() + 
      geom_vline(xintercept = expr_vals, color = "#756bb1", linetype = "dashed", linewidth=1.5) +
      theme_bw() + 
      facet_wrap(~tumor_type, scales = "free") + 
      xlab("Expression (TPM)") + 
      ylab("PCAWG sample count")
  })
  
  output$cnPlot <- renderPlot({
    print(selected_gene())
    gw <- genes_granges[genes_granges$gene_name == selected_gene()]
    
    # Ensure we get exactly one match
    if (length(gw) == 0) {
      print("Gene not found")
      return(NULL)  # Prevents the plot from executing
    } else if (length(gw) > 1) {
      print("Multiple matches found, refine search")
      return(NULL)
    }
    
    print(gw)
    
    # Proceed with plotting if a single valid gene is found
    plot(c(cnTrack, cnRaw, td38.focus), windows = gw + 20000, ylim = c(0, 8))
  }, width = 700, height = 300, res = 36)
  
  output$membraneGTExPlot <- renderPlotly({
    plot_ly(memgtex, 
            x = ~.data[["bostongene_2024.09"]], 
            y = ~.data[["max_log2"]], 
            text = ~.data[["GeneID"]], 
            type = 'scatter', 
            mode = 'markers',
            marker = list(size = 5)) %>%
      layout(xaxis = list(title = "Tumor 2024"),
             yaxis = list(title = "GTEx Max"))
  })
  
  output$cnePlot <- renderPlotly({
    dt_cn_exp[, is_target := FALSE] ## reset
    dt_cn_exp[, is_target:= GeneID==selected_gene() ]
    plot_ly() %>%
      add_trace(data = dt_cn_exp[dt_cn_exp$membrane != "Membrane", ], 
                x = ~jittercn, 
                y = ~bg24, 
                text = ~GeneID, 
                type = 'scatter', 
                mode = 'markers',
                name = "Non-Membrane",
                marker = list(size = 4, opacity = 0.5, color="black")) %>%
      add_trace(data = dt_cn_exp[dt_cn_exp$membrane == "Membrane", ], 
                x = ~jittercn, 
                y = ~bg24, 
                text = ~GeneID, 
                type = 'scatter', 
                mode = 'markers',
                name = "Membrane",
                marker = list(size = 4, opacity = 1, color = "red")) %>%
      add_trace(data = dt_cn_exp[is_target==TRUE, ], 
                x = ~jittercn, 
                y = ~bg24, 
                text = ~GeneID, 
                type = 'scatter', 
                mode = 'markers',
                name = "Selected Gene",
                marker = list(size = 16, opacity = 1, color = "#addd8e")) %>%
      layout(title = "Copy Number vs. Expression",
             xaxis = list(title = "Copy Number"),
             yaxis = list(title = "Boston Gene 2024 log2 expression")) 
      
  })
}

# Run the App
shinyApp(ui = ui, server = server)
