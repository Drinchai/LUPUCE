library(dashboardthemes)
library(plotly)
library(ggplot2)
library(DT)
library(reshape2)
library(shinydashboard)
library(ComplexHeatmap)
library(shiny)
library(ggpubr)
library(BiocManager)
options(repos = BiocManager::repositories())

#rsconnect::setAccountInfo(name='immunology-research',
#                          token='20A98A84749E36636D39432860943577',
 #                         secret='I/rcOccLkSGdl7gdHVitO3m5q8udHqnc0VrU+Jsk')
#rsconnect::setAccountInfo(name='thejacksonlaboratory',
 #                         token='78718D5DA0C18888FDC0B5FA405AF2CB',
 #                         secret='ZDlN1ef8GyMAOQjEoyvhJ8mq8wNAZ3OkNupjFm+4')

library(rsconnect)
load("./Shinny_LUPUCE_Module_Gen3v3.Rdata")

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "flat_red"),
  #tag link
  tags$script(HTML("
                   var openTab = function(tabName){
                   $('a', $('.sidebar')).each(function() {
                   if(this.getAttribute('data-value') == tabName) {
                   this.click()
                   };
                   });
                   }
                   ")),
  #link color
  tags$head(tags$style(HTML("a {color: dark-blue}"))),
  #footer
  tags$div(fluidRow(
    tags$footer(title="Your footer here", align = "right", style = "
                position:absolute;
                bottom:0;
                width:100%;
                height:10px; /* Height of the footer */
                color: white;
                padding: 10px;
                background-color: lightgrey;
                z-index: 1000;"
    ))),

  tabItems(
    tabItem("hometab",
            fluidRow(
              box(width = 12,
                  strong("The LUPUCE: SLE web-application is an interactive online platform that offers a comprehensive overview of the transcriptomic landscape associated with systemic Lupus Erythematosus (SLE), a complex and chronic autoimmune disease characterized by inflammation and damage to various organs such as skin, joints, and kidneys. ", style = "color:blue"),
                  br(),
                  br(),
                  
                  
                  p("The development of this web-application is based on the recently published" ,"BloodGen3", "repertoire, a collection of blood transcriptional modules specifically designed for the analysis and interpretation of blood transcriptome data",a("(Altman & Rinchai et al, Nature Communications 2021)",href="https://www.nature.com/articles/s41467-021-24584-w")),
                  
                  
                  p("The BloodGen3 repertoire is based on a wide range of disease and physiological states, encompassing a total of 16 conditions and 985 distinct transcriptome profiles. The input datasets used for its construction have been deposited in the NCBI's public repository GEO (", a("GSE100150",href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE100150"),")"),
                  
                  
                  p("The composition of the set of 382 modules constituting the repertoire has been shared",a("(Module lists)",href="https://github.com/Drinchai/BloodGen3Module/blob/master/data/Module_listGen3.rda"),
                    "along with extensive functional annotations which are accessible via interactive", a("“circle packing plots”",href="https://prezi.com/view/6FalfBDmwqezlDW0MTrY/"),
                    "and a R package, “BloodGen3module”, which is available via,", a("GitHub and Bioconductor",href="https://github.com/Drinchai/BloodGen3Module",".")),
                  
                  
                  p("Practically, the web-application allows users to explore fingerprint representations of microarray blood transcriptomic data obtained from the LUPUCE SLE cohort",
                    a("(Chiche et al. Arthritis & Rheumatology, 2014)",href="https://pubmed.ncbi.nlm.nih.gov/24644022/"),"and, ",a("Noémie Jourde-Chiche et al, Rheumatology, 2017)",href="https://pubmed.ncbi.nlm.nih.gov/28031441/")," for more informations), publicly available on GEO ",
                    a("GSE49454",href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE49454"), "This cohort includes 157 samples from 62 adult SLE patients, exhibiting different disease activity levels, various types of flares (including biopsy-proven lupus nephritis), distinct auto-antibody profiles, and varying levels of IFN signatures. Researchers using this app can assess immune profiles in different clinical and biological settings, investigate disease development or treatment response, and analyze various patterns, trends, and correlations within the provided data, facilitating a deeper understanding of the disease and its underlying mechanisms."),
                  
                  
                  p("Additionally, it is worth noting that three other web applications are available, focusing on : Covid-19:",a("(Rinchai et al, JTM, 2020)",href="https://translational-medicine.biomedcentral.com/articles/10.1186/s12967-020-02456-z"),
                    "and Respiratory syncytial virus infection:",a("(Rinchai et al, CTM, 2020).",href="https://onlinelibrary.wiley.com/doi/full/10.1002/ctm2.244"),
                    "and Psoriasis", a("(Rawat et al, Front Immunol, 2020)",href="https://pubmed.ncbi.nlm.nih.gov/33329570/"),". These applications provide further insights and analysis specific to these diseases."),
                  br(),
                  div(img(src = "https://www.mainebiz.biz/sites/default/files/indesign-import/images/GG21_JAX_logo_opt.png", width = 200),
                      img(src = "https://www.hopital-europeen.fr/themes/custom/hem/logo.svg", width = 200),
                      img(src = "https://upload.wikimedia.org/wikipedia/fr/thumb/7/70/Assistance_publique_-_H%C3%B4pitaux_de_Marseille_logo_2017.svg/504px-Assistance_publique_-_H%C3%B4pitaux_de_Marseille_logo_2017.svg.png?20191101162614", width = 150),
                      img(src = "https://nouveau.univ-brest.fr/lbai/sites/lbai.nouveau.univ-brest.fr/files/2021-07/LBAI_600px.jpg", width = 170),
                      img(src = "https://www.chu-brest.fr/sites/default/themes/chrub/logo.png", width =200),
                      img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/1/1b/Aix-Marseille_University_logo.png/250px-Aix-Marseille_University_logo.png", width = 170),
                      style = "text-align: center;"),
                  
                  br()
                  
              )
            )
    ),
    #Aggregate annotation tabitem
    tabItem("aggfunctionannotation",
            br(),
            h3("Functional Annotation"),
            br(),
            p("The composition of the set of 382 modules constituting the repertoire has been shared",a("Gen3 Module lists",href="https://github.com/Drinchai/BloodGen3Module/blob/master/R/sysdata.rda"), 
              "along with extensive functional annotations which are accessible via interactive “circle packing plots”"),
            br(),
            h3("Links to module aggregates annotation pages"),
            fluidRow(
              mainPanel(
                DT::dataTableOutput("imagetest")#, href=dat$infolink, target="_blank")
              )
              
            )),
    ##Fingerprint grid tabitem
    tabItem("gridfingerprint",
            h3("Fingerprint grid of selected dataset"),
            p("Grid plots indicate changes in transcript abundance in patients compared to controls, with red spots showing increase in abundance and blue spot decreases. The position of the modules on the grid is fixed (modules on a given row constitute a “module aggregate”). It permits, using a color code, to indicate functional associations at each position for the different modules on the grid. Once familiar with the manner in which modules are positioned on the grid it becomes straightforward to identify perturbations for a given pathway or cell type (e.g. row A28: interferon response, row A37: erythroid cells). Such a fingerprint grid can be generated for an individual subject or more commonly (as shown in this app) for a group of subjects."),
            fluidRow(
              box(width = NULL,height=650,solidHeader = TRUE,
                  box(width = 4,solidHeader = TRUE,
                      selectInput(  inputId = "groupcutoffInput",
                                    label = "Choose cutoff",
                                    choices = c("FDR < 0.1"),selected ="FDR < 0.1", multiple=F, selectize=TRUE,
                                    width = '80%')),
                  box(width = 4, solidHeader = TRUE,
                      selectInput(inputId = "diseaseInput",
                                  label = "Choose comparison",
                                  choices = c("DA1","DA2","DA3"), multiple=F, selectize=TRUE,
                                  width = '80%')),
                  plotOutput("plot2", height = 500)
              ),
              fluidRow(
                
                box(width = NULL,solidHeader = TRUE,
                    plotOutput("plot_map", height = 500))
              ),
              fluidRow(
                column(width = 4,
                       box(width = NULL,
                           #radioButtons("gidtype",label = "Image",choices = c("png")),
                           downloadButton("gridplot",label = "Download image")
                       )
                ),
                column(width = 3, offset = 1,
                       box(width = NULL, 
                           downloadButton("downloadlist",label = "Download table")    
                       ))
                
              )
            )),
    #Module X study tabitem
    tabItem(tabName = "individualfingerprint", 
            h3("Fingerprint heatmap displaying patterns of annotated modules across individual study subjects"),
            p("Heatmaps represent instead changes in abundance in individual samples (columns) across individual modules (rows). Rows and columns are ordered based on patterns of transcript abundance and therefore are not fixed but change with each analysis that is performed.   "),
            
            fluidRow(
              column(width = 12,
                     box(width = NULL,solidHeader = TRUE,color="darkgrey",
                         box(solidHeader = TRUE,
                             selectInput(inputId = "IllnessInput",
                                         label = "Choose cohort",
                                         choices = c("ALL","ABSENT","MILD","MODERATE","STRONG"),selected ="ALL",multiple=T, selectize=TRUE,
                                         width = '80%')),
                         box(solidHeader = TRUE,
                             selectInput(inputId = "clusterInput",
                                         label = "Choose cluster",
                                         choices = parameter_choise,selected ="IFN_Group",multiple=F, selectize=TRUE,
                                         width = '80%')),
                         box(solidHeader = TRUE,
                             selectInput(inputId = "IndaggregateInput",
                                         label = "Choose aggregate",
                                         choices = Aggregate_Gen3,selected ="A1", multiple=T, selectize=TRUE,
                                         width = '80%')),
                         (div(style='width:2000px;overflow: auto;height:800px;',
                              plotOutput("plot4",height = 1200, width = 3550)))
                     ),
                     fluidRow(
                       column(width = 3,
                              box(width = NULL,
                                  #radioButtons("individualtype",label = "Image",choices = c("png")),
                                  downloadButton("downloadindplot",label = "Download image")
                              )
                       ),
                       column(width = 3, offset = 1,
                              box(width = NULL, 
                                  downloadButton("individualtable",label = "Download table")    
                              ))
                       
                     )
                     
              ))),
    # module X individual tabitem
    tabItem("complexplot",
            h3("Fingerprint heatmap displaying group comparison across studies"),
            p("Heatmaps represent groups of samples (e.g. summarized changes at the level of a patient cohort) and rows groups of modules (e.g. summarized changes at the level of a module aggregate). Rows and columns are ordered based on patterns of transcript abundance and therefore are not fixed but change with each analysis that is performed.   "),
            fluidRow(
              column(width = 12,
                     box(width = NULL,solidHeader = TRUE,
                         box(width = 4,solidHeader = TRUE,
                             selectInput(  inputId = "heatmapcutoffInput",
                                           label = "Choose cutoff",
                                           choices = c("FDR < 0.1"),selected ="FDR < 0.1", multiple=F, selectize=TRUE,
                                           width = '80%')),
                         
                         box(width = 4,solidHeader = TRUE,
                             selectInput(inputId = "aggregateInput",
                                         label = "Choose aggregate",
                                         choices = Aggregate_Gen3, multiple=F, selectize=TRUE,
                                         width = '80%')),
                         plotOutput("plot3",height = 800)
                     ),
                     fluidRow(
                       column(width = 3,
                              box(width = NULL,
                                  #radioButtons("aggregatetypes",label = "Image",choices = c("png")),
                                  downloadButton("aggregateplot",label = "Download image")
                              )
                       ),
                       column(width = 3, offset = 1,
                              box(width = NULL, 
                                  downloadButton("downloadaggregate",label = "Download table")    
                              ))
                       # column(width = 12,
                       #       box(width = NULL, status = "info",
                       #            title = "Percentage response",
                       #           tableOutput("aggregateTable")
                       #        )
                       #)
                     )#fluidrow
                     
              )#Individual column
              
            )),
    tabItem(tabName = "module_individualheatmap",
            fluidRow(
              column(width = 10,offset = 1,
                     box(width = 600,"Expression Transcript X Individuals",status = "warning",
                         selectInput(inputId = "modheatmapInput",
                                     label = "Aggregate",
                                     choices = Aggregate_Gen3,selected =c("A28"), multiple=F, selectize=TRUE,
                                     width = '60%'),
                         plotOutput("mod_heatmapplot",height = 900)
                         
                     )))
    ),
    tabItem(tabName = "boxplot_individual",
            fluidRow(
              box(width = 600,
                  selectInput(inputId = "modboxplotInput",
                              label = "Module",
                              choices = Module_func,selected =c("A28.1| M10.1.Interferon"), multiple=F, selectize=TRUE,
                              width = '60%'))),
            # Title can include an icon
            #title = tagList(shiny::icon("gear"), "tabBox status"),
            fluidRow(
              box(h3("A: IFN group "), height = 500,
                  plotOutput("boxplot_msp",height = "400px")),
              
              box(h3("B: Disease flare"),height = 500,
                  plotOutput("boxplot_us", height = "400px")
              ))#,
            #fluidRow(
            #  box(h3("A: Whole blood "), height = 500,
            #      plotOutput("boxplot_wb",height = "400px")),
              
            #  box(h3("B: PBMCs"),height = 500,
            #      plotOutput("boxplot_pbmc", height = "400px")
            #  ))
            ,
            
            fluidRow(
              box(width = 600,status = "warning",
                  selectInput(inputId = "geneboxplotInput",
                              label = "Gene symbol",
                              choices = unique(CVgene_table_pax$Symbol),selected =c("IFI27"), multiple=F, selectize=TRUE,
                              width = '60%'))),
            fluidRow(
              box(h3("A: IFN group"), height = 500,
                  plotOutput("boxplot_genegroup",height = "400px")),
              
              box(h3("B: Disease flare"),height = 500,
                  plotOutput("boxplot_genebox_his", height = "400px"))
            )#,
            #fluidRow(
            #  box(h3("A: Whole blood "), height = 500,
             #     plotOutput("boxplot_wbgene",height = "400px")),
              
            #  box(h3("B: PBMCs"),height = 500,
            #      plotOutput("boxplot_pbmcgene", height = "400px")
             # ))
    )
    
    
  )#tabItems
    )#dashbody



###/SERVER SIDE/###
server <- function(input, output, session) {
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("calendar"),tabName = "hometab"),
      #menuItem("MODULE CONSTRUCTION", tabName = "moduleconstruction"),
      menuItem("AGGREGATE ANNOTATION", tabName = "aggfunctionannotation"),
      menuItem("FINGERPRINT GRIDS", tabName = "gridfingerprint"),
      menuItem("MODULES X DISEASE", tabName = "complexplot"),
      menuItem("MODULES X INDIVIDUALS", tabName = "individualfingerprint"),
      menuItem("HEATMAP (Transcripts X Individuals)", tabName = "module_individualheatmap"),
      menuItem("BOXPLOT (% Module Response)", tabName = "boxplot_individual"),
      br(),
      h4("Contact", style = "text-align: left;color:black"),
      menuItem("Eleonore Bettacchioli"),
      menuItem("Laurent Chiche",href="https://www.hopital-europeen.fr/professionnels/annuaire-medecins/dr-chiche-laurent?fbclid=IwAR0ZXiWvYlaCwkMbI0bGTGrBdJ5EIww136mMm3Lv_plQ2NfUw-9RZ6aeNWk"),
      menuItem("Noemie Jourde-Chiche"),
       #h4("Contact", style = "text-align: left;color:black"),
      menuItem("Damien Chaussabel",href="https://www.jax.org/people/damien-chaussabel"),
      menuItem("Darawan Rinchai", href = "https://www.researchgate.net/profile/Darawan-Rinchai")
      #p("   The Jackson Laboratory", style = "text-align: left;"),

      
    )
  })
  
  grid_data  <- reactive({
    
    filtered <-
      df_app %>%
      filter(diseases == input$diseaseInput)
  })
  
  # The currently selected tab from the first box
  # The currently selected tab from the first box
  output$plot2 <- renderPlot({
    #filtered <-
    # df_app %>%
    #  filter(diseases == input$diseaseInput)
    
    ggplot(grid_data(), aes(Aggregate, as.factor(sub_aggregate))) +
      geom_tile(color="#E6E6E6" , size = 0.2, fill= color)+
      ggtitle(paste(as.character(unique(grid_data()$diseases)),"vs Healthy control"))+
      #geom_tile(color="#E6E6E6" , size = 0.2, aes(fill=color),show.legend = F)+
      geom_point(aes(colour=`%Response`),size=4.5)+ 
      #scale_fill_manual(values = c("#E6E6E6"="#E6E6E6","white"="white"))+
      ylab("") +
      xlab("") +
      #labs(title= disease)+
      theme(axis.text.x = element_text(angle = -90, hjust = 0))+
      scale_color_gradient2(low = "blue", mid="white", high = "red",limits=c(-100,100),na.value = "#E6E6E6", guide = "colourbar")+
      theme_light() +
      theme(panel.grid.minor = element_line(colour="black", size=0.9))+
      coord_flip() + 
      scale_x_discrete(limits = rev(levels(grid_data()$Aggregate))) +
      theme(panel.border = element_rect(color = "black",size = 0.5),
            axis.text.x = element_text(colour="black",size=9,angle=0,hjust=0.5,vjust=2,face="plain"),
            axis.text.y = element_text(colour="black",size=9,angle=0,hjust=0.5,vjust=0.5,face="plain"))
  },height = 500, width=900,res = 90
  )
  
  
  ###GRID MAP
  output$plot_map <- renderPlot({
    #filtered <-
    # df_app %>%
    #  filter(diseases == input$diseaseInput)
    
    ggplot(melt_test1, aes(Cluster, as.factor(Position),fill = Key)) +
      geom_tile(color="grey" , size = 0.2) +
      scale_fill_manual (values = Gen3_map_ann$color[match(levels(melt_test1$Key),Gen3_map_ann$Function_New)], na.value = "#E6E6E6") +
      scale_color_gradient2(low = "blue", mid="white", high = "red", limits=c(-100,100),na.value = "#E6E6E6", guide = "colourbar")+
      ylab("") +
      xlab("") +
      theme(axis.text.x = element_text(angle = -90, hjust = 0)) +
      theme_light() +
      theme(panel.grid.minor = element_line(colour="white", size=0.9)) +
      coord_flip() + scale_x_discrete(limits = rev(levels(melt_test1$Cluster))) +
      guides(fill=guide_legend(ncol=2))+
      theme(legend.key.size = unit(4, "mm"))+
      theme(legend.text = element_text(colour = 'black', angle = 0, size = 10, hjust = 0, vjust = 0.5))+
      theme(panel.border = element_rect(color = "black",size = 0.5),
            axis.text.x = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.text.y = element_text(colour="black",size=9,angle=0,hjust=1,vjust=0.5,face="plain")) 
  },height = 500, width=1200,res = 90
  )
  
  # grid download
  plotInput = function() {
    ggplot(grid_data(), aes(Aggregate, as.factor(sub_aggregate))) +
      geom_tile(color="#E6E6E6" , size = 0.2, fill= color)+
      ggtitle(paste(as.character(unique(grid_data()$diseases)),"vs Healthy control"))+
      #geom_tile(color="#E6E6E6" , size = 0.2, aes(fill=color),show.legend = F)+
      geom_point(aes(colour=`%Response`),size=4.2)+ 
      #scale_fill_manual(values = c("#E6E6E6"="#E6E6E6","white"="white"))+
      ylab("") +
      xlab("") +
      #labs(title= disease)+
      theme(axis.text.x = element_text(angle = -90, hjust = 0))+
      scale_color_gradient2(low = "blue", mid="white", high = "red",limits=c(-100,100),na.value = "#E6E6E6", guide = "colourbar")+
      theme_light() +
      theme(panel.grid.minor = element_line(colour="black", size=0.9))+
      coord_flip() + 
      scale_x_discrete(limits = rev(levels(grid_data()$Aggregate))) +
      theme(panel.border = element_rect(color = "black",size = 0.5),
            axis.text.x = element_text(colour="black",size=9,angle=0,hjust=0.5,vjust=2,face="plain"),
            axis.text.y = element_text(colour="black",size=9,angle=0,hjust=0.5,vjust=0.5,face="plain"))
  }
  
  
  output$gridplot = downloadHandler(
    filename <- function(){
      paste(paste(input$diseaseInput,sep = "."))
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 9, height = 5,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotInput(), device = device)
    })
  
  
  ###Dowload grid table   
  thegrid = reactive({
    
    filtered <-
      df_app %>%
      filter(diseases == input$diseaseInput)
  })
  
  
  output$downloadlist <- downloadHandler(
    filename = function(){paste(input$diseaseInput,"csv",sep = ".")}, 
    content = function(fname){
      write.csv(thegrid(), fname)
    }
  )
  
  
  ###Group comparison heatmap
  
  output$plot3 <- renderPlot({
    
    filtered2 <- 
      df_app2 %>%
      filter(Aggregate == input$aggregateInput)
    
    rownames(filtered2) = filtered2$Module
    anno_table1 = filtered2
    filtered2$Module = NULL
    
    ##prepare annotation table
    ####################
    Gen3_ann$Module_func = paste(Gen3_ann$position, Gen3_ann$Module_func,sep = "| ")
    
    anno_table1$color = Gen3_ann$Module_color[match(anno_table1$Module,Gen3_ann$Module_func)]
    anno_table1$Function = Gen3_ann$Function[match(anno_table1$Module,Gen3_ann$Module_func)]
    rownames(anno_table1)= anno_table1$Module
    
    group_df = filtered2[rownames(anno_table1),]
    group_df$Aggregate = NULL
    
    plate_color = as.character(anno_table1$color)
    names(plate_color)=anno_table1$Function
    
    left_ha = rowAnnotation(df = data.frame(Module = anno_table1$Function),
                            show_annotation_name = FALSE,simple_anno_size = unit(0.3, "cm"),
                            col = list(Module = plate_color))
    
    #row_ha= rowAnnotation(Cluster = anno_text(rownames(group_df), gp = gpar(fontsize =6)),
    #show_annotation_name = FALSE,simple_anno_size = unit(0.3, "cm"))
    
    
    col_fun = circlize::colorRamp2(c(-100, 0, 100), c("blue", "white", "red"))
    ht=Heatmap(group_df,
               cluster_rows = T,
               cluster_columns = F,
               row_title = input$aggregateInput,
               #col = circlize::colorRamp2(c(-100, 0, 100), c("blue", "white", "red")),
               height = unit(4, "mm")*nrow(group_df), 
               width  = unit(4, "mm")*ncol(group_df), 
               rect_gp = gpar(type = "none"),
               row_dend_width = unit(2, "cm"),
               row_title_rot = 0,
               #column_names_side = "top",
               #column_split = df_app2_ann$Group,
               column_title_rot = 90,
               #row_split = sample_ann$Conditions,
               #right_annotation =row_ha ,
               #border = c(TRUE, lwd = 2),
               left_annotation = left_ha,
               #row_names_side = "right",
               name = "% Response",
               row_names_max_width = unit(10,"in"),
               row_title_gp = gpar(fontsize = 15),
               column_names_gp = gpar(fontsize = 12),
               row_names_gp = gpar(col = "black",fontsize = 12),
               cell_fun = function(j, i, x, y, width, height, fill) {
                 grid.circle(x = x, y = y, r = unit(1.85, "mm") ,gp = gpar(fill = col_fun(group_df[i, j]), col = NA))
               }
    )
    draw(ht,annotation_legend_side = "left", heatmap_legend_side = "left", padding = unit(c(2, 20, 2, 2), "mm"))
    
  })
  
  
  # Aggregate  heatmap download
  output$aggregateplot <- downloadHandler(
    filename <- function(){
      paste(paste(input$aggregateInput,"png",sep = "."))
    },
    
    content <- function(filename){
      #if (input$aggregatetypes == "png")
      png(filename, width = 1700, height = 3000, units ="px", res = 150)
      #else
      #  pdf(filename, width = 1700, height = 3000 )
      
      filtered2 <- 
        df_app2 %>%
        filter(Aggregate == input$aggregateInput
               
        )
      rownames(filtered2) = filtered2$Module
      anno_table1 = filtered2
      filtered2$Module = NULL
      
      ##prepare annotation table
      ####################
      Gen3_ann$Module_func = paste(Gen3_ann$position, Gen3_ann$Module_func,sep = "| ")
      
      anno_table1$color = Gen3_ann$Module_color[match(anno_table1$Module,Gen3_ann$Module_func)]
      anno_table1$Function = Gen3_ann$Function[match(anno_table1$Module,Gen3_ann$Module_func)]
      rownames(anno_table1)= anno_table1$Module
      
      group_df = filtered2[rownames(anno_table1),]
      group_df$Aggregate = NULL
      
      plate_color = as.character(anno_table1$color)
      names(plate_color)=anno_table1$Function
      
      left_ha = rowAnnotation(df = data.frame(Module = anno_table1$Function),
                              show_annotation_name = FALSE,simple_anno_size = unit(0.3, "cm"),
                              col = list(Module = plate_color))
      
      # row_ha= rowAnnotation(Cluster = anno_text(rownames(group_df), gp = gpar(fontsize =6)),
      #                      show_annotation_name = FALSE,simple_anno_size = unit(0.3, "cm"))
      
      
      col_fun = circlize::colorRamp2(c(-100, 0, 100), c("blue", "white", "red"))
      ht=Heatmap(group_df,
                 cluster_rows = T,
                 cluster_columns = T,
                 row_title = input$aggregateInput,
                 col = circlize::colorRamp2(c(-100, 0, 100), c("blue", "white", "red")),
                 height = unit(4, "mm")*nrow(group_df), 
                 width  = unit(4, "mm")*ncol(group_df), 
                 rect_gp = gpar(type = "none"),
                 row_dend_width = unit(2, "cm"),
                 row_title_rot = 0,
                 #column_names_side = "top",
                 #column_split = sample_ann$SampleID_1,
                 #row_split = sample_ann$Conditions,
                 #right_annotation =row_ha ,
                 #border = c(TRUE, lwd = 2),
                 left_annotation = left_ha,
                 #row_names_side = "right",
                 name = "% Response",
                 row_names_max_width = unit(10,"in"),
                 row_title_gp = gpar(fontsize = 15),
                 column_names_gp = gpar(fontsize = 12),
                 row_names_gp = gpar(col = "black",fontsize = 12),
                 cell_fun = function(j, i, x, y, width, height, fill) {
                   grid.circle(x = x, y = y, r = unit(1.85, "mm") ,gp = gpar(fill = col_fun(group_df[i, j]), col = NA))
                 }
      )
      draw(ht,annotation_legend_side = "left", heatmap_legend_side = "left", padding = unit(c(2, 20, 2, 2), "mm"))
      dev.off()
    }
    
  )
  ######
  
  
  
  thedata = reactive({
    filtered2 <- 
      df_app2 %>%
      filter(Aggregate == input$aggregateInput)
  })
  
  
  output$downloadaggregate <- downloadHandler(
    filename = function(){paste(input$aggregateInput,"csv",sep = ".")}, 
    content = function(fname){
      write.csv(thedata(), fname)
    }
  )
  
  
  ###Heatmap X individual 
  
  
  output$plot4 <- renderPlot({
    
    AGG = input$IndaggregateInput
    Gen3_ann$Module_func = paste(Gen3_ann$position, Gen3_ann$Module_func,sep = "| ")
    Aggregate_selected = Gen3_ann[Gen3_ann$Cluster%in%AGG,]
    
    
    if(input$IllnessInput== ("ALL")){
      data_selected = c("ABSENT","MILD","MODERATE","STRONG")
    }else{
      data_selected = input$IllnessInput
    }
    
    if(input$IllnessInput== ("ALL")){
      r_cm = 1.85 
      width_cm = 4.2
      col_cm = 10
      row_cm = 12
    }else{
      data_selected = input$IllnessInput
      r_cm = 1.85 
      width_cm = 4.2
      col_cm = 10
      row_cm = 12
    }
    
    df_ind <- 
      Ind_table %>%
      filter(Illness %in% data_selected) %>%
      select(sampleID,Aggregate_selected$Module_func)
    
    
    
    #df_ind = df_ind[,-grep(colnames(df_ind),pattern = "TBD")]
    #df_ind = df_ind[,colSums(df_ind != 0) > nrow(df_ind)*0.2] 
    rownames(df_ind) = df_ind$sampleID
    
    sample_info = sample_app[rownames(df_ind),]
    #Heatmap_titile = as.character(unique(df_ind$Illness))
    #df_ind$Illness = NULL
    df_ind$sampleID = NULL
    
    ##prepare annotation table
    ####################
    anno_table = Gen3_ann[which(Gen3_ann$Module_func%in%colnames(df_ind)),]
    rownames(anno_table)= anno_table$Module_func
    
    df_ind = df_ind[,rownames(anno_table)]
    
    rownames(df_ind) = sample_info$Samplename
    df_plot = t(df_ind)
    colnames(df_plot) = sample_info$Samplename
    
    plate_color = as.character(anno_table$Module_color)
    names(plate_color)=anno_table$Function
    
    ha_column= rowAnnotation(df = data.frame(Module = anno_table$Function),
                             show_annotation_name = FALSE,simple_anno_size = unit(0.3, "cm"),
                             col = list(Module = plate_color))
    
    #lgd_list = list(
    #  Legend(labels = c("MV","ICU","Hospital"), title = "Days", pch = 16, 
    #         legend_gp = gpar(fill =  c("#6EBAC5","#FBAB3D","#D0605E"))))
    
    
    
    col_column = HeatmapAnnotation(SLEDAI = anno_barplot(as.numeric(sample_info$SLEDAI),
                                                              #ICU = as.numeric(sample_info$ICU_stay),
                                                              #Hosital=as.numeric(sample_info$Hospital_stay)),
                                                           bar_width = 1, 
                                                           gp = gpar(col = "white", fill = c("#6EBAC5")), #,"#FBAB3D","#D0605E"
                                                           border = T,
                                                          show_legend=T,
                                                           height = unit(2, "cm"),gap = unit(0.5, "mm")),
                                   CS_dose = anno_barplot(as.numeric(sample_info$`CS daily dose`),
                                                          #ICU = as.numeric(sample_info$ICU_stay),
                                                          #Hosital=as.numeric(sample_info$Hospital_stay)),
                                                          bar_width = 1, 
                                                          gp = gpar(col = "white", fill = c("#E49343")), #,"#FBAB3D","#D0605E"
                                                          border = T,
                                                          show_legend=T,
                                                          height = unit(2, "cm"),gap = unit(0.5, "mm")),
                                 #  CSmgkg = anno_barplot(as.numeric(sample_info$`CS mgkg`),
                                                          #ICU = as.numeric(sample_info$ICU_stay),
                                                          #Hosital=as.numeric(sample_info$Hospital_stay)),
                                                     #     bar_width = 1, 
                                                     #     gp = gpar(col = "white", fill = c("#C67694")), #,"#FBAB3D","#D0605E"
                                                     #     border = T,
                                                     #     show_legend=T,
                                                       #   height = unit(2, "cm"),gap = unit(0.5, "mm")),
                                                          gp = gpar(col = "white", lwd = 0.01),
                                                            border = T,
                                                  df = data.frame(Conditions = sample_info$Condition,
                                                   Timepoint = sample_info$Visit,
                                                   Gender = sample_info$Gender,
                                                   IFN_Group = sample_info$`IFN group`,
                                                   Flare = sample_info$Flare,
                                                   SKIN = sample_info$Skin,
                                                   JOINT = sample_info$Joints,
                                                   HEMATO = sample_info$Hemato,
                                                   Nephropathy_classes= sample_info$Nephropathy_classes,
                                                   Renal = sample_info$Renal,
                                                   aRNP = sample_info$aRNP,
                                                   aSSB = sample_info$aSSB,
                                                   aSm = sample_info$aSm,
                                                   aSSA = sample_info$aSSA), 
                                   show_annotation_name = T,simple_anno_size = unit(0.4, "cm"),
                                   col = list(Conditions = c("SLE-NoFlare"="#6EBAC5","SLE-mildModerateFlare"="#FBAB3D","SLE-SevereFlare"="#D0605E"),
                                              #Study = c("Whole-blood"="#1C9D75","PBMCs"="#756FB3"),
                                              Timepoint=c("1"="#76D6FF","2"="#FF7F00",
                                                          "3"="#FEFB01","4"="#113fc7","5"="#941751",
                                                          "6"="#D6D6D6"),
                                              Gender = c("M"="#0096FF","F"="#F1C2D1"),
                                              IFN_Group = c("ABSENT"="#5878A3","MILD"="#FBAB3D","MODERATE"="#B19A42","STRONG"="#CDA7C6"),
                                              Flare = c("Y"="firebrick3","N"="#5878A3","ND"="#EBEBEB"),
                                              SKIN = c("Y"="#C67694","N"="#5878A3","ND"="#EBEBEB"),
                                              JOINT = c("Y"="#977763","N"="#5878A3"),
                                              HEMATO = c("Y"="#E49343","N"="#5878A3"),
                                              Nephropathy_classes= c("C"="#569EAB","II"="#B0C297","III"="#E1C75A","IV"="#9437FF","V"="#DF482A","III+V"="#E837FF","IV+V"="#FF85FF"
                                                                     ,"IN"="#009051","ND"="#EBEBEB"),
                                              Renal = c("Y"="#F2A19D","N"="#5878A3"),
                                              aRNP = c("Y"="#C67694","N"="#5878A3","ND"="#EBEBEB"),
                                              aSSB = c("Y"="#EBCF74","N"="#5878A3","ND"="#EBEBEB"),
                                              aSm = c("Y"="#A87C9F","N"="#5878A3","ND"="#EBEBEB"),
                                              aSSA = c("Y"="#A87C9F","N"="#5878A3","ND"="#EBEBEB")))
                                              #Outcomes = c("Alive"="#5E9693", "DEAD"="#D0605E")))
    
    
    sample_info$Visit = factor(sample_info$Visit, levels = c("1","2","3","4","5","6"))
    
    
    if(input$clusterInput== ("Condition")){
      splited_sel = sample_info$`IFN group` 
      col_cluster = FALSE
      col_degree= 90
    }else{
      splited_sel = input$clusterInput
      splited_sel = sample_info[,splited_sel]
      col_cluster = TRUE
      col_degree = 0
    }
    
    col_fun = circlize::colorRamp2(c(-100, 0, 100), c("blue", "white", "red"))
    ht2=Heatmap(df_plot,
                cluster_rows = T,
                cluster_columns = col_cluster,
                cluster_column_slices = F,
                #column_title = input$IllnessInput,
                col = circlize::colorRamp2(c(-100, 0, 100), c("blue", "white", "red")),
                height = unit(width_cm, "mm")*ncol(df_ind), 
                width  = unit(width_cm, "mm")*nrow(df_ind), 
                rect_gp = gpar(type = "none"),
                row_dend_width = unit(2, "cm"),
                row_title_rot = 0,
                column_title_rot = col_degree,
                column_title_gp = gpar(fontsize = 16),
                #column_names_side = "top",
                column_split = splited_sel,
                row_split = anno_table$Cluster,
                #right_annotation =row_ha ,
                #border = c(TRUE, lwd = 2),
                top_annotation = col_column,
                left_annotation = ha_column,
                row_names_side = "right",
                name = "% Response",
                row_names_max_width = unit(10,"in"),
                row_title_gp = gpar(fontsize = 15),
                column_names_gp = gpar(fontsize = col_cm),
                row_names_gp = gpar(col = "black",fontsize = row_cm),
                cell_fun = function(j, i, x, y, width, height, fill) {
                  grid.circle(x = x, y = y, r = unit(r_cm, "mm") ,gp = gpar(fill = col_fun(df_plot[i, j]), col = NA))
                }
    )
    draw(ht2,   annotation_legend_side = "left", heatmap_legend_side = "left", padding = unit(c(2, 20, 2, 2), "mm"))
    #annotation_legend_list = lgd_list,
  })
  
  # individual  heatmap download
  output$downloadindplot <- downloadHandler(
    filename <- function(){
      paste(paste(input$IllnessInput,input$IndaggregateInput,"png",sep = "."))
    },
    
    content <- function(filename){
      #if (input$individualtype == "png")
      png(filename, width = 10000, height = 3000, units ="px", res = 150)
      #else
      # pdf(filename, width = 1700, height = 3000 )
      
      AGG = input$IndaggregateInput
      Gen3_ann$Module_func = paste(Gen3_ann$position, Gen3_ann$Module_func,sep = "| ")
      Aggregate_selected = Gen3_ann[Gen3_ann$Cluster%in%AGG,]
      
      
      if(input$IllnessInput== ("ALL")){
        data_selected = c("ABSENT","MILD","MODERATE","STRONG")
      }else{
        data_selected = input$IllnessInput
      }
      
      df_ind <- 
        Ind_table %>%
        filter(Illness %in% data_selected) %>%
        select(sampleID,Aggregate_selected$Module_func)
      
      
      
      #df_ind = df_ind[,-grep(colnames(df_ind),pattern = "TBD")]
      #df_ind = df_ind[,colSums(df_ind != 0) > nrow(df_ind)*0.2] 
      rownames(df_ind) = df_ind$sampleID
      sample_info = sample_app[rownames(df_ind),]
      #Heatmap_titile = as.character(unique(df_ind$Illness))
      #df_ind$Illness = NULL
      df_ind$sampleID = NULL
      
      ##prepare annotation table
      ####################
      anno_table = Gen3_ann[which(Gen3_ann$Module_func%in%colnames(df_ind)),]
      rownames(anno_table)= anno_table$Module_func
      
      df_ind = df_ind[,rownames(anno_table)]
      rownames(df_ind) = sample_info$Samplename
      
      df_plot = t(df_ind)
      colnames(df_plot) = sample_info$Samplename
      
      
      plate_color = as.character(anno_table$Module_color)
      names(plate_color)=anno_table$Function
      
      ha_column= rowAnnotation(df = data.frame(Module = anno_table$Function),
                               show_annotation_name = FALSE,simple_anno_size = unit(0.3, "cm"),
                               col = list(Module = plate_color))
      
      #lgd_list = list(
       # Legend(labels = c("MV","ICU","Hospital"), title = "Days", pch = 16, 
       #        legend_gp = gpar(fill =  c("#6EBAC5","#FBAB3D","#D0605E"))))
      
      
      
      col_column = HeatmapAnnotation(SLEDAI = anno_barplot(as.numeric(sample_info$SLEDAI),
                                                           #ICU = as.numeric(sample_info$ICU_stay),
                                                           #Hosital=as.numeric(sample_info$Hospital_stay)),
                                                           bar_width = 1, 
                                                           gp = gpar(col = "white", fill = c("#6EBAC5")), #,"#FBAB3D","#D0605E"
                                                           border = T,
                                                           show_legend=T,
                                                           height = unit(2, "cm"),gap = unit(0.5, "mm")),
                                     CS_dose = anno_barplot(as.numeric(sample_info$`CS daily dose`),
                                                            #ICU = as.numeric(sample_info$ICU_stay),
                                                            #Hosital=as.numeric(sample_info$Hospital_stay)),
                                                            bar_width = 1, 
                                                            gp = gpar(col = "white", fill = c("#E49343")), #,"#FBAB3D","#D0605E"
                                                            border = T,
                                                            show_legend=T,
                                                            height = unit(2, "cm"),gap = unit(0.5, "mm")),
                                   #  CSmgkg = anno_barplot(as.numeric(sample_info$`CS mgkg`),
                                                           #ICU = as.numeric(sample_info$ICU_stay),
                                                           #Hosital=as.numeric(sample_info$Hospital_stay)),
                                    #                       bar_width = 1, 
                                   #                        gp = gpar(col = "white", fill = c("#C67694")), #,"#FBAB3D","#D0605E"
                                  #                         border = T,
                                  #                         show_legend=T,
                                   #                        height = unit(2, "cm"),gap = unit(0.5, "mm")),
                                     gp = gpar(col = "white", lwd = 0.01),
                                     border = T,
                                     df = data.frame(Conditions = sample_info$Condition,
                                                     Timepoint = sample_info$Visit,
                                                     Gender = sample_info$Gender,
                                                     IFN_Group = sample_info$`IFN group`,
                                                     Flare = sample_info$Flare,
                                                     SKIN = sample_info$Skin,
                                                     JOINT = sample_info$Joints,
                                                     HEMATO = sample_info$Hemato,
                                                     Nephropathy_classes= sample_info$Nephropathy_classes,
                                                     Renal = sample_info$Renal,
                                                     aRNP = sample_info$aRNP,
                                                     aSSB = sample_info$aSSB,
                                                     aSm = sample_info$aSm,
                                                     aSSA = sample_info$aSSA), 
                                     show_annotation_name = T,simple_anno_size = unit(0.4, "cm"),
                                     col = list(Conditions = c("SLE-NoFlare"="#6EBAC5","SLE-mildModerateFlare"="#FBAB3D","SLE-SevereFlare"="#D0605E"),
                                                #Study = c("Whole-blood"="#1C9D75","PBMCs"="#756FB3"),
                                                Timepoint=c("1"="#76D6FF","2"="#FF7F00",
                                                            "3"="#FEFB01","4"="#113fc7","5"="#941751",
                                                            "6"="#D6D6D6"),
                                                Gender = c("M"="#0096FF","F"="#F1C2D1"),
                                                IFN_Group = c("ABSENT"="#5878A3","MILD"="#FBAB3D","MODERATE"="#B19A42","STRONG"="#CDA7C6"),
                                                Flare = c("Y"="firebrick3","N"="#5878A3","ND"="#EBEBEB"),
                                                SKIN = c("Y"="#C67694","N"="#5878A3","ND"="#EBEBEB"),
                                                JOINT = c("Y"="#977763","N"="#5878A3"),
                                                HEMATO = c("Y"="#E49343","N"="#5878A3"),
                                                Nephropathy_classes= c("C"="#569EAB","II"="#B0C297","III"="#E1C75A","IV"="#9437FF","V"="#DF482A","III+V"="#E837FF","IV+V"="#FF85FF"
                                                                                           ,"IN"="#009051","ND"="#EBEBEB"),
                                                Renal = c("Y"="#F2A19D","N"="#5878A3"),
                                                aRNP = c("Y"="#C67694","N"="#5878A3","ND"="#EBEBEB"),
                                                aSSB = c("Y"="#EBCF74","N"="#5878A3","ND"="#EBEBEB"),
                                                aSm = c("Y"="#A87C9F","N"="#5878A3","ND"="#EBEBEB"),
                                                aSSA = c("Y"="#A87C9F","N"="#5878A3","ND"="#EBEBEB")))
      #Outcomes = c("Alive"="#5E9693", "DEAD"="#D0605E")))
      
      
      sample_info$Visit = factor(sample_info$Visit, levels = c("1","2","3","4","5","6"))
      
      if(input$clusterInput== ("IFN group")){
        splited_sel = sample_info$`IFN group` 
        col_cluster = FALSE
        col_degree= 90
      }else{
        splited_sel = input$clusterInput
        splited_sel = sample_info[,splited_sel]
        col_cluster = TRUE
        col_degree = 0
      }
      
      col_fun = circlize::colorRamp2(c(-100, 0, 100), c("blue", "white", "red"))
      ht2=Heatmap(df_plot,
                  cluster_rows = T,
                  cluster_columns = col_cluster,
                  cluster_column_slices = F,
                  #column_title = input$IllnessInput,
                  #col = circlize::colorRamp2(c(-100, 0, 100), c("blue", "white", "red")),
                  height = unit(2.1, "mm")*ncol(df_ind), 
                  width  = unit(2.1, "mm")*nrow(df_ind), 
                  rect_gp = gpar(type = "none"),
                  row_dend_width = unit(2, "cm"),
                  row_title_rot = 0,
                  column_title_gp = gpar(fontsize = 16),
                  #column_names_side = "top",
                  column_split = splited_sel,
                  row_split = anno_table$Cluster,
                  #right_annotation =row_ha ,
                  #border = c(TRUE, lwd = 2),
                  top_annotation = col_column,
                  left_annotation = ha_column,
                  row_names_side = "right",
                  name = "% Response",
                  row_names_max_width = unit(10,"in"),
                  row_title_gp = gpar(fontsize = 15),
                  column_names_gp = gpar(fontsize = 4),
                  row_names_gp = gpar(col = "black",fontsize = 7),
                  cell_fun = function(j, i, x, y, width, height, fill) {
                    grid.circle(x = x, y = y, r = unit(0.905, "mm") ,gp = gpar(fill = col_fun(df_plot[i, j]), col = NA))
                  }
      )
      draw(ht2, annotation_legend_side = "left", heatmap_legend_side = "left", padding = unit(c(2, 20, 2, 2), "mm"))
      dev.off()
    }
    
  )
  
  
  
  ##download individual table      
  theIND = reactive({
    AGG = input$IndaggregateInput
    Aggregate_selected = Gen3_ann[Gen3_ann$Cluster%in%AGG,]
    
    
    if(input$IllnessInput== ("ALL")){
      data_selected = c("ABSENT","MILD","MODERATE","STRONG")
    }else{
      data_selected = input$IllnessInput
    }
    
    
    df_ind <- 
      Ind_table %>%
      filter(Illness == data_selected) %>%
      select(sampleID,Aggregate_selected$Module_func)
  })
  
  
  output$individualtable <- downloadHandler(
    filename = function(){paste(data_selected,input$IndaggregateInput,"csv",sep = ".")}, 
    content = function(fname){
      write.csv(theIND(), fname)
    }
  )
  
  
  ##############################
  # Generate a module heatmap of individual data ----
  output$mod_heatmapplot <- renderPlot({
    
    AGG = input$modheatmapInput
    Aggregate_selected = Gen3_ann[Gen3_ann$Cluster%in%AGG,]
    
    CVexp_table_pax <- 
      CVexp_table_pax %>%
      filter(Aggregate == input$modheatmapInput) 
    
    rownames(CVexp_table_pax) = CVexp_table_pax$Module_gene
    anno_table5 = CVexp_table_pax
    CVexp_table_pax$Module_gene = NULL
    
    ##prepare annotation 
    anno_table5 = Module_listGen3[which(Module_listGen3$Module_gene%in%rownames(anno_table5)),]
    rownames(anno_table5) = anno_table5$Module_gene
    
    df_selected_all  = CVexp_table_pax[rownames(anno_table5),]
    df_selected_all$Aggregate = NULL
    
    sample_info = sample_PT
    sample_info$`IFN group` = factor(sample_info$`IFN group`,levels = c("ABSENT","MILD","MODERATE","STRONG"))
    sample_info = sample_info[order(sample_info$Patient),]
    sample_info = sample_info[order(sample_info$Visit),]
    sample_info = sample_info[order(sample_info$`IFN group`),]
  
    
    df_selected = df_selected_all[,rownames(sample_info)]
    colnames(df_selected) ==rownames(sample_info)
    
    ES2 =as.matrix(df_selected)
    
    ESz2 <- ES2
    for(i in 1: nrow(ES2))
    {
      ESz2[i,]<- (ES2[i,]-mean(ES2[i,]))/sd(ES2[i,])
    }
   
    
    df_plot_all =  ESz2
    
    
    colnames(df_plot_all)==rownames(sample_info)
    anno_table5 = anno_table5[rownames(df_plot_all),]
    
    
    ha_column3 = HeatmapAnnotation( df = data.frame(Conditions = sample_info$Condition,
                                                    Timepoint = sample_info$Visit,
                                                    Gender = sample_info$Gender,
                                                    IFN_Group = sample_info$`IFN group`,
                                                    Flare = sample_info$Flare,
                                                    SKIN = sample_info$Skin,
                                                    JOINT = sample_info$Joints,
                                                    HEMATO = sample_info$Hemato,
                                                    Nephropathy_classes= sample_info$Nephropathy_classes,
                                                    Renal = sample_info$Renal,
                                                    aRNP = sample_info$aRNP,
                                                    aSSB = sample_info$aSSB,
                                                    aSm = sample_info$aSm,
                                                    aSSA = sample_info$aSSA), 
                                    show_annotation_name = F,simple_anno_size = unit(0.3, "cm"),
                                    col = list(Conditions = c("SLE-NoFlare"="#6EBAC5","SLE-mildModerateFlare"="#FBAB3D","SLE-SevereFlare"="#D0605E"),
                                               #Study = c("Whole-blood"="#1C9D75","PBMCs"="#756FB3"),
                                               Timepoint=c("1"="#76D6FF","2"="#FF7F00",
                                                           "3"="#FEFB01","4"="#113fc7","5"="#941751",
                                                           "6"="#D6D6D6"),
                                               Gender = c("M"="#0096FF","F"="#F1C2D1"),
                                               IFN_Group = c("ABSENT"="#5878A3","MILD"="#FBAB3D","MODERATE"="#B19A42","STRONG"="#CDA7C6"),
                                               Flare = c("Y"="firebrick3","N"="#5878A3","ND"="#EBEBEB"),
                                               SKIN = c("Y"="#C67694","N"="#5878A3","ND"="#EBEBEB"),
                                               JOINT = c("Y"="#977763","N"="#5878A3"),
                                               HEMATO = c("Y"="#E49343","N"="#5878A3"),
                                               Nephropathy_classes= c("C"="#569EAB","II"="#B0C297","III"="#E1C75A","IV"="#9437FF","V"="#DF482A","III+V"="#E837FF","IV+V"="#FF85FF"
                                                                                                               ,"IN"="#009051","ND"="#EBEBEB"),
                                               Renal = c("Y"="#F2A19D","N"="#5878A3"),
                                               aRNP = c("Y"="#C67694","N"="#5878A3","ND"="#EBEBEB"),
                                               aSSB = c("Y"="#EBCF74","N"="#5878A3","ND"="#EBEBEB"),
                                               aSm = c("Y"="#A87C9F","N"="#5878A3","ND"="#EBEBEB"),
                                               aSSA = c("Y"="#A87C9F","N"="#5878A3","ND"="#EBEBEB")))
    
    
    sample_info$Visit = factor(sample_info$Visit, levels = c("1","2","3","4","5","6"))
    
    
    
    ##left annotation
    ##Prepare annotation
    
    if (nrow(anno_table5)==0) {next}
    
    df_plot_all = df_plot_all[rownames(anno_table5),]
    rownames(anno_table5)==rownames(df_plot_all)
    
    plate_color3 = as.character(anno_table5$color)
    names(plate_color3)=anno_table5$Function
    
    row_ha3 = rowAnnotation(df = data.frame(Function = anno_table5$Function), 
                            show_annotation_name = FALSE,simple_anno_size = unit(0.3, "cm"),
                            col = list(Function =plate_color3))
    
    
    rownames(df_plot_all)
    ht= Heatmap(df_plot_all, 
                name = "z score", 
                col = circlize::colorRamp2(c(-1.5, 0, 1.5), c("#7BAFB9", "#231F23", "#F1E84D")),
                cluster_rows = T,
                cluster_columns = T,
                cluster_column_slices = FALSE,
                #cluster_columns = T,
                show_row_names = F,
                #height = unit(2, "mm")*nrow(df_plot2), 
                #width  = unit(1, "mm")*ncol(df_plot2), 
                row_split = anno_table5$Module,
                #row_title = unique(anno_table$Module),
                column_split = sample_info$`IFN group`,
                column_title_gp = gpar(fontsize = 0),
                #column_names_side = "top",
                show_column_names = FALSE,
                row_title_gp = gpar(fontsize = 16),
                row_title_rot = 0,
                #column_names_gp = gpar(fontsize =2),
                #row_names_gp = gpar(fontsize = 3),
                top_annotation = ha_column3,
                left_annotation = row_ha3)
    
    draw(ht,heatmap_legend_side = "right", annotation_legend_side = "right", padding = unit(c(2, 20, 2, 2), "mm"))
    
  })
  
  
  ##boxplot module
  mod_indplot  <- reactive({
    Pregmod_table_WB= Pregmod_table
    filtered_mod <-
      Pregmod_table_WB %>%
      filter(Module_func == input$modboxplotInput)
  })
          
  my.comparison = list(c("ABSENT", "MILD"), c("ABSENT", "MODERATE"),c("ABSENT", "STRONG"))
  
  output$boxplot_msp <- renderPlot({
    ggplot(mod_indplot(), aes(x=Group,y=Percentage))+
      geom_boxplot(aes(x=Group,y=Percentage,fill=Group),width = 0.5,outlier.shape = NA)+
      geom_jitter(color="black",alpha=0.4,width = 0.1)+
      ggtitle(input$modboxplotInput)+
      ylab("Percentage response")+
      ylim(-100,150)+
      scale_fill_manual(values = c("ABSENT"="#76D6FF","MILD"="#FF7F00",
                                   "MODERATE"="#FEFB01","STRONG"="#113fc7"))+#,"TP4"="#941751",
                                   #"TP5"="#D6D6D6"))+
      scale_x_discrete(name = "IFN group", limits=c("ABSENT","MILD","MODERATE","STRONG"))+
      stat_compare_means(method = "t.test",paired = FALSE, 
                         comparisons = my.comparison, 
                         label = "p.signif",size =4,label.y = c(80,95,110,125,135))+
      stat_compare_means(label = "p.signif", method = "t.test",
                         ref.group = "Healthy",label.y =140)+
      theme_bw() +
      theme(strip.background = element_blank(),
            legend.title=element_text(size=20),
            strip.text = element_text(face="bold", size=16),
            legend.text = element_text(size=16),
            plot.title = element_text(lineheight=.8,size = 20,face = "bold"),
            axis.text.x = element_text(colour="black",size=20,angle=0,hjust=0.5,vjust=1,face="plain"),
            axis.text.y = element_text(colour="black",size=20,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title.y = element_text(colour="black",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
            legend.position = "none", aspect.ratio = 1/1.5)
    
  }, bg="transparent")
  
  output$boxplot_wb <- renderPlot({
    ggplot(mod_indplot(), aes(x=Condition,y=Percentage))+
      geom_boxplot(aes(x=Condition,y=Percentage,fill=Condition),width = 0.5,outlier.shape = NA)+
      ggtitle(input$modboxplotInput)+
      ylab("Percentage response")+
      ylim(-100,150)+
      #scale_fill_manual(values = c("Healthy"="#76D6FF","TP1"="#FF7F00",
       #                            "TP2"="#FEFB01","TP3"="#113fc7","TP4"="#941751",
      #                             "TP5"="#D6D6D6"))+
      scale_x_discrete(name = "IFN group", limits=c("NoFlare","mildModerateFlare","SevereFlare"))+
      stat_compare_means(aes(group = Condition), label = "p.format")+
      #stat_compare_means(method = "t.test",paired = FALSE, 
           #              comparisons = my.comparison, 
           #             label = "p.signif",size =4,label.y = c(80,95,110,125,135))+
      theme_bw() +
      theme(strip.background = element_blank(),
            legend.title=element_text(size=20),
            strip.text = element_text(face="bold", size=16),
            legend.text = element_text(size=16),
            plot.title = element_text(lineheight=.8,size = 20,face = "bold"),
            axis.text.x = element_text(colour="black",size=20,angle=0,hjust=0.5,vjust=1,face="plain"),
            axis.text.y = element_text(colour="black",size=20,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title.y = element_text(colour="black",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
            legend.position = "bottom", aspect.ratio = 1/1.5)
    
  }, bg="transparent")
  
  ##boxplot RNAseq
  
  mod_indplot_us  <- reactive({
    Pregmod_table_PBMCs= Pregmod_table
    filtered_mod <-
      Pregmod_table_PBMCs %>%
      filter(Module_func == input$modboxplotInput)
    
  })
  
  output$boxplot_us <- renderPlot({
    my.comparison2 = list(c("NoFlare", "mildModerateFlare"), c("NoFlare", "SevereFlare"),c("mildModerateFlare", "SevereFlare"))
    
    ggplot(mod_indplot_us(), aes(x=Condition,y=Percentage))+
      geom_boxplot(aes(x=Condition,y=Percentage,fill=Condition),width = 0.5,outlier.shape = NA)+
      geom_jitter(color="black",alpha=0.4,width = 0.1)+
      ggtitle(input$modboxplotInput)+
      ylab("Percentage response")+
      ylim(-100,150)+
      scale_fill_manual(values = c("NoFlare"="#76D6FF","mildModerateFlare"="#FF7F00",
                                   "SevereFlare"="#FEFB01"))+#,"TP4"="#941751",
      #"TP5"="#D6D6D6"))+
      scale_x_discrete(name = "Condition", limits=c("NoFlare","mildModerateFlare","SevereFlare"))+
      stat_compare_means(method = "t.test",paired = FALSE, 
                         comparisons = my.comparison2, 
                         label = "p.signif",size =5,label.y = c(115,135,115,135))+
      stat_compare_means(label = "p.signif", method = "t.test",
                         ref.group = "Healthy",label.y =140)+
      theme_bw() +
      theme(strip.background = element_blank(),
            legend.title=element_text(size=20),
            strip.text = element_text(face="bold", size=16),
            legend.text = element_text(size=16),
            plot.title = element_text(lineheight=.8,size = 20,face = "bold"),
            axis.text.x = element_text(colour="black",size=18,angle=0,hjust=0.5,vjust=1,face="plain"),
            axis.text.y = element_text(colour="black",size=20,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title.y = element_text(colour="black",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
            legend.position = "none", aspect.ratio = 1/1.5)
    
  })
  
  #severity plot
  output$boxplot_pbmc <- renderPlot({
    
    ggplot(mod_indplot_us(), aes(x=Condition,y=Percentage))+
      geom_boxplot(aes(x=Condition,y=Percentage,fill=Condition),width = 0.5,outlier.shape = NA)+
      ggtitle(input$modboxplotInput)+
      ylab("Percentage response")+
      ylim(-100,150)+
      #scale_fill_manual(values = c( "Healthy"="#76D6FF","TP1"="#FF7F00",
      #                              "TP2"="#FEFB01","TP3"="#113fc7","TP4"="#941751",
       #                             "TP5"="#D6D6D6"))+
      scale_x_discrete(name = "IFN group", limits=c("NoFlare","mildModerateFlare","SevereFlare"))+
      stat_compare_means(aes(group = Condition), label = "p.format")+
      theme_bw() +
      theme(strip.background = element_blank(),
            legend.title=element_text(size=20),
            strip.text = element_text(face="bold", size=16),
            legend.text = element_text(size=16),
            plot.title = element_text(lineheight=.8,size = 20,face = "bold"),
            axis.text.x = element_text(colour="black",size=18,angle=0,hjust=0.5,vjust=1,face="plain"),
            axis.text.y = element_text(colour="black",size=20,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title.y = element_text(colour="black",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
            legend.position = "bottom", aspect.ratio = 1/1.5)
    
  })
  
  ##Box gene level
  gene_indplot  <- reactive({
    filtered_gene <-
      CVgene_table_pax %>%
      filter(Symbol== input$geneboxplotInput)
  })
  
  my.comparison = list(c("ABSENT", "MILD"), c("ABSENT", "MODERATE"),c("ABSENT", "STRONG"))
  
  output$boxplot_genegroup <- renderPlot({
    ggplot(gene_indplot(), aes(x=Group,y=log2expression))+
      geom_boxplot(aes(x=Group,y=log2expression,fill=Group),width = 0.5,outlier.shape = NA)+
      geom_jitter(color="black",alpha=0.4,width = 0.1)+
      ggtitle(input$geneboxplotInput)+
      ylab("Normalization count (log2)")+
      ylim(0,20)+
      scale_fill_manual(values = c("ABSENT"="#76D6FF","MILD"="#FF7F00",
                                   "MODERATE"="#FEFB01","STRONG"="#113fc7"))+#,"TP4"="#941751",
      #"TP5"="#D6D6D6"))+
      scale_x_discrete(name = "IFN group", limits=c("ABSENT","MILD","MODERATE","STRONG"))+
      stat_compare_means(method = "t.test",paired = FALSE, 
                         comparisons = my.comparison, 
                         label = "p.signif",size =4,label.y = c(12,14,16,18,20))+
      stat_compare_means(label = "p.signif", method = "t.test",
                         ref.group = "Healthy",label.y =18)+
      theme_bw() +
      theme(strip.background = element_blank(),
            legend.title=element_text(size=20),
            strip.text = element_text(face="bold", size=16),
            legend.text = element_text(size=16),
            plot.title = element_text(lineheight=.8,size = 20,face = "bold"),
            axis.text.x = element_text(colour="black",size=20,angle=0,hjust=0.5,vjust=1,face="plain"),
            axis.text.y = element_text(colour="black",size=20,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title.y = element_text(colour="black",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
            legend.position = "none", aspect.ratio = 2/3)
    
  }, bg="transparent")
  
  #severity gene
  output$boxplot_wbgene <- renderPlot({
    
    ggplot(gene_indplot(), aes(x=Condition,y=log2expression))+
      geom_boxplot(aes(x=Condition,y=log2expression,fill=Condition),width = 0.5,outlier.shape = NA)+
      #geom_jitter(color="black",alpha=0.4,width = 0.1)+
      ggtitle(input$geneboxplotInput)+
      ylab("Normalization count (log2)")+
      ylim(0,20)+
      #scale_fill_manual(values = c("Healthy"="#76D6FF","TP1"="#FF7F00",
      #                             "TP2"="#FEFB01","TP3"="#113fc7","TP4"="#941751",
      #                             "TP5"="#D6D6D6"))+
      scale_x_discrete(name = "IFN group", limits=c("NoFlare","mildModerateFlare","SevereFlare"))+
      stat_compare_means(aes(group = Condition), label = "p.format")+
      theme_bw() +
      theme(strip.background = element_blank(),
            legend.title=element_text(size=20),
            strip.text = element_text(face="bold", size=16),
            legend.text = element_text(size=16),
            plot.title = element_text(lineheight=.8,size = 20,face = "bold"),
            axis.text.x = element_text(colour="black",size=20,angle=0,hjust=0.5,vjust=1,face="plain"),
            axis.text.y = element_text(colour="black",size=20,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title.y = element_text(colour="black",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
            legend.position = "bottom", aspect.ratio = 2/3)
  
  })
  ##boxplot RNAseq
  
  gene_indplot_us  <- reactive({
    CVgene_table_pax = CVgene_table_pax[-grep(CVgene_table_pax$Condition,pattern = "Healthy control of SLE"),]
    filtered_mod <-
      CVgene_table_pax %>%
      filter(Symbol == input$geneboxplotInput)
  })
####HERE#####  
  output$boxplot_genebox_his <- renderPlot({
    my.comparison2 = list(c("NoFlare", "mildModerateFlare"), c("NoFlare", "SevereFlare"),c("mildModerateFlare", "SevereFlare"))
    
    ggplot(gene_indplot_us(), aes(x=Condition,y=log2expression))+
      geom_boxplot(aes(x=Condition,y=log2expression,fill=Condition),width = 0.5,outlier.shape = NA)+
      geom_jitter(color="black",alpha=0.4,width = 0.1)+
      ggtitle(input$geneboxplotInput)+
      ylab("Normalization count (log2)")+
      ylim(0,20)+
      scale_fill_manual(values = c("NoFlare"="#76D6FF","mildModerateFlare"="#FF7F00",
                                   "SevereFlare"="#FEFB01"))+#,"TP4"="#941751",
      #"TP5"="#D6D6D6"))+
      scale_x_discrete(name = "Condition", limits=c("NoFlare","mildModerateFlare","SevereFlare"))+
      stat_compare_means(method = "t.test",paired = FALSE, 
                         comparisons = my.comparison2, 
                         label = "p.signif",size =5,label.y = c(15,17,15,15))+
      stat_compare_means(label = "p.signif", method = "t.test",
                         ref.group = "Healthy",label.y =14)+
      theme_bw() +
      theme(strip.background = element_blank(),
            legend.title=element_text(size=20),
            strip.text = element_text(face="bold", size=16),
            legend.text = element_text(size=16),
            plot.title = element_text(lineheight=.8,size = 20,face = "bold"),
            axis.text.x = element_text(colour="black",size=18,angle=0,hjust=0.5,vjust=1,face="plain"),
            axis.text.y = element_text(colour="black",size=20,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title.y = element_text(colour="black",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
            legend.position = "none", aspect.ratio = 2/3)
    
  })
  
output$boxplot_pbmcgene <- renderPlot({
  ggplot(gene_indplot_us(), aes(x=Timepoint,y=log2expression))+
    geom_boxplot(aes(x=Timepoint,y=log2expression,fill=Outcomes),width = 0.5,outlier.shape = NA)+
    #geom_jitter(color="black",alpha=0.4,width = 0.1)+
    ggtitle(input$geneboxplotInput)+
    ylab("Normalization count (log2)")+
    ylim(0,20)+
    #scale_fill_manual(values = c("Healthy"="#76D6FF","TP1"="#FF7F00",
    #                             "TP2"="#FEFB01","TP3"="#113fc7","TP4"="#941751",
    #                             "TP5"="#D6D6D6"))+
    scale_x_discrete(name = "IFN group", limits=c("SLE-NoFlare","SLE-mildModerateFlare","SLE-SevereFlare"))+
    stat_compare_means(aes(group = Outcomes), label = "p.format")+
    theme_bw() +
    theme(strip.background = element_blank(),
          legend.title=element_text(size=20),
          strip.text = element_text(face="bold", size=16),
          legend.text = element_text(size=16),
          plot.title = element_text(lineheight=.8,size = 20,face = "bold"),
          axis.text.x = element_text(colour="black",size=18,angle=0,hjust=0.5,vjust=1,face="plain"),
          axis.text.y = element_text(colour="black",size=20,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.position = "bottom", aspect.ratio = 2/3)
})
  
  #################################LINK#############################################
  
  ###URL ourput
  url <- a("COVID19 APP", href="https://drinchai.shinyapps.io/COVID_19_project/")
  output$tab <- renderUI({
    tagList(url)
  })
  
  rsvurl <- a("RSV APP", href="https://drinchai.shinyapps.io/RSV_Meta_Module_analysis/")
  output$rsvtab <- renderUI({
    tagList(rsvurl)
  })
  ###URL GXB
  gxburl <- a("16 Diseases", href="http://cd2k.gxbsidra.org/dm3/geneBrowser/list")
  output$gxbtab <- renderUI({
    tagList(gxburl)
  })
  
  gxb16url <- a("16 Diseases", href="http://cd2k.gxbsidra.org/dm3/geneBrowser/list")
  output$gxb16tab <- renderUI({
    tagList(gxb16url)
  })
  shinyurl <- a("Shiny app", href="https://drinchai.shinyapps.io/dc_gen3_module_analysis/")
  output$shinytab <- renderUI({
    tagList(shinyurl)
  })
  
  
  ###URL GXB
  preziurl <- a("Module aggregate annotation", href="https://prezi.com/view/6FalfBDmwqezlDW0MTrY/")
  output$prezitab <- renderUI({
    tagList(preziurl)
  })
  ###URL BloodGen3Module
  packageurl <- a("BloodGen3Module", href="https://github.com/Drinchai/BloodGen3Module")
  output$packagetab <- renderUI({
    tagList(packageurl)
  })
  packageinfourl <- a("BloodGen3Module", href="https://github.com/Drinchai/BloodGen3Module/blob/master/README.md")
  output$packageinfotab <- renderUI({
    tagList(packageinfourl)
  })
  
  
  ###aggregate link
  prezi_url <- a("“circle packing plots”", onclick = "openTab('aggfunctionannotation')", href="#")
  output$prezi_tab <- renderUI({
    tagList(prezi_url)
  })
  
  ##Aggregate annotation table 
  hlink <-apply(dat[,-c(1:2)],1, function(x){
    as.character(a(HTML(x[["Links"]]), href=x[["Links"]], target="_blank"))
  })      
  
  dat$URL <- hlink
  output$imagetest <- DT::renderDataTable({
    DT::datatable(dat[,-c(3,4)], escape = FALSE)
  })
  
  ##16 Diseases datasets table 
  hlink <-apply(dat16_dis,1, function(x){
    as.character(a(HTML(x[["URL"]]), href=x[["URL"]], target="_blank"))
  })      
  
  dat16_dis$URL <- hlink
  output$datsetlink <- DT::renderDataTable({
    DT::datatable(dat16_dis, escape = FALSE)
  })
  ###Infobox link
  output$preziout <- renderInfoBox({
    infoBox(title = "Annotation", 
            a("Prezi", onclick = "openTab('aggfunctionannotation')", href="#"),
            width = 3,color = "purple",icon = icon("fas fa-atom"))
  })
  
  
  
}


# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "LUPUCE: SLE",titleWidth = 250),
  dashboardSidebar(width = 270,
    sidebarMenuOutput("menu")
  ),
  body
)

# Preview the UI in the console
shinyApp(ui = ui, server )

