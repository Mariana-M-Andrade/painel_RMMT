library("dplyr")
library("shinydashboard")
library("readr")
library("shiny")
library("summarytools")
library("highcharter")
library("pander")
library("tidyverse")
library("janitor")
library("shiny")
library("reactable")
library("reactablefmtr")
library("openxlsx")
library("gtsummary")
library("gt")
library("glue")
library("ggplot2")
library("geobr")

df_mt_desc <- read.csv("df_mt_desc_tardia.csv") 
obitos_mt_ofc <- read.csv("obitos_mt_ofc_tardia.csv")
sinasc <- read.csv("sinasc.csv")
sinasc_mapa <- read.csv("sinasc_mapa.csv")
br_uf <- read_state(year = 2020, simplified = TRUE)

#Filtrando as bases
df_mt_desc <- df_mt_desc %>%
  filter(ano >= 2015 & ano<= 2022) %>%
  filter(capitulo_cid10 != "XX - Causas externas de morbidade e de mortalidade") %>%
  filter(idade >= 10 & idade <= 49) %>%
  mutate(categoria = str_sub(causabas_categoria, end = 3))

obitos_mt_ofc <- obitos_mt_ofc %>%
  filter(ano >= 2015 & ano<= 2022) %>%
  filter(idade >= 10 & idade <= 49) %>%
  mutate(categoria = str_sub(causabas_categoria, end = 3))

sinasc <- sinasc %>%
  rename(estado = uf)%>%
  rename(uf = sigla_uf) %>%
  filter(ano >= 2015 & ano<= 2022)


# Juntando as bases de Óbitos desconsiderados e Óbitos oficiais
all_obitos <- bind_rows(df_mt_desc, obitos_mt_ofc)

# Mudando o tipo da variável
all_obitos$ano <- as.character(all_obitos$ano)
sinasc$ano <- as.character(sinasc$ano)
sinasc_mapa$ano <- as.character(sinasc_mapa$ano)

# Agrupando por ano e uf, e somando os obitos
all_obitos <- all_obitos %>%
  group_by(ano, uf, regiao, idade, racacor, categoria) %>%
  summarise(obitos = sum(obitos, na.rm = TRUE), .groups = 'drop')




ui <- dashboardPage(
  title= "Morte Materna Tardia",
  dashboardHeader(title = strong('Morte Materna Tardia'),
                  titleWidth=450
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sobre", tabName = "sobre", icon = icon("info")),
      menuItem("Óbitos Maternos Tardios", tabName = "obitos", icon = icon("table")),
      menuItem("Mapa", tabName = "mapa", icon = icon("map")),
      menuItem("Análises sociodemográficas", tabName = "analises", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML('
    /* logo */
    .skin-blue .main-header .logo {
      background-color: #0A1E3C;
    }

    /* logo when hovered */
    .skin-blue .main-header .logo:hover {
      background-color: #0A1E3C;
    }

    /* navbar (rest of the header) */
    .skin-blue .main-header .navbar {
      background-color: #0A1E3C;
    }        

    /* main sidebar */
    .skin-blue .main-sidebar {
      background-color: #0A1E3C;
    }

    /* active selected tab in the sidebarmenu */
    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
      background-color: #32A0FF;
    }

    /* other links in the sidebarmenu */
    .skin-blue .main-sidebar .sidebar .sidebar-menu a {
      background-color: #0A1E3C;
      color: #FFFFFF;
    }

    /* other links in the sidebarmenu when hovered */
    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
      background-color: #32A0FF;
    }

    /* toggle button when hovered  */                    
    .skin-blue .main-header .navbar .sidebar-toggle:hover {
      background-color: #32A0FF;
    }
  ')),
      
      # Insert the logo in the panel header
      tags$style(HTML('
    .main-header .logo {
      background-image: url("logo-oobr.png");
      background-size: contain;
      background-repeat: no-repeat;
      background-position: left center;
    }
  '),
      
      # Horizontal rule style
      HTML("hr {border-top: 1px solid #0A1E3C;}")
    )),
    
    tabItems(
      # Página "Sobre"
      tabItem(tabName = "sobre",
              HTML('
        <h3 style="font-weight: bold;">Morte Materna Tardia</h3>
        <p>É considerado uma morte materna tardia o óbito materno que ocorre no período do puerpério tardio, em outras palavras, entre 43 dias a 1 ano após o parto. Os óbitos maternos tardios não são considerados no cálculo da Razão de Mortalidade Materna (RMM).</p>

        <h3 style="font-weight: bold;">Razão de Mortalidade Materna Tardia</h3>
        <p>Visto que o cálculo da RMM não inclui mortes maternas ocorridas no puerpério tardio, a maneira utilizada para medir a situação é calcular uma nova taxa utilizando apenas óbitos maternos tardios e seguindo o mesmo modelo de cálculo da RMM.</p>

        <h3 style="font-weight: bold;">Os dados</h3>
        <p>O painel conta com dados disponibilizados pelo Ministério da Saúde no DATASUS, utilizando as bases "Sistema de Informação de Mortalidade" (SIM) e "Sistema de Informação de Nascidos Vivos (SINASC), ambas bases públicas.</p>
        <p>Para esse estudo, as bases foram filtradas, constando dados do período de 2015 a 2022.</p>
        <p>Os dados utilizados neste painel podem ser acessados em: Herzog, R. S.; Francisco, R. P. V.; Rodrigues, A. S. Óbitos de gestantes e puérperas [banco de dados], 2022, Observatório Obstétrico Brasileiro (OOBr). Disponível em DOI: <a href="https://doi.org/10.7303/syn42902915" target="_blank">https://doi.org/10.7303/syn42902915</a>.</p>

        <h3 style="font-weight: bold;">Realização</h3>
        <center>
        <img src="logos/realizacao/logo_oobr.png" width="10%">
        <img src="logos/realizacao/logo_ufes.png" width="10%">
        <img src="logos/realizacao/logo_medicina_usp.png" width="10%">
        <img src="logos/realizacao/logo_daslab.png" width="10%">
        </center>

        <h3 style="font-weight: bold;">Financiadores</h3>
        <center>
        <img src="logos/financiadores/logo_bill_melinda.png" width="10%">
        <img src="logos/financiadores/logo_cnpq.png" width="10%">
        <img src="logos/financiadores/logo_ms1.png" width="10%">
        <img src="logos/financiadores/logo_ms2.png" width="10%">
        <img src="logos/financiadores/logo_ms3.png" width="10%">
        <img src="logos/financiadores/logo_fapes.png" width="10%">
        <img src="logos/financiadores/logo_fapes2.png" width="10%">
        </center>
      ')
      ),
      
      # Página "Óbitos Maternos Tardios"
      tabItem(tabName = "obitos",
              fluidRow(
                # Coluna de filtros 
                column(4, 
                       span("Filtros", style = "font-weight: bold; font-size: 17px"),
                       HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                       
                       box(width = NULL,
                           
                           span("Temporalidade e localidade", style = "font-weight: bold; font-size: 17px"),
                           HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                           
                           
                           numericInput(
                             inputId = "selectAnoOM",
                             label = HTML("<span class = 'negrito'> Selecione o ano de análise: </span>"),
                             value = max(all_obitos$ano),
                             min = min(all_obitos$ano),
                             max = max(all_obitos$ano)
                           ),
                           
                           selectInput(
                             inputId = "selectNivelOM",
                             label = HTML("<span class = 'negrito'> Selecione o nível de análise: </span>"),
                             choices = c("Nacional", "Estadual"),
                             selected = "Nacional"
                           ),
                           
                           conditionalPanel(
                             condition = "input.selectNivelOM == 'Nacional'",
                             checkboxGroupInput(
                               inputId = "selectRegiaoOM",
                               label = HTML("<span class = 'negrito'> Selecione as regiões de análise: </span>"),
                               choices = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
                               selected = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
                             )
                           ),
                           
                           conditionalPanel(
                             condition = "input.selectNivelOM == 'Estadual'",
                             selectInput(
                               inputId = "selectEstadoOM",
                               label = HTML("<span class = 'negrito'> Selecione o estado: </span>"),
                               choices = sort(unique(all_obitos$uf)),
                               selected = sort(unique(all_obitos$uf))
                             )
                           ),
                           
                           span("Características da puérpera", style = "font-weight: bold; font-size: 17px"),
                           HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                           
                           sliderInput(
                             inputId = "selectIdadeOM",
                             label = HTML("<span class = 'negrito'> Selecione a faixa etária a ser considerada: </span>"),
                             min = 0,
                             max = 99,
                             value = c(10, 49)
                           ),
                           
                           checkboxGroupInput(
                             inputId = "selectRacaOM",
                             label = HTML("<span class = 'negrito'> Selecione a Raça/cor da puérpera: </span>"),
                             choices = sort(unique(all_obitos$racacor)),
                             selected = sort(unique(all_obitos$racacor))
                           ),
                           
                           selectInput(
                             inputId = "selectDownloadOM",
                             label = HTML("<span class = 'negrito'> Deseja fazer o download dessa tabela? </span>"),
                             choices = c("Não", "Sim"),
                             selected = "Não"
                           ),
                           
                           conditionalPanel(
                             condition = "input.selectDownloadOM == 'Sim'",
                             selectInput(
                               inputId = "selectArquivoOM",
                               label = HTML("<span class = 'negrito'> Selecione o tipo do arquivo: </span>"),
                               choices = c("CSV", "XLSX"),
                               selected = "CSV"
                             ),
                             conditionalPanel(
                               condition = "input.selectArquivoOM == 'CSV'",
                               downloadHandler(
                                 filename = reactive({
                                   case_when(
                                     input$selectNivelOM == "Nacional" ~ paste0("obitos_maternos_", janitor::make_clean_names(input$selectNivelOM), input$selectAnoOM, ".csv"),
                                     input$selectNivelOM == "Estadual" ~ paste0("obitos_maternos_", janitor::make_clean_names(input$selectEstadoOM), input$selectAnoOM, ".csv")
                                   )
                                 }),
                                 content = function(file) {
                                   write.table(dados_om_filtrados(), file, sep = ";", row.names = FALSE, fileEncoding = "UTF-8")
                                 }
                               )
                             ),
                             conditionalPanel(
                               condition = "input.selectArquivoOM == 'XLSX'",
                               downloadHandler(
                                 filename = reactive({
                                   case_when(
                                     input$selectNivelOM == "Nacional" ~ paste0("obitos_maternos_", janitor::make_clean_names(input$selectNivelOM), input$selectAnoOM, ".xlsx"),
                                     input$selectNivelOM == "Estadual" ~ paste0("obitos_maternos_", janitor::make_clean_names(input$selectEstadoOM), input$selectAnoOM, ".xlsx")
                                   )
                                 }),
                                 content = function(file) {
                                   write.xlsx(dados_om_filtrados(), file, sep = ";", row.names = FALSE, fileEncoding = "UTF-8")
                                 }
                               )
                             )
                           )
                       )
                ),
                
                # Coluna para a tabela e o gráfico
                column(8,
                       
                       span("Tabela Óbitos Maternos Tardios", style = "font-weight: bold; font-size: 17px"),
                       HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                       
                       
                       fluidPage(
                         box(width = NULL, reactableOutput("table1"))
                       )
                )
              )
      ),
      

    tabItem(tabName = "mapa",
            sidebarLayout(
              sidebarPanel(
                span("Filtros", style = "font-weight: bold; font-size: 17px"),
                HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                
                box(width = NULL,
                    span("Temporalidade", style = "font-weight: bold; font-size: 17px"),
                    HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                    
                    numericInput(
                      inputId = "selectAnoMAPA",
                      label = HTML("<span class = 'negrito'> Selecione o ano de análise: </span>"),
                      value = max(all_obitos$ano),
                      min = min(all_obitos$ano),
                      max = max(all_obitos$ano)
                    ),
                    
                    span("Características da puérpera", style = "font-weight: bold; font-size: 17px"),
                    HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                    
                    sliderInput(
                      inputId = "selectIdadeMAPA",
                      label = HTML("<span class = 'negrito'> Selecione a faixa etária a ser considerada: </span>"),
                      min = 0,
                      max = 99,
                      value = c(10, 49)
                    ),
                    
                    checkboxGroupInput(
                      inputId = "selectRacaMAPA",
                      label = HTML("<span class = 'negrito'> Selecione a Raça/cor da puérpera: </span>"),
                      choices = sort(unique(all_obitos$racacor)),
                      selected = sort(unique(all_obitos$racacor))
                    )
                )
              ),
              
              mainPanel(
                span("Mapa - Razão de Mortalidade Materna Tardia", style = "font-weight: bold; font-size: 17px"),
                HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                
                box(width = NULL, plotOutput("mapaRMM"))
              )
            )
    ),
      
      tabItem(tabName = "analises",
              
              sidebarLayout(
                sidebarPanel(
                  span("Filtros", style = "font-weight: bold; font-size: 17px"),
                  HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                  
                  box(width = NULL,
                      span("Temporalidade e localidade", style = "font-weight: bold; font-size: 17px"),
                      HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                      
                      numericInput(
                        inputId = "selectAnoOMT",
                        label = HTML("<span class = 'negrito'> Selecione o ano de análise: </span>"),
                        value = max(all_obitos$ano),
                        min = min(all_obitos$ano),
                        max = max(all_obitos$ano)
                      ),
                      
                      selectInput(
                        inputId = "selectNivelOMT",
                        label = HTML("<span class = 'negrito'> Selecione o nível de análise: </span>"),
                        choices = c("Nacional", "Estadual"),
                        selected = "Nacional"
                      ),
                      
                      conditionalPanel(
                        condition = "input.selectNivelOMT == 'Nacional'",
                        checkboxGroupInput(
                          inputId = "selectRegiaoOMT",
                          label = HTML("<span class = 'negrito'> Selecione as regiões de análise: </span>"),
                          choices = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
                          selected = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.selectNivelOMT == 'Estadual'",
                        selectInput(
                          inputId = "selectEstadoOMT",
                          label = HTML("<span class = 'negrito'> Selecione o estado: </span>"),
                          choices = sort(unique(all_obitos$uf)),
                          selected = sort(unique(all_obitos$uf))
                        )
                      ))
                   ),    
                
                mainPanel(
                  span("Gráficos sociodemográficos", style = "font-weight: bold; font-size: 17px"),
                  HTML("<span style='display: block; margin-bottom: 7px;'> </span>"),
                  
                  box(width = NULL, highchartOutput("plot1")),
                  box(width = NULL, highchartOutput("plot2"))
                )
              
     
              
      )
    )
  )
  ))  


server <- function(input, output, session) {
 
  
#Dados filtrados para a tabela   
  dados_om_filtrados <- reactive({
    if (input$selectNivelOM == "Nacional") {
      if (length(input$selectRegiaoOM) == 5) {
        all_obitos |>
          filter(ano == input$selectAnoOM,
                 racacor %in% input$selectRacaOM,
                 idade >= input$selectIdadeOM[1] & idade <= input$selectIdadeOM[2]) |>
          mutate(regiao = "Todas") |>
          arrange(ano)
      } else {
        all_obitos |>
          filter(ano == input$selectAnoOM,
                 regiao %in% input$selectRegiaoOM,
                 racacor %in% input$selectRacaOM,
                 idade >= input$selectIdadeOM[1] & idade <= input$selectIdadeOM[2]) |>
          arrange(ano)
      }
    } else {
      all_obitos |>
        filter(ano == input$selectAnoOM,
               uf %in% input$selectEstadoOM,
               racacor %in% input$selectRacaOM,
               idade >= input$selectIdadeOM[1] & idade <= input$selectIdadeOM[2]) |>
        arrange(ano)
    }
  })

  
#Output tabela  
  
  output$table1 <- renderReactable({
    reactable(
      dados_om_filtrados(),
      filterable = TRUE,
      searchable = TRUE,
      columns = list(
        ano = colDef(name = "Ano"),
        uf = colDef(name = "UF"),
        regiao = colDef(name = "Região"),
        idade = colDef(name = "Idade"),
        racacor = colDef(name = "Raça/Cor"),
        categoria = colDef(name = "Categoria"),
        obitos = colDef(
          name = "Óbitos",
          aggregate = "sum",  
          footer = JS(
            "function(colInfo) {
                        let obitosTotais = 0
                        colInfo.data.forEach(function(row) {
                          obitosTotais += row['obitos']
                        })
                        return obitosTotais
                        }"
          )
        )
      ),
      defaultColDef = colDef(
        footerStyle = list(fontWeight = "bold")  
      ),
      rowStyle = JS(
        "function(rowInfo) {
          if (rowInfo.aggregated === true) {
           return { fontWeight: 700 }
          }
        }"
      )
    )
  })
 
#Dado filtrados para o mapa   

# Calculando a RMM tardia com os inputs
  obitos_filtrados <- reactive({
    all_obitos |>
      filter(ano == input$selectAnoMAPA,
             racacor %in% input$selectRacaMAPA,
             idade >= input$selectIdadeMAPA[1] & idade <= input$selectIdadeMAPA[2])
  })
  
  nascidos_filtrados <- reactive({
    sinasc_mapa |>
      filter(ano == input$selectAnoMAPA,
             racacor %in% input$selectRacaMAPA)
  })
  
  
  # Juntando as bases de Óbitos e nascidos vivos
  obit_nasc <- reactive({
    obitos <- obitos_filtrados()
    nascidos <- nascidos_filtrados()
    
    obit_nasc <- left_join(obitos, nascidos, by = c("ano", "uf")) %>%
      filter(!is.na(uf)) %>%
      group_by(ano, uf) %>%
      summarise(
        obitos = sum(as.numeric(obitos), na.rm = TRUE),
        nascidos_vivos = sum(as.numeric(nascidos_vivos), na.rm = TRUE)
      ) %>%
      mutate(rmm = round((obitos / nascidos_vivos) * 100000, 2)) %>%
      ungroup()
  })
  
  # Criando o mapa
  output$mapaRMM <- renderPlot({
    dados_mapa <- obit_nasc()
    
    # Juntando os dados de RMM com os dados geográficos das UFs
    br_uf1 <- reactive(left_join(br_uf, dados_mapa, by = c("abbrev_state" = "uf")))
    
    ggplot(data = br_uf1()) +
      geom_sf(aes(fill = rmm), color = "white") +
      scale_fill_gradient(low = "lightgreen", high = "purple", na.value = "grey50", name = "RMM Tardia") +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  #ANÁLISES SÓCIODEMOGRAFICAS 
  
  #Filtrando os dados
  
  omt_filtrados <- reactive({
    req(input$selectNivelOMT, input$selectRegiaoOMT)
    
    if (input$selectNivelOMT == "Nacional") {
      if (length(input$selectRegiaoOMT) == 5) {
        all_obitos |>
          filter(ano == input$selectAnoOMT
                 ) |>
          mutate(regiao = "Todas") |>
          arrange(ano)
      } else {
        all_obitos |>
          filter(ano == input$selectAnoOMT,
                 regiao %in% input$selectRegiaoOMT) |>
          arrange(ano)
      }
    } else {
      all_obitos |>
        filter(ano == input$selectAnoOMT,
               uf %in% input$selectEstadoOMT) |>
        arrange(ano)
    }
  })
  
  
  nasc_filtrados <- reactive({
    req(input$selectNivelOMT)
    if (input$selectNivelOMT == "Nacional") {
        sinasc_mapa |>
          filter(ano == input$selectAnoOMT) |>
          arrange(ano)
    } else {
      sinasc_mapa |>
        filter(ano == input$selectAnoOMT,
               uf %in% input$selectEstadoOMT) |>
        arrange(ano)
    }
  })
  

  #Calculando RMM tardia por raça/cor  
  
  dados_rc <- reactive({
    omt <- omt_filtrados() 
    nasc <- nasc_filtrados() 

    req(omt, nasc)
    
    # Agrupando por ano, uf, e mantendo as variáveis idade e racacor
    omt <- omt %>%
      group_by(ano, uf, regiao, idade, racacor) %>% 
      summarise(obitos = sum(obitos, na.rm = TRUE), .groups = 'drop')
    
    dados_rc <- left_join(omt, nasc, by = c("ano", "uf", "racacor"))
    
    # Remove NA
    dados_rc <- dados_rc %>%
      filter(!is.na(uf)) 
    
  })
    
    # Criando a variável RMM
    rmmt_rc <- reactive({
      dados_rc() %>%
        group_by(racacor) %>%
        summarise(
          obitos = sum(as.numeric(obitos), na.rm = TRUE),
          nascidos_vivos = sum(as.numeric(nascidos_vivos), na.rm = TRUE)
        ) %>%
        mutate(rmm = round((obitos / nascidos_vivos) * 100000, 2)) 
    })
  
  # O mesmo ajuste para rmmt_idade
  dados_idade <- reactive({
    omt <- omt_filtrados() 
    nasc <- nasc_filtrados() 

    
    # Agrupando e mantendo idade
    omt <- omt %>%
      group_by(ano, uf, regiao, idade) %>%  # Inclua racacor se necessário
      summarise(obitos = sum(obitos, na.rm = TRUE), .groups = 'drop')
    
    dados_idade <- left_join(omt, nasc, by = c("ano", "uf"))
    
    # Remove NA
    dados_idade <- dados_idade %>%
      filter(!is.na(uf))
  })
    
    # Criando a variável RMM
rmmt_idade <- reactive({
  dados_idade() %>%
    group_by(idade) %>%
      summarise(
        obitos = sum(as.numeric(obitos), na.rm = TRUE),
        nascidos_vivos = sum(as.numeric(nascidos_vivos), na.rm = TRUE)
      ) %>%
      mutate(rmm = round((obitos / nascidos_vivos) * 100000, 2)) 
})
    

  
  
  # Gráfico RMM tardia por idade
  output$plot1 <- highcharter::renderHighchart({

    
    # Verifique se as colunas 'idade' e 'rmm' existem no dataframe
    if ("idade" %in% colnames(rmmt_idade()) & "rmm" %in% colnames(rmmt_idade())) {
      # Definindo um vetor de cores 
      cols <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928")
      
      hchart(rmmt_idade(), 
             type = "point",
             hcaes(x = idade, y = rmm)) %>%
        hc_xAxis(title = list(text = "Idade")) %>%
        hc_yAxis(title = list(text = "RMM Tardia")) %>%
        hc_add_theme(hc_theme_elementary()) %>%
        hc_colors(cols)  
    } else {
      # Notifique se as colunas não existirem
      showNotification("As variáveis 'idade' ou 'rmm' não foram encontradas nos dados.", type = "error")
    }
  })
  
  
  
  # Gráfico RMM tardia por raça/cor
  output$plot2 <- highcharter::renderHighchart({

    
    # Verifique se as colunas 'racacor' e 'rmm' existem no dataframe
    if ("racacor" %in% colnames(rmmt_rc() ) & "rmm" %in% colnames(rmmt_rc() )) {
      # Definindo um vetor de cores 
      cols <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928")
      
      hchart(rmmt_rc() , 
             type = "column",
             hcaes(x = racacor, y = rmm)) %>%
        hc_xAxis(title = list(text = "Raça/cor")) %>%
        hc_yAxis(title = list(text = "RMM Tardia")) %>%
        hc_add_theme(hc_theme_elementary()) %>%
        hc_colors(cols)  
    } else {
      # Notifique se as colunas não existirem
      showNotification("As variáveis 'racacor' ou 'rmm' não foram encontradas nos dados.", type = "error")
    }
  })
  
  
  
}

shinyApp(ui, server)
