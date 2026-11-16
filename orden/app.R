library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(shinyscreenshot)

# 1. INICIALIZACIÓN DE TABLAS --------------------------------------------------
if (!file.exists("catalogo_nuwa.csv")) {
  write.csv(tibble(
    categoria = c("Horneos", "Horneos", "Horneos", "Adicionales"),
    descripcion = c("Horneo pavo grande", "Horneo pavo mediano", "Horneo pavo pequeño", "Servicio de entrega"),
    precio_unitario = c(30.00, 25.00, 20.00, 3.00)
  ), "catalogo_nuwa.csv", row.names = FALSE)
}

if (!file.exists("ordenes_nuwa.csv")) {
  write.csv(tibble(
    n_orden = character(), fecha_orden = character(), cliente_nombre = character(),
    cliente_id = character(), telefono = character(), fecha_entrega = character(),
    hora_entrega = character(), tipo_entrega = character(), direccion_entrega = character(),
    observacion = character(), metodo_pago = character(), estado = character(),
    total = numeric(), abono = numeric(), saldo = numeric()
  ), "ordenes_nuwa.csv", row.names = FALSE)
}

if (!file.exists("detalle_ordenes_nuwa.csv")) {
  write.csv(tibble(
    n_orden = character(), cantidad = numeric(), descripcion = character(),
    precio_unitario = numeric(), subtotal = numeric()
  ), "detalle_ordenes_nuwa.csv", row.names = FALSE)
}

# 2. INTERFAZ VISUAL POS -------------------------------------------------------
ui <- page_navbar(
  title = tags$span(
    tags$strong("NUWA fusion"),
    tags$small(" | POS & Gestión", style = "opacity: 0.8; font-weight: normal;")
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#5B2C6F",
    success = "#1E8449",
    warning = "#D68910",
    danger = "#C0392B",
    base_font = font_google("Inter")
  ),
  
  nav_spacer(),
  
  # PESTAÑA 1: PUNTO DE VENTA (TOMA DE PEDIDOS)
  nav_panel(
    "Tomar Pedido (POS)",
    icon = icon("cash-register"),
    div(
      class = "container-fluid", style = "max-width: 1350px; padding-top: 10px;",
      
      # Barra superior de búsqueda por ID o Nombre
      div(
        class = "bg-white p-2 mb-3 border rounded shadow-sm",
        fluidRow(
          column(3, textInput("buscar_id", NULL, placeholder = "Buscar por ID o Nombre...", width = "100%")),
          column(2, actionButton("btn_buscar_cliente", "Buscar Cliente", class = "btn-info text-white w-100 mb-2 mb-md-0")),
          column(3, uiOutput("selector_orden_editar")),
          column(2, uiOutput("selector_modo_orden")),
          column(2, actionButton("btn_nueva_orden", "+ Nueva Orden", class = "btn-outline-secondary w-100"))
        )
      ),
      
      fluidRow(
        # COLUMNA IZQUIERDA (5/12): 1° LOGÍSTICA Y ESTADO -> 2° AGREGAR PRODUCTOS
        column(
          5,
          div(
            class = "bg-white p-3 mb-3 border rounded shadow-sm",
            h6(icon("truck"), " LOGÍSTICA Y ESTADO", style = "color: #5B2C6F; font-weight: bold; margin-bottom: 12px;"),
            fluidRow(
              column(6, selectInput("tipo_entrega", "Tipo Entrega:", choices = c("Local", "Envío"), width = "100%")),
              column(6, selectInput("metodo_pago", "Método de Pago:", choices = c("Efectivo", "Transferencia", "Tarjeta", "Pendiente"), width = "100%"))
            ),
            fluidRow(
              column(6, selectInput("estado_orden", "Estado:", choices = c("Recibido", "En Cocina", "Listo", "Entregado"), width = "100%")),
              column(6, textInput("hora_entrega", "Hora Entrega:", placeholder = "", width = "100%"))
            ),
            conditionalPanel(
              condition = "input.tipo_entrega == 'Envío'",
              textInput("direccion_entrega", "Dirección Exacta de Envío:", placeholder = "", width = "100%")
            ),
            textInput("observacion", "Nota para el cliente:", placeholder = "", width = "100%")
          ),
          
          div(
            class = "bg-white p-3 mb-3 border rounded shadow-sm",
            h6(icon("utensils"), " AGREGAR PRODUCTOS", style = "color: #5B2C6F; font-weight: bold; margin-bottom: 12px;"),
            fluidRow(
              column(3, numericInput("item_cant", "Cant:", value = 1, min = 1, width = "100%")),
              column(6, uiOutput("selector_producto_manual")),
              column(3, numericInput("item_precio", "Precio:", value = 1.00, min = 0, step = 0.5, width = "100%"))
            ),
            actionButton("btn_agregar_item", "+ Añadir a la Orden", class = "btn-success w-100 mt-1")
          )
        ),
        
        # COLUMNA DERECHA (7/12): ORDEN DE PEDIDO COMPLETA
        column(
          7,
          div(
            class = "rounded shadow-sm border",
            div(
              class = "bg-primary text-white p-3 rounded-top d-flex justify-content-between align-items-center",
              div(
                div("ORDEN DE PEDIDO", style = "font-weight: bold; font-size: 1.35em; letter-spacing: 0.5px;"),
                div("NUWA fusion • Quito - Ecuador", style = "font-size: 0.9em; opacity: 0.9;")
              ),
              uiOutput("badge_n_orden")
            ),
            
            tags$fieldset(
              id = "fieldset_formulario",
              style = "border: none; margin: 0; padding: 0;",
              div(
                class = "bg-white p-4 rounded-bottom",
                
                fluidRow(
                  column(4, dateInput("fecha_orden", "Fecha Pedido:", value = Sys.Date(), format = "dd/mm/yyyy", width = "100%")),
                  column(4, textInput("cliente_nombre", "Cliente:", placeholder = "", width = "100%")),
                  column(4, textInput("cliente_id", "ID / Cédula:", placeholder = "", width = "100%"))
                ),
                fluidRow(
                  column(4, textInput("telefono", "Teléfono:", placeholder = "", width = "100%")),
                  column(4, dateInput("fecha_entrega", "Fecha Entrega:", value = Sys.Date() + 1, format = "dd/mm/yyyy", width = "100%")),
                  column(4, div(class = "pt-4 text-end", uiOutput("resumen_logistica_badge")))
                ),
                
                uiOutput("bloque_info_orden_ui"),
                
                hr(class = "my-3"),
                div(
                  class = "mb-2",
                  h6("DETALLE DE CONSUMO", style = "color: #5B2C6F; font-weight: bold; margin: 0;")
                ),
                DTOutput("tabla_items_actuales"),
                hr(class = "my-3"),
                
                fluidRow(
                  column(6, div(
                    class = "p-3 bg-light border rounded h-100 d-flex flex-column justify-content-center mb-2 mb-md-0",
                    div(style = "margin-bottom: -15px;", numericInput("abono", "Abona ($):", value = 0.00, min = 0, step = 0.50, width = "100%"))
                  )),
                  column(6, div(
                    class = "p-3 bg-light border rounded h-100 d-flex flex-column justify-content-center text-center",
                    h6("SALDO PENDIENTE", style = "margin-bottom: 4px; font-weight: bold;"),
                    h3(textOutput("txt_saldo"), style = "font-weight: bold; color: #000000; margin: 0;")
                  ))
                )
              )
            )
          ),
          div(class = "mt-3", uiOutput("botones_inferiores_ui"))
        )
      )
    )
  ),
  
  # PESTAÑA 2: MONITOR DE COCINA Y DESPACHOS (KDS)
  nav_panel(
    "Cocina y Entregas",
    icon = icon("fire-burner"),
    div(
      class = "container-fluid", style = "max-width: 1250px; padding-top: 15px;",
      div(
        class = "d-flex justify-content-between align-items-center mb-3",
        h4("Tablero de Pedidos Activos", style = "color: #5B2C6F; font-weight: bold; margin: 0;"),
        selectInput("filtro_estado_kds", NULL, choices = c("Todos los Activos", "Recibido", "En Cocina", "Listo", "Entregado"), width = "250px")
      ),
      uiOutput("tarjetas_cocina_ui")
    )
  ),
  
  # PESTAÑA 3: CAJA Y REPORTES FINANCIEROS
  nav_panel(
    "Caja y Historial",
    icon = icon("chart-line"),
    div(
      class = "container-fluid", style = "max-width: 1250px; padding-top: 15px;",
      uiOutput("kpis_caja_ui"),
      br(),
      div(
        class = "bg-white p-4 border rounded shadow-sm mb-4",
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          h5("Registro Maestro de Órdenes", style = "color: #5B2C6F; font-weight: bold; margin: 0;"),
          downloadButton("descargar_ordenes_csv", "📥 Descargar Órdenes (CSV)", class = "btn-outline-success btn-sm")
        ),
        DTOutput("tabla_historial_ordenes")
      ),
      div(
        class = "bg-white p-4 border rounded shadow-sm",
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          h5("Detalle de Ítems Vendidos", style = "color: #5B2C6F; font-weight: bold; margin: 0;"),
          downloadButton("descargar_detalle_csv", "📥 Descargar Detalle (CSV)", class = "btn-outline-success btn-sm")
        ),
        DTOutput("tabla_historial_detalle")
      )
    )
  ),
  # PESTAÑA 4: GESTIÓN DE MENÚ / CATÁLOGO
  nav_panel(
    "Menú / Catálogo",
    icon = icon("book-open"),
    div(
      class = "container-fluid", style = "max-width: 1150px; padding-top: 15px;",
      fluidRow(
        column(4, div(
          class = "bg-white p-4 border rounded shadow-sm",
          h5("Nuevo Plato / Servicio", style = "color: #5B2C6F; font-weight: bold;"),
          uiOutput("selector_categoria_abierta"),
          textInput("nuevo_prod_desc", "Nombre del Producto:", placeholder = "", width = "100%"),
          numericInput("nuevo_prod_precio", "Precio ($):", value = 0, min = 0, step = 0.25, width = "100%"),
          actionButton("btn_guardar_prod", "Agregar al Menú", class = "btn-primary w-100")
        )),
        column(8, div(
          class = "bg-white p-4 border rounded shadow-sm",
          div(
            class = "d-flex justify-content-between align-items-center mb-2",
            div(
              h5("Carta Actual NUWA", style = "color: #5B2C6F; font-weight: bold; margin: 0;"),
              tags$small("Doble clic en cualquier celda para editar", class = "text-muted")
            ),
            downloadButton("descargar_catalogo_csv", "📥 Descargar Catálogo (CSV)", class = "btn-outline-success btn-sm")
          ),
          DTOutput("tabla_catalogo")
        ))
      )
    )
  ),
  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('toggle_solo_imprimir', function(bloquear) {
      var fs = document.getElementById('fieldset_formulario');
      if (fs) {
        fs.disabled = bloquear;
        fs.style.opacity = bloquear ? '0.96' : '1';
      }
    });
  "))
)
server <- function(input, output, session) {
  
  leer_catalogo <- function() {
    df <- read.csv("catalogo_nuwa.csv", stringsAsFactors = FALSE)
    if (!"categoria" %in% names(df)) df$categoria <- "General"
    df
  }
  
  leer_ordenes <- function() {
    read.csv("ordenes_nuwa.csv", colClasses = c(
      "character", "character", "character", "character", "character",
      "character", "character", "character", "character", "character",
      "character", "character", "numeric", "numeric", "numeric"
    ), stringsAsFactors = FALSE)
  }
  
  leer_detalle <- function() {
    read.csv("detalle_ordenes_nuwa.csv", colClasses = c(
      "character", "numeric", "character", "numeric", "numeric"
    ), stringsAsFactors = FALSE)
  }
  
  rv <- reactiveValues(
    catalogo = leer_catalogo(),
    ordenes = leer_ordenes(),
    detalle = leer_detalle(),
    items_actuales = tibble(Cant = numeric(), Descripción = character(), `P.Unit` = numeric(), Total = numeric()),
    orden_en_edicion = NULL,
    ordenes_encontradas = character()
  )
  
  es_solo_imprimir <- reactive({
    !is.null(rv$orden_en_edicion) && !is.null(input$modo_orden) && input$modo_orden == "imprimir"
  })
  
  observe({ session$sendCustomMessage("toggle_solo_imprimir", es_solo_imprimir()) })
  
  siguiente_orden <- reactive({
    prefijo <- format(Sys.Date(), "%y")
    existentes <- rv$ordenes$n_orden[startsWith(rv$ordenes$n_orden, prefijo) & nchar(rv$ordenes$n_orden) == 5]
    if (length(existentes) == 0) return(paste0(prefijo, "001"))
    nums <- suppressWarnings(as.integer(substring(existentes, 3)))
    nums <- nums[!is.na(nums)]
    if (length(nums) == 0) return(paste0(prefijo, "001"))
    sprintf("%s%03d", prefijo, max(nums) + 1L)
  })
  
  numero_orden_actual <- reactive({
    if (is.null(rv$orden_en_edicion)) siguiente_orden() else rv$orden_en_edicion
  })
  
  output$badge_n_orden <- renderUI({
    if (is.null(rv$orden_en_edicion)) {
      span(
        paste("N° ORDEN:", siguiente_orden()), 
        class = "badge", 
        style = "background-color: #FFFFFF; color: #5B2C6F; font-weight: bold; font-size: 1.15em; padding: 8px 14px;"
      )
    } else if (es_solo_imprimir()) {
      span(
        paste("N° ORDEN:", rv$orden_en_edicion), 
        class = "badge", 
        style = "background-color: #FFFFFF; color: #5B2C6F; font-weight: bold; font-size: 1.15em; padding: 8px 14px;"
      )
    } else {
      span(
        paste("EDITANDO:", rv$orden_en_edicion), 
        class = "badge bg-warning text-dark", 
        style = "font-size: 1.15em; padding: 8px 14px;"
      )
    }
  })
  
  output$resumen_logistica_badge <- renderUI({
    color_est <- switch(input$estado_orden, "Recibido" = "bg-secondary", "En Cocina" = "bg-warning text-dark", "Listo" = "bg-info", "Entregado" = "bg-success", "bg-secondary")
    tagList(
      span(input$tipo_entrega, class = "badge bg-primary me-1"),
      span(input$estado_orden, class = paste("badge", color_est))
    )
  })
  
  output$bloque_info_orden_ui <- renderUI({
    dir_txt <- if (!is.null(input$tipo_entrega) && input$tipo_entrega == "Envío" && trimws(input$direccion_entrega) != "") {
      div(class = "mt-1", tags$strong("Dirección de Envío: "), input$direccion_entrega)
    } else {
      NULL
    }
    
    nota_txt <- if (!is.null(input$observacion) && trimws(input$observacion) != "") {
      div(class = "mt-1 text-dark", tags$strong("Nota para el cliente: "), input$observacion)
    } else {
      NULL
    }
    
    div(
      class = "p-2 mt-2 bg-light border rounded small",
      div(
        class = "d-flex flex-wrap justify-content-between",
        span(tags$strong("Tipo Entrega: "), input$tipo_entrega),
        span(tags$strong("Hora Entrega: "), ifelse(trimws(input$hora_entrega) == "", "-", input$hora_entrega)),
        span(tags$strong("Método Pago: "), input$metodo_pago),
        span(tags$strong("Estado: "), input$estado_orden)
      ),
      dir_txt,
      nota_txt
    )
  })
  
  output$selector_producto_manual <- renderUI({
    selectizeInput(
      "item_desc",
      "Producto:",
      choices = c("Seleccionar producto..." = "", rv$catalogo$descripcion),
      selected = "",
      width = "100%"
    )
  })
  
  observeEvent(input$item_desc, {
    if (is.null(input$item_desc) || input$item_desc == "") {
      updateNumericInput(session, "item_precio", value = NA)
      return()
    }
    prod <- rv$catalogo %>% filter(descripcion == input$item_desc)
    if (nrow(prod) > 0) updateNumericInput(session, "item_precio", value = prod$precio_unitario[1])
  })
  
  observeEvent(input$btn_agregar_item, {
    if (es_solo_imprimir()) return()
    if (is.null(input$item_desc) || input$item_desc == "") {
      showNotification("Selecciona un producto de la lista.", type = "warning")
      return()
    }
    req(input$item_cant > 0, !is.na(input$item_precio))
    subtotal <- round(input$item_cant * input$item_precio, 2)
    rv$items_actuales <- bind_rows(rv$items_actuales, tibble(
      Cant = as.numeric(input$item_cant),
      Descripción = as.character(input$item_desc),
      `P.Unit` = as.numeric(input$item_precio),
      Total = as.numeric(subtotal)
    ))
    updateNumericInput(session, "item_cant", value = 1)
    updateSelectizeInput(session, "item_desc", selected = "")
    updateNumericInput(session, "item_precio", value = NA)
  })
  
  observeEvent(input$btn_agregar_item, {
    if (es_solo_imprimir()) return()
    req(input$item_desc, input$item_cant > 0)
    subtotal <- round(input$item_cant * input$item_precio, 2)
    rv$items_actuales <- bind_rows(rv$items_actuales, tibble(
      Cant = as.numeric(input$item_cant), Descripción = as.character(input$item_desc),
      `P.Unit` = as.numeric(input$item_precio), Total = as.numeric(subtotal)
    ))
    updateNumericInput(session, "item_cant", value = 1)
  })
  
  observeEvent(input$eliminar_fila_idx, {
    if (es_solo_imprimir()) return()
    idx <- as.numeric(input$eliminar_fila_idx)
    if (!is.na(idx) && idx >= 1 && idx <= nrow(rv$items_actuales)) {
      rv$items_actuales <- rv$items_actuales[-idx, ]
    }
  })
  
  total_orden <- reactive({ sum(rv$items_actuales$Total, na.rm = TRUE) })
  saldo_orden <- reactive({ max(0, total_orden() - ifelse(is.na(input$abono), 0, input$abono)) })
  
  output$txt_saldo <- renderText({ sprintf("$ %.2f", saldo_orden()) })
  
  output$tabla_items_actuales <- renderDT({
    df_mostrar <- rv$items_actuales
    
    if (nrow(df_mostrar) > 0 && !es_solo_imprimir()) {
      df_mostrar$Borrar <- sapply(seq_len(nrow(df_mostrar)), function(i) {
        sprintf(
          '<button type="button" class="btn btn-outline-danger btn-sm py-0 px-2" onclick="Shiny.setInputValue(\'eliminar_fila_idx\', %d, {priority: \'event\'})">🗑️</button>',
          i
        )
      })
    } else {
      df_mostrar$Borrar <- character(nrow(df_mostrar))
    }
    
    datatable(
      df_mostrar,
      rownames = FALSE,
      escape = FALSE,
      container = htmltools::withTags(table(
        class = 'display',
        thead(tr(
          th("Cant"), th("Descripción"), th("P.Unit"), th("Total"), th("")
        )),
        tfoot(tr(
          th(colspan = 3, style = "text-align: right; font-weight: bold; color: #5B2C6F;", "TOTAL ORDEN:"),
          th(style = "font-weight: bold; font-size: 1.15em; color: #2C3E50;", sprintf("$ %.2f", total_orden())),
          th("")
        ))
      )),
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = c(0, 4)))
      )
    ) %>% formatCurrency(c("P.Unit", "Total"), currency = "$ ", digits = 2)
  })
  
  output$molde_comprobante_foto <- renderUI({
    filas_tabla <- if (nrow(rv$items_actuales) == 0) {
      tags$tr(tags$td(colspan = 4, style = "text-align: center; padding: 12px; color: #7f8c8d;", "Sin productos registrados"))
    } else {
      lapply(seq_len(nrow(rv$items_actuales)), function(i) {
        it <- rv$items_actuales[i, ]
        tags$tr(
          style = "border-bottom: 1px solid #e0e0e0;",
          tags$td(style = "padding: 8px; text-align: center;", it$Cant),
          tags$td(style = "padding: 8px;", it$Descripción),
          tags$td(style = "padding: 8px; text-align: right;", sprintf("$ %.2f", it[["P.Unit"]])),
          tags$td(style = "padding: 8px; text-align: right; font-weight: bold;", sprintf("$ %.2f", it$Total))
        )
      })
    }
    
    div(
      id = "comprobante_vista_previa",
      style = "width: 650px; background: #ffffff; border: 1px solid #dcdcdc; border-radius: 8px; overflow: hidden; font-family: Arial, sans-serif; color: #2C3E50;",
      div(
        style = "background-color: #5B2C6F; color: #ffffff; padding: 18px 24px; display: flex; justify-content: space-between; align-items: center;",
        div(
          div("ORDEN DE PEDIDO", style = "font-weight: bold; font-size: 22px; line-height: 1.2;"),
          div("NUWA fusion • Quito - Ecuador", style = "font-size: 13px; opacity: 0.9;")
        ),
        div(
          style = "background: #FFFFFF; color: #5B2C6F; padding: 6px 14px; border-radius: 6px; font-weight: bold; font-size: 16px;",
          paste("N° ORDEN:", numero_orden_actual())
        )
      ),
      div(
        style = "padding: 20px 24px;",
        div(
          style = "display: flex; justify-content: space-between; margin-bottom: 15px; font-size: 14px;",
          div(
            style = "width: 48%;",
            p(style = "margin: 4px 0;", tags$strong("Fecha Pedido: "), format(input$fecha_orden, "%d/%m/%Y")),
            p(style = "margin: 4px 0;", tags$strong("Cliente: "), ifelse(trimws(input$cliente_nombre) == "", "-", input$cliente_nombre)),
            p(style = "margin: 4px 0;", tags$strong("ID / Cédula: "), ifelse(trimws(input$cliente_id) == "", "-", input$cliente_id)),
            p(style = "margin: 4px 0;", tags$strong("Teléfono: "), ifelse(trimws(input$telefono) == "", "-", input$telefono))
          ),
          div(
            style = "width: 48%;",
            p(style = "margin: 4px 0;", tags$strong("Fecha Entrega: "), format(input$fecha_entrega, "%d/%m/%Y")),
            p(style = "margin: 4px 0;", tags$strong("Hora Entrega: "), ifelse(trimws(input$hora_entrega) == "", "-", input$hora_entrega)),
            p(style = "margin: 4px 0;", tags$strong("Tipo Entrega: "), input$tipo_entrega),
            p(style = "margin: 4px 0;", tags$strong("Método Pago: "), input$metodo_pago),
            p(style = "margin: 4px 0;", tags$strong("Estado: "), input$estado_orden)
          )
        ),
        if (input$tipo_entrega == "Envío" && trimws(input$direccion_entrega) != "") {
          div(style = "background: #f8f9fa; padding: 8px 12px; border-radius: 4px; border-left: 4px solid #5B2C6F; margin-bottom: 10px; font-size: 13px;", tags$strong("Dirección de Envío: "), input$direccion_entrega)
        },
        if (trimws(input$observacion) != "") {
          div(style = "background: #fff8e1; padding: 8px 12px; border-radius: 4px; border-left: 4px solid #D68910; margin-bottom: 15px; font-size: 13px;",
              tags$strong("Nota para el cliente: "), input$observacion)
        },
        tags$hr(style = "border-top: 1px solid #e0e0e0; margin: 15px 0;"),
        div("DETALLE DE CONSUMO", style = "color: #5B2C6F; font-weight: bold; font-size: 14px; margin-bottom: 10px;"),
        tags$table(
          style = "width: 100%; border-collapse: collapse; font-size: 14px; margin-bottom: 15px;",
          tags$thead(
            tags$tr(
              style = "background-color: #f4f6f7; border-bottom: 2px solid #5B2C6F;",
              tags$th(style = "padding: 8px; text-align: center; width: 12%;", "Cant."),
              tags$th(style = "padding: 8px; text-align: left; width: 52%;", "Descripción"),
              tags$th(style = "padding: 8px; text-align: right; width: 18%;", "P. Unit"),
              tags$th(style = "padding: 8px; text-align: right; width: 18%;", "Total")
            )
          ),
          tags$tbody(filas_tabla),
          tags$tfoot(
            tags$tr(
              style = "border-top: 2px solid #2C3E50; background-color: #f9f9f9;",
              tags$td(colspan = 3, style = "padding: 10px 8px; text-align: right; font-weight: bold; color: #5B2C6F;", "TOTAL ORDEN:"),
              tags$td(style = "padding: 10px 8px; text-align: right; font-weight: bold; font-size: 16px;", sprintf("$ %.2f", total_orden()))
            )
          )
        ),
        div(
          style = "display: flex; justify-content: space-between; gap: 15px; margin-top: 15px;",
          div(
            style = "width: 48%; background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; text-align: center;",
            div("ABONO RECIBIDO", style = "font-size: 12px; font-weight: bold; color: #555;"),
            div(sprintf("$ %.2f", ifelse(is.na(input$abono), 0, input$abono)), style = "font-size: 22px; font-weight: bold; color: #1E8449; margin-top: 4px;")
          ),
          div(
            style = "width: 48%; background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; text-align: center;",
            div("SALDO PENDIENTE", style = "font-size: 12px; font-weight: bold; color: #555;"),
            div(sprintf("$ %.2f", saldo_orden()), style = "font-size: 22px; font-weight: bold; color: #000000; margin-top: 4px;")
          )
        )
      )
    )
  })
  output$botones_inferiores_ui <- renderUI({
    btn_foto <- actionButton(
      "btn_abrir_vista_foto",
      "📷 Ver Orden",
      class = "btn-outline-primary btn-lg w-100 mb-2 mb-md-0"
    )
    if (es_solo_imprimir()) {
      actionButton(
        "btn_abrir_vista_foto",
        "📷 Ver Orden",
        class = "btn-primary btn-lg w-100"
      )
    } else if (!is.null(rv$orden_en_edicion)) {
      fluidRow(
        column(4, btn_foto),
        column(8, actionButton("btn_guardar_orden", paste("ACTUALIZAR ORDEN", rv$orden_en_edicion), class = "btn-warning btn-lg w-100 fw-bold"))
      )
    } else {
      fluidRow(
        column(4, btn_foto),
        column(8, actionButton("btn_guardar_orden", "✅ CONFIRMAR ORDEN", class = "btn-success btn-lg w-100 fw-bold"))
      )
    }
  })
  
  observeEvent(input$btn_abrir_vista_foto, {
    nombre_foto <- paste0("Orden_NUWA_", numero_orden_actual())
    showModal(modalDialog(
      title = tags$strong("Vista Previa del Comprobante", style = "color: #5B2C6F;"),
      size = "l",
      easyClose = TRUE,
      div(
        style = "overflow-x: auto; display: flex; justify-content: center; padding: 10px; background: #f0f2f5; border-radius: 6px;",
        uiOutput("molde_comprobante_foto")
      ),
      footer = tagList(
        modalButton("Cerrar"),
        screenshotButton(
          id = "comprobante_vista_previa",
          label = "Descargar orden",
          filename = nombre_foto,
          scale = 2,
          class = "btn-success fw-bold"
        )
      )
    ))
  })
  limpiar_todo <- function() {
    rv$orden_en_edicion <- NULL
    rv$items_actuales <- rv$items_actuales[0, ]
    updateDateInput(session, "fecha_orden", value = Sys.Date())
    updateTextInput(session, "cliente_nombre", value = "")
    updateTextInput(session, "cliente_id", value = "")
    updateTextInput(session, "telefono", value = "")
    updateDateInput(session, "fecha_entrega", value = Sys.Date() + 1)
    updateSelectInput(session, "tipo_entrega", selected = "Local")
    updateSelectInput(session, "metodo_pago", selected = "Efectivo")
    updateSelectInput(session, "estado_orden", selected = "Recibido")
    updateTextInput(session, "direccion_entrega", value = "")
    updateTextInput(session, "hora_entrega", value = "")
    updateTextInput(session, "observacion", value = "")
    updateNumericInput(session, "abono", value = 0)
  }
  observeEvent(input$btn_nueva_orden, {
    limpiar_todo()
    rv$ordenes_encontradas <- character()
    updateTextInput(session, "buscar_id", value = "")
  })
  observeEvent(input$btn_buscar_cliente, {
    termino <- tolower(trimws(input$buscar_id))
    if (termino == "") {
      showNotification("Escribe un ID o nombre para buscar.", type = "warning")
      return()
    }
    enc <- rv$ordenes %>%
      filter(
        tolower(trimws(cliente_id)) == termino |
          grepl(termino, tolower(cliente_nombre), fixed = TRUE)
      )
    if (nrow(enc) == 0) {
      rv$ordenes_encontradas <- character()
      showNotification("No se encontraron órdenes con ese ID o nombre.", type = "warning")
    } else {
      ult <- tail(enc, 1)
      updateTextInput(session, "cliente_nombre", value = ult$cliente_nombre)
      updateTextInput(session, "cliente_id", value = ult$cliente_id)
      updateTextInput(session, "telefono", value = ult$telefono)
      updateTextInput(session, "direccion_entrega", value = ult$direccion_entrega)
      rv$ordenes_encontradas <- setNames(
        enc$n_orden,
        paste0("#", enc$n_orden, " - ", enc$cliente_nombre, " (", enc$fecha_entrega, ")")
      )
      showNotification(paste("Coincidencias encontradas para:", ult$cliente_nombre), type = "message")
    }
  })
  output$selector_orden_editar <- renderUI({
    if (length(rv$ordenes_encontradas) == 0) return(NULL)
    selectInput("orden_a_cargar", NULL, choices = c("Seleccionar orden..." = "", rv$ordenes_encontradas), width = "100%")
  })
  output$selector_modo_orden <- renderUI({
    req(input$orden_a_cargar)
    if (input$orden_a_cargar == "") return(NULL)
    selectInput("modo_orden", NULL, choices = c("Sólo Imprimir" = "imprimir", "Editar Orden" = "editar"), selected = "imprimir", width = "100%")
  })
  observeEvent(input$orden_a_cargar, {
    req(input$orden_a_cargar)
    if (input$orden_a_cargar == "") { rv$orden_en_edicion <- NULL; rv$items_actuales <- rv$items_actuales[0, ]; return() }
    ord <- rv$ordenes %>% filter(n_orden == input$orden_a_cargar)
    if (nrow(ord) > 0) {
      rv$orden_en_edicion <- ord$n_orden
      updateDateInput(session, "fecha_orden", value = as.Date(ord$fecha_orden, format = "%d/%m/%Y"))
      updateTextInput(session, "cliente_nombre", value = ord$cliente_nombre)
      updateTextInput(session, "cliente_id", value = ord$cliente_id)
      updateTextInput(session, "telefono", value = ord$telefono)
      updateDateInput(session, "fecha_entrega", value = as.Date(ord$fecha_entrega, format = "%d/%m/%Y"))
      updateSelectInput(session, "tipo_entrega", selected = ord$tipo_entrega)
      updateSelectInput(session, "metodo_pago", selected = ord$metodo_pago)
      updateSelectInput(session, "estado_orden", selected = ord$estado)
      updateTextInput(session, "direccion_entrega", value = ord$direccion_entrega)
      updateTextInput(session, "hora_entrega", value = ord$hora_entrega)
      updateTextInput(session, "observacion", value = ord$observacion)
      updateNumericInput(session, "abono", value = ord$abono)
      rv$items_actuales <- rv$detalle %>%
        filter(n_orden == ord$n_orden) %>%
        select(Cant = cantidad, Descripción = descripcion, P.Unit = precio_unitario, Total = subtotal)
    }
  })
  observeEvent(input$btn_guardar_orden, {
    if (es_solo_imprimir()) return()
    if (nrow(rv$items_actuales) == 0) { showNotification("Agrega al menos un producto al ticket.", type = "error"); return() }
    if (trimws(input$cliente_nombre) == "") { showNotification("Ingresa el nombre del cliente.", type = "error"); return() }
    es_ed <- !is.null(rv$orden_en_edicion)
    num_ord <- ifelse(es_ed, rv$orden_en_edicion, siguiente_orden())
    dir_f <- ifelse(input$tipo_entrega == "Envío", as.character(input$direccion_entrega), "")
    nueva_o <- tibble(
      n_orden = as.character(num_ord), fecha_orden = as.character(format(input$fecha_orden, "%d/%m/%Y")),
      cliente_nombre = as.character(input$cliente_nombre), cliente_id = as.character(input$cliente_id),
      telefono = as.character(input$telefono), fecha_entrega = as.character(format(input$fecha_entrega, "%d/%m/%Y")),
      hora_entrega = as.character(input$hora_entrega), tipo_entrega = as.character(input$tipo_entrega),
      direccion_entrega = dir_f, observacion = as.character(input$observacion),
      metodo_pago = as.character(input$metodo_pago), estado = as.character(input$estado_orden),
      total = as.numeric(total_orden()), abono = as.numeric(ifelse(is.na(input$abono), 0, input$abono)),
      saldo = as.numeric(saldo_orden())
    )
    if (es_ed) {
      rv$ordenes <- rv$ordenes %>% filter(n_orden != num_ord)
      rv$detalle <- rv$detalle %>% filter(n_orden != num_ord)
    }
    rv$ordenes <- bind_rows(rv$ordenes, nueva_o) %>% arrange(n_orden)
    write.csv(rv$ordenes, "ordenes_nuwa.csv", row.names = FALSE)
    nuevo_d <- rv$items_actuales %>%
      mutate(n_orden = as.character(num_ord)) %>%
      select(n_orden, cantidad = Cant, descripcion = Descripción, precio_unitario = "P.Unit", subtotal = Total)
    rv$detalle <- bind_rows(rv$detalle, nuevo_d) %>% arrange(n_orden)
    write.csv(rv$detalle, "detalle_ordenes_nuwa.csv", row.names = FALSE)
    limpiar_todo()
    rv$ordenes_encontradas <- character()
    updateTextInput(session, "buscar_id", value = "")
    showNotification(paste("Orden #", num_ord, "guardada correctamente."), type = "message")
  })
  output$tarjetas_cocina_ui <- renderUI({
    req(nrow(rv$ordenes) > 0)
    df <- rv$ordenes
    if (input$filtro_estado_kds == "Todos los Activos") {
      df <- df %>% filter(estado != "Entregado")
    } else {
      df <- df %>% filter(estado == input$filtro_estado_kds)
    }
    if (nrow(df) == 0) return(div(class = "alert alert-info", "No hay órdenes en esta categoría."))
    df <- df %>%
      mutate(fecha_ordenar = as.Date(fecha_entrega, format = "%d/%m/%Y")) %>%
      arrange(fecha_ordenar, hora_entrega, n_orden)
    tarjetas <- lapply(seq_len(nrow(df)), function(i) {
      ord <- df[i, ]
      items <- rv$detalle %>% filter(n_orden == ord$n_orden)
      lista_items <- lapply(seq_len(nrow(items)), function(j) {
        tags$li(paste0(items$cantidad[j], "x ", items$descripcion[j]))
      })
      borde <- switch(ord$estado, "Recibido" = "border-secondary", "En Cocina" = "border-warning", "Listo" = "border-info", "Entregado" = "border-success", "border-primary")
      column(
        4, class = "mb-3",
        div(
          class = paste("card shadow-sm h-100", borde), style = "border-width: 2px;",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            tags$strong(paste("Orden #", ord$n_orden)),
            span(ord$estado, class = "badge bg-primary")
          ),
          div(
            class = "card-body",
            p(class = "mb-1", tags$strong("Cliente: "), ord$cliente_nombre),
            p(class = "mb-1", tags$strong("Entrega: "), ord$fecha_entrega, " • ", ord$hora_entrega, " (", ord$tipo_entrega, ")"),
            if (ord$observacion != "") p(class = "text-danger small mb-2", tags$strong("Nota: "), ord$observacion),
            hr(class = "my-2"),
            tags$ul(class = "ps-3 mb-2", lista_items),
            hr(class = "my-2"),
            div(class = "d-flex justify-content-between",
                span(paste("Total: $", sprintf("%.2f", ord$total))),
                tags$strong(paste("Saldo: $", sprintf("%.2f", ord$saldo)), style = "color: #000000;")
            )
          )
        )
      )
    })
    fluidRow(tarjetas)
  })
  output$kpis_caja_ui <- renderUI({
    tot_ventas <- sum(rv$ordenes$total, na.rm = TRUE)
    tot_cobrado <- sum(rv$ordenes$abono, na.rm = TRUE)
    tot_pendiente <- sum(rv$ordenes$saldo, na.rm = TRUE)
    n_ord <- nrow(rv$ordenes)
    fluidRow(
      column(3, div(class = "p-3 bg-white border rounded shadow-sm text-center mb-2", tags$small("ÓRDENES TOTALES"), h3(n_ord, style = "color: #5B2C6F; font-weight: bold; margin:0;"))),
      column(3, div(class = "p-3 bg-white border rounded shadow-sm text-center mb-2", tags$small("VENTAS BRUTAS"), h3(sprintf("$ %.2f", tot_ventas), style = "color: #2C3E50; font-weight: bold; margin:0;"))),
      column(3, div(class = "p-3 bg-white border rounded shadow-sm text-center mb-2", tags$small("COBRADO (ABONOS)"), h3(sprintf("$ %.2f", tot_cobrado), style = "color: #1E8449; font-weight: bold; margin:0;"))),
      column(3, div(class = "p-3 bg-white border rounded shadow-sm text-center mb-2", tags$small("POR COBRAR (SALDOS)"), h3(sprintf("$ %.2f", tot_pendiente), style = "color: #C0392B; font-weight: bold; margin:0;")))
    )
  })
  output$tabla_historial_ordenes <- renderDT({
    datatable(
      rv$ordenes,
      options = list(pageLength = 10, order = list(list(0, 'desc'))),
      rownames = FALSE
    ) %>%
      formatCurrency(c("total", "abono", "saldo"), currency = "$ ", digits = 2) %>%
      formatStyle(
        "saldo",
        color = styleInterval(0, c("#2C3E50", "#C0392B")),
        fontWeight = styleInterval(0, c("normal", "bold"))
      )
  })
  output$tabla_historial_detalle <- renderDT({
    datatable(rv$detalle, options = list(pageLength = 10, order = list(list(0, 'desc'))), rownames = FALSE)
  })
  output$selector_categoria_abierta <- renderUI({
    cats_existentes <- unique(c("Cenas", "Horneos", "Platos Fuertes", "Postres", "Adicionales", rv$catalogo$categoria))
    cats_existentes <- cats_existentes[cats_existentes != ""]
    selectizeInput(
      "nuevo_prod_cat",
      "Categoría (selecciona o escribe nueva):",
      choices = cats_existentes,
      options = list(create = TRUE, placeholder = ""),
      width = "100%"
    )
  })
  observeEvent(input$btn_guardar_prod, {
    req(trimws(input$nuevo_prod_desc) != "")
    cat_final <- ifelse(is.null(input$nuevo_prod_cat) || trimws(input$nuevo_prod_cat) == "", "General", as.character(input$nuevo_prod_cat))
    rv$catalogo <- bind_rows(rv$catalogo, tibble(
      categoria = cat_final,
      descripcion = as.character(input$nuevo_prod_desc),
      precio_unitario = as.numeric(input$nuevo_prod_precio)
    ))
    write.csv(rv$catalogo, "catalogo_nuwa.csv", row.names = FALSE)
    updateTextInput(session, "nuevo_prod_desc", value = "")
    showNotification("Plato agregado al menú.", type = "message")
  })
  observeEvent(input$tabla_catalogo_cell_edit, {
    info <- input$tabla_catalogo_cell_edit
    fila <- info$row
    col <- info$col + 1
    valor <- info$value
    if (col <= 3) {
      if (col == 3) {
        num_val <- suppressWarnings(as.numeric(valor))
        if (!is.na(num_val) && num_val >= 0) {
          rv$catalogo[fila, col] <- num_val
        } else {
          showNotification("Ingresa un precio numérico válido.", type = "error")
        }
      } else {
        rv$catalogo[fila, col] <- as.character(valor)
      }
      write.csv(rv$catalogo, "catalogo_nuwa.csv", row.names = FALSE)
      showNotification("Catálogo actualizado.", type = "message", duration = 2)
    }
  })
  observeEvent(input$eliminar_cat_idx, {
    idx <- as.numeric(input$eliminar_cat_idx)
    if (!is.na(idx) && idx >= 1 && idx <= nrow(rv$catalogo)) {
      rv$catalogo <- rv$catalogo[-idx, ]
      write.csv(rv$catalogo, "catalogo_nuwa.csv", row.names = FALSE)
      showNotification("Producto eliminado del catálogo.", type = "warning", duration = 2)
    }
  })
  output$tabla_catalogo <- renderDT({
    df_cat <- rv$catalogo
    if (nrow(df_cat) > 0) {
      df_cat$Borrar <- sprintf(
        '<button type="button" class="btn btn-outline-danger btn-sm py-0 px-2 btn-borrar-cat" data-row="%d">🗑️</button>',
        seq_len(nrow(df_cat))
      )
    } else {
      df_cat$Borrar <- character(0)
    }
    
    datatable(
      df_cat,
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      editable = list(target = "cell", disable = list(columns = c(3))),
      colnames = c("Categoría", "Descripción", "Precio ($)", ""),
      callback = JS("
        table.on('click', '.btn-borrar-cat', function(e) {
          e.preventDefault();
          e.stopPropagation();
          var fila = $(this).data('row');
          Shiny.setInputValue('eliminar_cat_idx', fila, {priority: 'event'});
        });
      "),
      options = list(
        pageLength = 10,
        columnDefs = list(list(className = 'dt-center', targets = 3))
      )
    ) %>% formatCurrency("precio_unitario", currency = "$ ", digits = 2)
  })
  output$descargar_ordenes_csv <- downloadHandler(
    filename = function() {
      paste0("Ordenes_NUWA_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      write.csv(rv$ordenes, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$descargar_detalle_csv <- downloadHandler(
    filename = function() {
      paste0("Detalle_Ordenes_NUWA_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      write.csv(rv$detalle, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$descargar_catalogo_csv <- downloadHandler(
    filename = function() {
      paste0("Catalogo_NUWA_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      write.csv(rv$catalogo, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}
shinyApp(ui = ui, server = server)
