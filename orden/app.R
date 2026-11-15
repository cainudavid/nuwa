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
    tags$small(" | POS & Cocina", style = "opacity: 0.8; font-weight: normal;")
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
      
      div(
        class = "bg-white p-2 mb-3 border rounded shadow-sm",
        fluidRow(
          column(3, textInput("buscar_id", NULL, placeholder = "Buscar por Cédula / ID Cliente...", width = "100%")),
          column(2, actionButton("btn_buscar_cliente", "Buscar Cliente", class = "btn-info text-white w-100")),
          column(3, uiOutput("selector_orden_editar")),
          column(2, uiOutput("selector_modo_orden")),
          column(2, actionButton("btn_nueva_orden", "+ Nueva Orden", class = "btn-outline-secondary w-100"))
        )
      ),
      
      fluidRow(
        # COLUMNA IZQUIERDA: MENÚ RÁPIDO Y LOGÍSTICA
        column(
          5,
          div(
            class = "bg-white p-3 mb-3 border rounded shadow-sm",
            h6(icon("utensils"), " MENÚ RÁPIDO (Clic para agregar)", style = "color: #5B2C6F; font-weight: bold; margin-bottom: 12px;"),
            uiOutput("botones_menu_rapido"),
            hr(class = "my-2"),
            fluidRow(
              column(3, numericInput("item_cant", "Cant:", value = 1, min = 1, width = "100%")),
              column(6, uiOutput("selector_producto_manual")),
              column(3, numericInput("item_precio", "Precio:", value = 1.00, min = 0, step = 0.5, width = "100%"))
            ),
            actionButton("btn_agregar_item", "+ Añadir Cantidad Personalizada", class = "btn-outline-success btn-sm w-100")
          ),
          
          div(
            class = "bg-white p-3 border rounded shadow-sm",
            h6(icon("truck"), " LOGÍSTICA Y ESTADO", style = "color: #5B2C6F; font-weight: bold; margin-bottom: 12px;"),
            fluidRow(
              column(6, selectInput("tipo_entrega", "Tipo Entrega:", choices = c("Local", "Envío"), width = "100%")),
              column(6, selectInput("metodo_pago", "Método de Pago:", choices = c("Efectivo", "Transferencia", "Tarjeta", "Pendiente"), width = "100%"))
            ),
            fluidRow(
              column(6, selectInput("estado_orden", "Estado Cocina:", choices = c("Pendiente", "En Cocina", "Listo", "Entregado"), width = "100%")),
              column(6, textInput("hora_entrega", "Hora Entrega:", placeholder = "Ej. 13h30", width = "100%"))
            ),
            conditionalPanel(
              condition = "input.tipo_entrega == 'Envío'",
              textInput("direccion_entrega", "Dirección Exacta de Envío:", placeholder = "Sector, calles, referencia...", width = "100%")
            ),
            textInput("observacion", "Notas para Cocina / Empaque:", placeholder = "Ej. Bien dorado, sin cebolla...", width = "100%")
          )
        ),
        
        # COLUMNA DERECHA: COMANDA DIGITAL (ÁREA CAPTURABLE COMO FOTO)
        column(
          7,
          div(
            id = "area_orden_foto",
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
                  column(4, textInput("cliente_nombre", "Cliente:", placeholder = "Nombre completo", width = "100%")),
                  column(4, textInput("cliente_id", "ID / Cédula:", placeholder = "17XXXXXXXX", width = "100%"))
                ),
                fluidRow(
                  column(4, textInput("telefono", "Teléfono:", placeholder = "099XXXXXXX", width = "100%")),
                  column(4, dateInput("fecha_entrega", "Fecha Entrega:", value = Sys.Date() + 1, format = "dd/mm/yyyy", width = "100%")),
                  column(4, div(class = "pt-4 text-end", uiOutput("resumen_logistica_badge")))
                ),
                
                hr(class = "my-3"),
                
                div(
                  class = "d-flex justify-content-between align-items-center mb-2",
                  h6("DETALLE DE CONSUMO", style = "color: #5B2C6F; font-weight: bold; margin: 0;"),
                  div(
                    `data-html2canvas-ignore` = "true",
                    actionButton("btn_quitar_ultima", "Borrar Última Fila", class = "btn-outline-warning btn-sm"),
                    actionButton("btn_limpiar_items", "Vaciar", class = "btn-outline-danger btn-sm ms-1")
                  )
                ),
                
                DTOutput("tabla_items_actuales"),
                
                hr(class = "my-3"),
                
                fluidRow(
                  column(6, div(
                    class = "p-3 bg-light border rounded h-100 d-flex flex-column justify-content-center",
                    div(style = "margin-bottom: -15px;", numericInput("abono", "Abona ($):", value = 0.00, min = 0, step = 0.50, width = "100%"))
                  )),
                  column(6, div(
                    class = "p-3 bg-light border rounded h-100 d-flex flex-column justify-content-center text-center",
                    h6("SALDO PENDIENTE", style = "margin-bottom: 4px; font-weight: bold;"),
                    h3(textOutput("txt_saldo"), style = "font-weight: bold; color: #C0392B; margin: 0;")
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
        selectInput("filtro_estado_kds", NULL, choices = c("Todos los Activos", "Pendiente", "En Cocina", "Listo", "Entregado"), width = "250px")
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
        h5("Registro Maestro de Órdenes", style = "color: #5B2C6F; font-weight: bold;"),
        DTOutput("tabla_historial_ordenes")
      ),
      div(
        class = "bg-white p-4 border rounded shadow-sm",
        h5("Detalle de Ítems Vendidos", style = "color: #5B2C6F; font-weight: bold;"),
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
          selectInput("nuevo_prod_cat", "Categoría:", choices = c("Horneos", "Platos Fuertes", "Bebidas", "Postres", "Adicionales"), width = "100%"),
          textInput("nuevo_prod_desc", "Nombre del Producto:", width = "100%"),
          numericInput("nuevo_prod_precio", "Precio ($):", value = 5.00, min = 0, step = 0.25, width = "100%"),
          actionButton("btn_guardar_prod", "Agregar al Menú", class = "btn-primary w-100")
        )),
        column(8, div(
          class = "bg-white p-4 border rounded shadow-sm",
          h5("Carta Actual NUWA", style = "color: #5B2C6F; font-weight: bold;"),
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
### Bloque 2: Lógica del Servidor (`server`) y Lanzamiento

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
  
  output$badge_n_orden <- renderUI({
    if (is.null(rv$orden_en_edicion)) {
      span(paste("N° ORDEN:", siguiente_orden()), class = "badge bg-success", style = "font-size: 1.15em; padding: 8px 14px;")
    } else if (es_solo_imprimir()) {
      span(paste("N° ORDEN:", rv$orden_en_edicion), class = "badge bg-light text-dark", style = "font-size: 1.15em; padding: 8px 14px;")
    } else {
      span(paste("EDITANDO:", rv$orden_en_edicion), class = "badge bg-warning text-dark", style = "font-size: 1.15em; padding: 8px 14px;")
    }
  })
  
  output$resumen_logistica_badge <- renderUI({
    color_est <- switch(input$estado_orden, "Pendiente" = "bg-secondary", "En Cocina" = "bg-warning text-dark", "Listo" = "bg-info", "Entregado" = "bg-success", "bg-secondary")
    tagList(
      span(input$tipo_entrega, class = "badge bg-primary me-1"),
      span(input$estado_orden, class = paste("badge", color_est))
    )
  })
  
  output$botones_menu_rapido <- renderUI({
    req(nrow(rv$catalogo) > 0)
    btns <- lapply(seq_len(nrow(rv$catalogo)), function(i) {
      prod <- rv$catalogo[i, ]
      actionButton(
        inputId = paste0("quick_prod_", i),
        label = HTML(paste0("<strong>", prod$descripcion, "</strong><br><small>$", sprintf("%.2f", prod$precio_unitario), "</small>")),
        class = "btn-outline-primary btn-sm m-1",
        style = "min-width: 140px; text-align: left;"
      )
    })
    div(class = "d-flex flex-wrap", btns)
  })
  
  observe({
    req(nrow(rv$catalogo) > 0)
    lapply(seq_len(nrow(rv$catalogo)), function(i) {
      observeEvent(input[[paste0("quick_prod_", i)]], {
        if (es_solo_imprimir()) return()
        prod <- rv$catalogo[i, ]
        idx <- which(rv$items_actuales$Descripción == prod$descripcion & rv$items_actuales[["P.Unit"]] == prod$precio_unitario)
        if (length(idx) > 0) {
          rv$items_actuales$Cant[idx] <- rv$items_actuales$Cant[idx] + 1
          rv$items_actuales$Total[idx] <- round(rv$items_actuales$Cant[idx] * prod$precio_unitario, 2)
        } else {
          rv$items_actuales <- bind_rows(rv$items_actuales, tibble(
            Cant = 1, Descripción = prod$descripcion, `P.Unit` = prod$precio_unitario, Total = prod$precio_unitario
          ))
        }
      }, ignoreInit = TRUE)
    })
  })
  
  output$selector_producto_manual <- renderUI({
    selectInput("item_desc", "Producto:", choices = rv$catalogo$descripcion, width = "100%")
  })
  
  observeEvent(input$item_desc, {
    req(input$item_desc)
    prod <- rv$catalogo %>% filter(descripcion == input$item_desc)
    if (nrow(prod) > 0) updateNumericInput(session, "item_precio", value = prod$precio_unitario)
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
  
  observeEvent(input$btn_quitar_ultima, {
    if (nrow(rv$items_actuales) > 0) rv$items_actuales <- rv$items_actuales[-nrow(rv$items_actuales), ]
  })
  
  observeEvent(input$btn_limpiar_items, { rv$items_actuales <- rv$items_actuales[0, ] })
  
  total_orden <- reactive({ sum(rv$items_actuales$Total, na.rm = TRUE) })
  saldo_orden <- reactive({ max(0, total_orden() - ifelse(is.na(input$abono), 0, input$abono)) })
  
  output$txt_saldo <- renderText({ sprintf("$ %.2f", saldo_orden()) })
  
  output$tabla_items_actuales <- renderDT({
    datatable(
      rv$items_actuales,
      rownames = FALSE,
      container = htmltools::withTags(table(
        class = 'display',
        thead(tr(lapply(names(rv$items_actuales), th))),
        tfoot(tr(
          th(colspan = 3, style = "text-align: right; font-weight: bold; color: #5B2C6F;", "TOTAL ORDEN:"),
          th(style = "font-weight: bold; font-size: 1.15em; color: #2C3E50;", sprintf("$ %.2f", total_orden()))
        ))
      )),
      options = list(dom = 't', paging = FALSE, ordering = FALSE)
    ) %>% formatCurrency(c("P.Unit", "Total"), currency = "$ ", digits = 2)
  })
  
  output$botones_inferiores_ui <- renderUI({
    if (es_solo_imprimir()) {
      screenshotButton(id = "area_orden_foto", label = "Descargar Comanda", filename = paste0("Orden_NUWA_", rv$orden_en_edicion), scale = 2, class = "btn-primary btn-lg w-100")
    } else if (!is.null(rv$orden_en_edicion)) {
      fluidRow(
        column(4, screenshotButton(id = "area_orden_foto", label = "Foto", filename = paste0("Orden_NUWA_", rv$orden_en_edicion), scale = 2, class = "btn-outline-primary btn-lg w-100")),
        column(8, actionButton("btn_guardar_orden", paste("ACTUALIZAR ORDEN", rv$orden_en_edicion), class = "btn-warning btn-lg w-100 fw-bold"))
      )
    } else {
      fluidRow(
        column(4, screenshotButton(id = "area_orden_foto", label = "Foto", filename = "Orden_NUWA", scale = 2, class = "btn-outline-primary btn-lg w-100")),
        column(8, actionButton("btn_guardar_orden", "CONFIRMAR Y ENVIAR A COCINA", class = "btn-success btn-lg w-100 fw-bold"))
      )
    }
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
    updateSelectInput(session, "estado_orden", selected = "Pendiente")
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
    id_b <- trimws(input$buscar_id)
    if (id_b == "") { showNotification("Escribe un ID para buscar.", type = "warning"); return() }
    enc <- rv$ordenes %>% filter(trimws(cliente_id) == id_b)
    if (nrow(enc) == 0) {
      rv$ordenes_encontradas <- character()
      showNotification("Cliente nuevo: no hay órdenes previas con ese ID.", type = "warning")
    } else {
      ult <- tail(enc, 1)
      updateTextInput(session, "cliente_nombre", value = ult$cliente_nombre)
      updateTextInput(session, "cliente_id", value = ult$cliente_id)
      updateTextInput(session, "telefono", value = ult$telefono)
      updateTextInput(session, "direccion_entrega", value = ult$direccion_entrega)
      rv$ordenes_encontradas <- setNames(enc$n_orden, paste0("#", enc$n_orden, " (", enc$fecha_entrega, ")"))
      showNotification(paste("Cliente:", ult$cliente_nombre), type = "message")
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
        select(Cant = cantidad, Descripción = descripcion, `P.Unit` = precio_unitario, Total = subtotal)
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
    showNotification(paste("Orden #", num_ord, "enviada correctamente."), type = "message")
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
    tarjetas <- lapply(seq_len(nrow(df)), function(i) {
      ord <- df[i, ]
      items <- rv$detalle %>% filter(n_orden == ord$n_orden)
      lista_items <- lapply(seq_len(nrow(items)), function(j) {
        tags$li(paste0(items$cantidad[j], "x ", items$descripcion[j]))
      })
      borde <- switch(ord$estado, "Pendiente" = "border-secondary", "En Cocina" = "border-warning", "Listo" = "border-info", "Entregado" = "border-success", "border-primary")
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
                tags$strong(paste("Saldo: $", sprintf("%.2f", ord$saldo)), style = "color: #C0392B;")
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
      column(3, div(class = "p-3 bg-white border rounded shadow-sm text-center", tags$small("ÓRDENES TOTALES"), h3(n_ord, style = "color: #5B2C6F; font-weight: bold; margin:0;"))),
      column(3, div(class = "p-3 bg-white border rounded shadow-sm text-center", tags$small("VENTAS BRUTAS"), h3(sprintf("$ %.2f", tot_ventas), style = "color: #2C3E50; font-weight: bold; margin:0;"))),
      column(3, div(class = "p-3 bg-white border rounded shadow-sm text-center", tags$small("COBRADO (ABONOS)"), h3(sprintf("$ %.2f", tot_cobrado), style = "color: #1E8449; font-weight: bold; margin:0;"))),
      column(3, div(class = "p-3 bg-white border rounded shadow-sm text-center", tags$small("POR COBRAR (SALDOS)"), h3(sprintf("$ %.2f", tot_pendiente), style = "color: #C0392B; font-weight: bold; margin:0;")))
    )
  })
  output$tabla_historial_ordenes <- renderDT({
    datatable(rv$ordenes, options = list(pageLength = 10, order = list(list(0, 'desc'))), rownames = FALSE)
  })
  output$tabla_historial_detalle <- renderDT({
    datatable(rv$detalle, options = list(pageLength = 10, order = list(list(0, 'desc'))), rownames = FALSE)
  })
  observeEvent(input$btn_guardar_prod, {
    req(trimws(input$nuevo_prod_desc) != "")
    rv$catalogo <- bind_rows(rv$catalogo, tibble(
      categoria = as.character(input$nuevo_prod_cat),
      descripcion = as.character(input$nuevo_prod_desc),
      precio_unitario = as.numeric(input$nuevo_prod_precio)
    ))
    write.csv(rv$catalogo, "catalogo_nuwa.csv", row.names = FALSE)
    updateTextInput(session, "nuevo_prod_desc", value = "")
    showNotification("Plato agregado al menú.", type = "message")
  })
  output$tabla_catalogo <- renderDT({
    datatable(rv$catalogo, options = list(pageLength = 10), rownames = FALSE)
  })
}
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))