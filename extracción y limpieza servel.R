
library(tidyverse)
library(RSelenium)
library(rvest)
library(glue)

driver <- rsDriver(browser = "firefox",
                   chromever = NULL,
                   phantomver = NULL)

cliente <- driver$client
cliente$open()
# cliente$closeall()

cliente$navigate("https://segundavotacion.servel.cl/")

tempboton <- cliente$findElement(using = "css selector", 
                           value = '[data-id="División Geográfica Chile"]')

tempboton$clickElement()


lapply(1:16, function(r){
  
  
  # navegar region
  temprgion <- cliente$findElement(using = "css selector",
                                   value = '#filtros_filtros > div > div:nth-child(1) > select')
  
  
  cliente$executeScript(
    glue("arguments[0].selectedIndex = {r}; arguments[0].dispatchEvent(new Event('change'));"),
    list(temprgion)
  )
  
  Sys.sleep(1.5)
  
  cur_reg <- cliente$executeScript(
    "var sel = arguments[0]; return sel.options[sel.selectedIndex].text;",
    list(temprgion)
  )[[1]]
  
  
  
  #navegar provincia
  temprovincia <- cliente$findElement(using = "css selector",
                                      value = '#filtros_filtros > div > div:nth-child(2) > select')
  
  opciones <- temprovincia$findChildElements(using = "css selector", value = "option")
  nprovincias <- length(opciones)-1
  
  # iterar provincias
  lapply(1:nprovincias, function(p){
    
    cliente$executeScript(
      glue("arguments[0].selectedIndex = {p}; arguments[0].dispatchEvent(new Event('change'));"),
      list(temprovincia)
    )
    Sys.sleep(1)
    
    # navegar comunas
    tempcomuna<- cliente$findElement(using = "css selector",
                                     value = '#filtros_filtros > div > div:nth-child(3) > select')
    
    opciones <- tempcomuna$findChildElements(using = "css selector", value = "option")
    ncomunas <- length(opciones)-1
    
    lapply(1:ncomunas, function(c){
      
      cliente$executeScript(
        glue("arguments[0].selectedIndex = {c}; arguments[0].dispatchEvent(new Event('change'));"),
        list(tempcomuna)
      )
      
      Sys.sleep(1)
      
      cur_comun <- cliente$executeScript(
        "var sel = arguments[0]; return sel.options[sel.selectedIndex].text;",
        list(tempcomuna)
      )[[1]]
      
      
      temptabla <- cliente$findElement(using = "css selector",
                                       value = "#id_computo_presidente")
      
      htmltabla <- temptabla$getElementAttribute("outerHTML")[[1]]
      
      tempresultados <- htmltabla %>% 
        read_html() %>% 
        html_element("table") %>% 
        html_table(convert = FALSE)
      
      tempresultados$region <- cur_reg
      tempresultados$comuna <- cur_comun
      
      tempresultados
      
    })# fin comunas
    
    
  })# fin provincias
  
  
}) -> listaresultados
# fin regiones
beepr::beep(3)

# =================== CONSOLIDANDO =====================
conso <- data.frame()

for (i in 1:16) {
  
  temporal <- listaresultados[[i]] %>% 
    do.call("bind_rows", .)
  
  conso <- bind_rows(conso, temporal)
  
}

conso$Total <- str_replace_all(conso$Total, "\\.", "") %>% 
  as.numeric()


conso2 <- conso %>% 
  mutate(Candidaturas = case_when(
    Candidaturas == "2 JEANNETTE JARA ROMAN" ~ "jara",
    Candidaturas == "5 JOSE ANTONIO KAST RIST" ~ "kast",
    Candidaturas == "Válidamente Emitidos"  ~ "validos",
    Candidaturas == "Votos Nulos"  ~ "nulos",
    Candidaturas == "Votos en Blanco"   ~ "blancos",
    Candidaturas ==  "Total Votación"  ~ "totalvotos")) %>% 
  select(1, 2, 4, 5) %>% 
  spread(Candidaturas, Total) %>% 
  mutate(p_jara = jara/totalvotos,
         p_kast = kast/totalvotos)


conso2$region <- conso2$region %>% 
  str_replace_all("DE |DEL ", "")

write_rds(conso2, "consoservel2025.rds")













