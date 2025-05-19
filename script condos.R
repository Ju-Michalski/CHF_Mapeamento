library(sp)
library(sf)
library(leaflet)
library(dplyr)
library(raster)
library(elevatr)
library(progress)
library(osmdata)
library(osmextract)
library(stringr)
library(mapview)

setwd("C:\\Users\\julia_z131ibj\\Documents\\Mestrado\\Dissertação")

# ------------------------------------------------------------
# Etapa manual:
# Baixe o shapefile das regiões do OSM em:
# https://download.geofabrik.de/south-america/brazil.html
# ------------------------------------------------------------

#Como iremos tratar de todo o território nacional, divido em regiões, primeiro é melhor importar em pbf, filtrar e depois converter para shp

sudeste_lines<- oe_read("C:\\Users\\julia_z131ibj\\Documents\\Mestrado\\Dissertação\\OSM pbf\\sudeste-latest.osm.pbf", layer = "lines")
sudeste_polygon<- oe_read("C:\\Users\\julia_z131ibj\\Documents\\Mestrado\\Dissertação\\OSM pbf\\sudeste-latest.osm.pbf", layer = "multipolygons")

##### Linhas #####
# Primeiro filtrar as linhas para living_street ou residencial, quero somente as em área residencial
sudeste_lines <- sudeste_lines %>%
  filter(highway %in% c("residential", "living_street"))

# Agora sim transformar para shp e trabalhar com ele assim
st_write(sudeste_lines,"C:\\Users\\julia_z131ibj\\Documents\\Mestrado\\Dissertação\\OSM shapefiles\\regiões\\sudeste_ruas.shp")

sudeste_ruas <- st_read("C:\\Users\\julia_z131ibj\\Documents\\Mestrado\\Dissertação\\OSM shapefiles\\regiões\\sudeste_ruas.shp")

# Filtrar ruas privadas
sudeste_ruas <- sudeste_ruas %>%
  filter(
    is.na(other_tags) |
      str_detect(other_tags, '"access"=>"private"') |
      str_detect(other_tags, '"access"=>"destination"')
  )

# Selecionar ruas que se intersectam (característico de condomínios)

ix <- st_intersects(sudeste_ruas)

linhas_com_vizinhas <- lengths(ix) > 1

sudeste_ruas <- sudeste_ruas[linhas_com_vizinhas,]

plot(st_geometry(sudeste_ruas))
st_crs(sudeste_ruas)

leaflet(data = sudeste_ruas) %>%
  addTiles() %>%
  addPolygons(color = "blue", weight = 1, fillOpacity = 0.3)

##### Polígonos #####

##Filtrar para características de condomínios (C1)
sudeste_polygon <- sudeste_polygon %>%
  filter(
    landuse == "residential",
    is.na(barrier) | barrier == "wall",
    is.na(type) | type != "boundary"
  )

#Salvar como shp
st_write(sudeste_polygon,"C:\\Users\\julia_z131ibj\\Documents\\Mestrado\\Dissertação\\OSM shapefiles\\regiões\\sudeste_poligono.shp")

sudeste_poligono <- st_read("C:\\Users\\julia_z131ibj\\Documents\\Mestrado\\Dissertação\\OSM shapefiles\\regiões\\sudeste_poligono.shp")

plot(st_geometry(sudeste_poligono))

## Resolver erros do shp (C2)
# Remove geometrias vazias 
sudeste_poligono <- sudeste_poligono[!st_is_empty(sudeste_poligono), ]

leaflet(data = sudeste_poligono_filtrado) %>%
  addTiles() %>%
  addPolygons(color = "blue", 
              weight = 1, 
              fillOpacity = 0.3,
              group = "Condomínios",
              label = ~name) %>%
  addLayersControl(
    overlayGroups = c("Condomínios"),
    options = layersControlOptions(collapsed = FALSE)
  )


# Corrigir geometrias inválidas
sudeste_poligono <- st_make_valid(sudeste_poligono)

# Verificar quais geometrias são válidas
validos <- st_is_valid(sudeste_poligono)

# Filtrar apenas os válidos
sudeste_poligono <- sudeste_poligono[validos, ]

## Calcular métricas geométricas (C3)

perimetros <- st_length(st_cast(sudeste_poligono$geometry, "MULTILINESTRING"))

sudeste_poligono <- sudeste_poligono %>%
  mutate(
    area = st_area(geometry),
    perimetro = perimetros,
    
    area_num = as.numeric(area),
    perimetro_num = as.numeric(perimetro),
    
    circularidade = (4 * pi * area_num) / (perimetro_num^2),
    area_per_perimetro = area_num / perimetro_num
  )

##Tirar polígonos que contenham outros polígonos (provavelmente são bairros ou regiões residenciais sendo classificadas como condomínio)(C4)
# Passo 1: identificar quais polígonos contêm outros

contidos <- st_contains(sudeste_poligono, sudeste_poligono)

# Passo 2: eliminar auto-referência (o polígono que contém ele mesmo)
# Cria um vetor lógico: TRUE se o polígono contém OUTRO polígono
tem_outros_dentro <- lengths(contidos) > 1

# Passo 3: filtrar os que NÃO contêm outros (mantém só os internos ou isolados)
sudeste_poligono <- sudeste_poligono[!tem_outros_dentro, ]


## Excluir termos não característicos de condomínos horizontais. (C5)

# Coluna other_tags
# Vetor com todas as palavras-chave a excluir
palavras_excluir <- c(
  "apartments", "CodFavela", "Comunidade_de_Casas_Irregulares", 
  "building", "irregular", "HDB", "slum", "Favela_\\(Slum\\)",
  "Aglomerado Subnormal", "AEIS", "Comunidade", 
  "CodQuadra", "Geocodigo", "Loteamento aberto", "access:yes",
  "IPP:CADASTRAL"
)

# Juntar os termos em uma expressão regex
regex_excluir <- str_c(palavras_excluir, collapse = "|")

# Filtrar mantendo apenas os que NÃO contêm nenhum dos termos
sudeste_poligono <- sudeste_poligono %>%
  filter(
    is.na(othr_tg) | !str_detect(othr_tg, regex_excluir)
  )

# Coluna name
# 1. Palavras que NÃO podem estar EM QUALQUER PARTE do nome
palavras_name <- c("Favela", "Flat", "Ed[ií]fic[ií]o", "Conjunto Habitacional",
                   "Ocupação", "Estudantil", "Colônia", "Hotel", "hotel", "hostel",
                   "Hostel")

# 2. Palavras que NÃO podem estar NO COMEÇO do nome
palavras_começo_name <- c("Bairro","bairro","bairo","Bairo", "BAIRRO", 
                          "Morro", "Morrinho", "Cidade", "Comunidade",
                          "Jardim", "Sítio", "Praça","Pracinha","rua",
                          "Rua", "Igreja")

# Regex para detectar palavras no meio
regex_meio <- str_c(palavras_name, collapse = "|")

# Regex para detectar palavras no começo
regex_comeco <- str_c("^(", str_c(palavras_começo_name, collapse = "|"), ")")

# 3. Aplicar os filtros todos juntos
sudeste_poligono_filtrado <- sudeste_poligono %>%
  filter(
    # Filtro 1: nome não contém essas palavras (ou é NA)
    is.na(name) | !str_detect(name, regex_meio),
    
    # Filtro 2: nome não começa com essas palavras (ou é NA)
    is.na(name) | !str_detect(name, regex_comeco),
    
    # Filtro 3: eliminar place que não é relevante
    is.na(place) | place %in% c("neighbourhood", "suburb"),
    
    # Filtro 4: condomínio não tem amenity
    is.na(amenity),
    
    # Filtro 5: garantir que seja casa o tipo de building
    is.na(buildng) | buildng == "house",
    
    # Filtro 6: garantir que não seja hotel.
    is.na(tourism) | tourism != "hotel" & tourism != "hostel",
    
    # Filtro 7: Eliminar tamanhos muito pequenos p/ excluir verticais
    area_num >= 13000,
    
    # Filtro 4: excluir se boundary == "administrative" ou historic == "district"
    is.na(boundry) | boundry != "administrative",
    is.na(historc) | historc != "district" & historc != "building"
  )

st_write(sudeste_poligono_filtrado,"C:\\Users\\julia_z131ibj\\Documents\\Mestrado\\Dissertação\\OSM shapefiles\\regiões\\sudeste_filtro.shp")

## Filtro por densidade próxima para eliminar prédios (C6)

# Defina a distância do raio (em metros)
raio <- 80 
tolerancia_area <- 0.20

# Cria uma lista de vizinhos para cada polígono
vizinhanca <- st_is_within_distance(sudeste_poligono_filtrado, sudeste_poligono_filtrado, dist = raio)

# Função para verificar quantos vizinhos têm área semelhante
contar_vizinhos_semelhantes <- function(i) {
  vizinhos <- vizinhanca[[i]]
  area_alvo <- sudeste_poligono_filtrado$area_num[i]
  area_min <- area_alvo * (1 - tolerancia_area)
  area_max <- area_alvo * (1 + tolerancia_area)

# Remove ele mesmo
vizinhos <- setdiff(vizinhos, i)

semelhantes <- vizinhos[
  sudeste_poligono_filtrado$area_num[vizinhos] >= area_min &
    sudeste_poligono_filtrado$area_num[vizinhos] <= area_max
]

return(semelhantes)

}

# Aplica a função
vizinhos_semelhantes_lista <- map(1:nrow(sudeste_poligono_filtrado), contar_vizinhos_semelhantes)

n_semelhantes <- map_int(vizinhos_semelhantes_lista, length)

limite <- 7

centros_suspeitos <- which(n_semelhantes >= limite)

# Junta todos os vizinhos semelhantes desses suspeitos
todos_para_remover <- unique(unlist(vizinhos_semelhantes_lista[centros_suspeitos]))

# Adiciona também os próprios centros
todos_para_remover <- unique(c(todos_para_remover, centros_suspeitos))

# Cria a base filtrada excluindo todos esses
sudeste_poligono_filtrado2 <- sudeste_poligono_filtrado[-todos_para_remover, ]


