library(xml2)
library(sf)
parse_sld <- function(.xml) {
  Label <- xml2::xml_text(
    xml2::xml_find_all(
      .xml,
      './/sld:NamedLayer//sld:UserStyle//sld:FeatureTypeStyle//sld:Name'
    )
  )
  Fill <- xml2::xml_text(
    xml2::xml_find_all(
      .xml,
      './/sld:NamedLayer//sld:UserStyle//sld:FeatureTypeStyle//sld:Fill//sld:CssParameter[@name="fill"]'
    )
  )
  setNames(Fill, Label)
}
parse_names <- function(x) {
  vapply(strsplit(x, '-'), function(x) {
    paste0(lapply(x, function(word) {
      sprintf('%s%s', toupper(substring(word, 1,1)), tolower(substring(word, 2,nchar(word))))
    }), collapse = '')
  }, FUN.VALUE = character(1))
}
SLDFiles <- setNames(list.files('inst/extdata', pattern = '.sld$', full.names = TRUE),
                     sub('\\.sld$', '',
                         parse_names(list.files('inst/extdata', pattern = '.sld$'))))
ecopalettes <- lapply(SLDFiles, function(SLDFile) {parse_sld(read_xml(SLDFile))})

ZipFiles <- list.files('inst/extdata/', pattern = '\\.zip$', full.names = TRUE)
Map(f = unzip,
    zipfile = ZipFiles,
    exdir = sub('\\.zip', '', ZipFiles))
keys <- c(
  NorthAmericaEcoregion1 = 'NA_L1KEY',
  NorthAmericaEcoregion2 = 'NA_L2KEY',
  NorthAmericaEcoregion3 = 'NA_L3KEY',
  ContinentalUsEcoregion3 = 'L3_KEY',
  ContinentalUsEcoregion4 = 'L4_KEY')
ShpFiles <- c(NorthAmericaEcoregion1 = 'inst/extdata/na_cec_eco_l1/NA_CEC_Eco_Level1.shp'
              ,NorthAmericaEcoregion2 = 'inst/extdata/na_cec_eco_l2/NA_CEC_Eco_Level2.shp'
              ,NorthAmericaEcoregion3 = 'inst/extdata/na_cec_eco_l3/NA_CEC_Eco_Level3.shp'
              ,ContinentalUsEcoregion3 = 'inst/extdata/us_eco_l3/us_eco_l3.shp'
              ,ContinentalUsEcoregion4 = 'inst/extdata/us_eco_l4/us_eco_l4_no_st.shp')
Map(f = function(varname, ecopalettes, ShpFiles, keys) {
  shp <- st_read(ShpFiles[varname], quiet = TRUE)
  key <- keys[varname]
  ecopalette <- ecopalettes[[varname]]
  shp[['color']] <- ecopalette[shp[[key]]]
  names(shp) <- tolower(names(shp))
  assign(varname, shp, envir = .GlobalEnv)
  anyNA(shp[['color']])
}, varname = names(keys), ecopalettes = list(ecopalettes),
ShpFiles = list(ShpFiles), keys = list(keys))
worldecoregions <- st_read('inst/extdata/official_teow/official/wwf_terr_ecos.shp')
worldecoregions[['ecoregion_color']] <- ecopalettes[['WorldEcoregions']][
  as.character(worldecoregions[['ECO_SYM']])]
anyNA(worldecoregions$eco_color)
biomecodes <- c(
  `1` = 'Tropical and Subtropical Moist Broadleaf Forests'
  ,`2` = 'Tropical and Subtropical Dry Broadleaf Forests'
  ,`3` = 'Tropical and Subtropical Coniferous Forests'
  ,`4` = 'Temperate Broadleaf and Mixed Forests'
  ,`5` = 'Temperate Coniferous Forests'
  ,`6` = 'Boreal Forests/Taiga'
  ,`7` = 'Tropical and subtropical grasslands, savannas, and shrublands'
  ,`8` = 'Temperate Grasslands, Savannas, and Shrublands'
  ,`9` = 'Flooded Grasslands and Savannas'
  ,`10` = 'Montane Grasslands and Shrublands'
  ,`11` = 'Tundra'
  ,`12` = 'Mediterranean Forests, Woodlands, and Scrub'
  ,`13` = 'Deserts and Xeric Shrublands'
  ,`14` = 'Mangroves'
  ,`98` = 'Lakes'
  ,`99` = 'Rock and Ice'
)
worldecoregions[['biome_name']] <- biomecodes[as.character(worldecoregions$BIOME)]
anyNA(worldecoregions$biome_name)
worldecoregions[['biome_color']] <- ecopalettes[['WorldBiomes']][
  as.character(worldecoregions[['biome_name']])]
anyNA(worldecoregions$biome_color)
names(worldecoregions) <- tolower(names(worldecoregions))

usethis::use_data(worldecoregions, compress = "bzip2", overwrite = TRUE)
usethis::use_data(ContinentalUsEcoregion3, compress = "bzip2", overwrite = TRUE)
usethis::use_data(ContinentalUsEcoregion4, compress = "bzip2", overwrite = TRUE)
usethis::use_data(NorthAmericaEcoregion1, compress = "bzip2", overwrite = TRUE)
usethis::use_data(NorthAmericaEcoregion2, compress = "bzip2", overwrite = TRUE)
usethis::use_data(NorthAmericaEcoregion3, compress = "bzip2", overwrite = TRUE)

unlink(list.dirs('inst/extdata/', recursive = FALSE)[-1], recursive = TRUE)
