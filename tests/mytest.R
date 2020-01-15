app <- ShinyDriver$new("../", loadTimeout = 10000)
app$snapshotInit("mytest")

app$setInputs(sidebarCollapsed = FALSE)
app$snapshot()
app$setInputs(selectProvincia = "Albacete")
app$setInputs(selectMunicipio = "Hellín (Albacete)")
# Input 'mapa_groups' was set, but doesn't have an input binding.
app$setInputs(selectNacionalidad = "Total Extranjeros")
app$setInputs(porcentaje = TRUE)
app$setInputs(hombreMujer = TRUE)
# Input 'mapa_shape_mouseover' was set, but doesn't have an input binding.
# Input 'mapa_shape_mouseout' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(selectProvincia2 = "Lleida")
app$setInputs(selectNacionalidad2 = "Total Unión Europea")
app$setInputs(sort = TRUE)
app$setInputs(manWoman = TRUE)
app$setInputs(percentage2 = TRUE)
app$snapshot()
app$setInputs(selectNacionalidad3 = "Reino Unido")
app$snapshot()
