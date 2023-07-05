// 1
// Hogares situados en capital de provincia donde el sustentador principal es hombre
clear all

global datos "Z:\Documents\Mercados, estrategias y regulación\Stata\Datos"
global graficos "Z:\Documents\Mercados, estrategias y regulación\Stata\Video 1\Gráficos"
cd "$datos\"


// PASOS PREVIOS
* Definición de la función para el reshape con 12 códigos
program define reshape_data_12codigos, rclass

	// Argumentos de entrada
    local using ""
    if "`1'" != "" local using "`1'"
	
    // Obtener el nombre de la base de datos a utilizar
    local bbdd "`using'"
	
	// Usar la base de datos especificada
	use "`bbdd'", clear
	
	// Usar strpos() para verificar que BB.DD. es
	// GASTOS
	if strpos("`bbdd'", "EPFgastos") > 0 {
    display "La BB.DD. es la de gastos."
	local year = substr("`bbdd'", -8, 4)
	gen CODIGO_1 = substr(CODIGO,1,2)
	drop CODIGO
	rename CODIGO_1 CODIGO
	collapse (sum) GASTO-GASTNOM5, by(NUMERO CODIGO)
	reshape wide GASTO-GASTNOM5, i(NUMERO) j(CODIGO) string // Reorganizamos de formato largo a ancho desde GASTO a GASTMON5
	save "12_GASTOS`year'.dta", replace
	}
	// HOGAR
	else if strpos("`bbdd'", "EPFhogar") > 0 {
    display "La BB.DD. es la de características del hogar. No hay que hacer 'reshape'."
	local year = substr("`bbdd'", -8, 4)
	save "HOGAR`year'.dta", replace
	}
	// MIEMBROS HOGAR
	else if strpos("`bbdd'", "EPFmhogar") > 0 {
    display "La BB.DD. es la de miembros del hogar."
	local year = substr("`bbdd'", -8, 4)
	reshape wide CATEGMH-ADULTO, i(NUMERO) j(NORDEN)
	save "MHOGAR`year'.dta", replace
	}
	
    return local bbdd "`bbdd'"

end

* Definición de la función para el merge
program define merge_data_12codigos, rclass

    // Argumentos de entrada
    local using ""
    if "`1'" != "" local using "`1'"
	
    // Obtener el año
    local year "`using'"
    
	// Usamos y unimos las BB.DD. anteriormente creadas
    use "HOGAR`year'.dta", clear
    merge 1:1 NUMERO using "MHOGAR`year'.dta"
    drop _merge
    merge 1:1 NUMERO using "12_GASTOS`year'.dta"
	drop _merge
    foreach var of varlist *{
         rename `var' `=lower("`var'")'
    }
    save "12_EPF`year'.dta", replace
    
    display "12_EPF`year'.dta ya se ha creado."
end

* Definición de la función para el append con 12 códigos
program define append_data_12codigos, rclass

    // Argumentos de entrada
    local year1 ""
    if "`1'" != "" local year1 "`1'"
    local year2 "`2'"

    // Carga el primer archivo de datos
    use "12_EPF`year1'.dta", clear

    // Utiliza el comando append para unir el segundo archivo de datos
    append using "12_EPF`year2'.dta", force

    // Guarda el archivo unido en un nuevo archivo
    save "12_EPF`year1'_`year2'.dta", replace

    // Muestra un mensaje indicando que la unión se completó
    display "La unión de los datos ha sido completada."

end

reshape_data_12codigos "EPFgastos_2019.dta"
reshape_data_12codigos "EPFgastos_2020.dta"

reshape_data_12codigos "EPFhogar_2019.dta"
reshape_data_12codigos "EPFhogar_2020.dta"

reshape_data_12codigos "EPFmhogar_2019.dta"
reshape_data_12codigos "EPFmhogar_2020.dta"

* Llamada a la función merge_data
merge_data_12codigos "2019"

merge_data_12codigos "2020"

* Creamos una BB.DD. general
append_data_12codigos "2019" "2020"


* Utilizamos la BD
use "12_EPF2019_2020.dta", clear

cd "$graficos\"


// APARTADO A
// i. - Submuestra
preserve
* Eliminamos la parte que no queremos en la submuestra
keep if caprov == "1"
keep if sexosp == "1"

* Calcular el gasto total, el gasto medio por hogar y el gasto medio por persona para cada año
gen gasthog = gastmon / factor
gen gastper = gastmon / factor / nmiemb

collapse (sum) gastmon (mean) gasthog (mean) gastper, by(anoenc)


* Calcular las tasas de variación interanual para cada variable
gen gastmon_var = (gastmon - gastmon[_n-1]) / gastmon[_n-1]
gen gasthog_var = (gasthog - gasthog[_n-1]) / gasthog[_n-1]
gen gastper_var = (gastper - gastper[_n-1]) / gastper[_n-1]

* Crear la tabla
table anoenc, stat(sum gastmon gasthog gastper gastmon_var gasthog_var gastper_var) nototals nformat(%20.2f)

tabstat gastmon gasthog gastper gastmon_var gasthog_var gastper_var, by(anoenc) save

putexcel set tablas_video1.xlsx, replace sheet("Apartado A", replace)
putexcel C2:E3, nformat("#,##0 €")
putexcel F2:H3, nformat("0.00%")
putexcel B2 = `r(name1)' C2 = matrix(r(Stat1)) B3 = `r(name2)' C3 = matrix(r(Stat2)) C1 = "Gasto total" D1 = "Gasto por hogar" E1 = "Gasto por persona" F1 = "Var. gasto total" G1 = "Var. gasto por hogar" H1 = "Var. gasto por persona" A2 = "Submuestra"
putexcel (A2:A3), merge hcenter vcenter
putexcel close

restore


// ii. - España
preserve
* Calcular el gasto total, el gasto medio por hogar y el gasto medio por persona para cada año
gen gasthog = gastmon / factor
gen gastper = gastmon / factor / nmiemb

collapse (sum) gastmon (mean) gasthog (mean) gastper, by(anoenc)


* Calcular las tasas de variación interanual para cada variable
gen gastmon_var = (gastmon - gastmon[_n-1]) / gastmon[_n-1]
gen gasthog_var = (gasthog - gasthog[_n-1]) / gasthog[_n-1]
gen gastper_var = (gastper - gastper[_n-1]) / gastper[_n-1]

* Crear la tabla
table anoenc, stat(sum gastmon gasthog gastper gastmon_var gasthog_var gastper_var) nototals nformat(%20.2f)

tabstat gastmon gasthog gastper gastmon_var gasthog_var gastper_var, by(anoenc) save

putexcel set tablas_video1.xlsx, modify sheet("Apartado A")
putexcel C4:E5, nformat("#,##0 €")
putexcel F4:H5, nformat("0.00%")
putexcel B4 = `r(name1)' C4 = matrix(r(Stat1)) B5 = `r(name2)' C5 = matrix(r(Stat2)) A4 = "España"
putexcel (A4:A5), merge hcenter vcenter
putexcel close

restore



// APARTADO B

// i. - Submuestra
preserve
* Eliminamos la parte que no queremos en la submuestra
keep if caprov == "1"
keep if sexosp == "1"

* Loop y collapse
foreach var of varlist gastomon* {
    gen med`var' = `var' / factor
}

collapse (mean) medgastomon01-medgastomon12, by(anoenc)

reshape long medgastomon, i(anoenc) j(codigo) string

destring codigo, replace

label define codigo 1 "Alimentos y bebidas no alcoholicas" 2 "Bebidas alcoholicas y tabaco" 3 "Vestido y calzado" 4 "Vivienda, agua, electricidad, gas y otros combustibles" 5 "Muebles, articulos del hogar y articulos para el mantenimiento corriente del hogar" 6 "Sanidad" 7 "Transporte" 8 "Comunicaciones" 9 "Ocio y cultura" 10 "Enseñanza" 11 "Restaurantes y hoteles" 12 "Otros bienes y servicios"

label values codigo codigo

rename medgastomon medgasto

* Calcular la suma total por año
egen sum_medgasto = total(medgasto), by(anoenc)

* Calcular la distribución porcentual
gen dist_medgasto = medgasto / sum_medgasto
drop sum_medgasto

* Ordenamos para la diferencia absoluta y la tasa de variación
sort codigo anoenc

* Calcular la diferencia absoluta
gen medgasto_diff = medgasto - medgasto[_n-1]
replace medgasto_diff = . if anoenc == 2019

gen medgasto_var = (medgasto - medgasto[_n-1]) / medgasto[_n-1]
replace medgasto_var = . if anoenc == 2019

* Volvemos al orden anterior
sort anoenc codigo

display "Tabla de la media de gasto, la distribución porcentual, la diferencia y variación anual, por año y código"
table (anoenc codigo), stat(sum medgasto dist_medgasto medgasto_diff medgasto_var) nototals nformat(%20.2f)

* Convertimos el dataset a matriz
mkmat medgasto-medgasto_var, mat(table1)

putexcel set tablas_video1.xlsx, modify sheet("Apartado B", replace)
putexcel C3 = matrix(table1)
putexcel close

qui tabstat anoenc, by(codigo) save
putexcel set tablas_video1.xlsx, modify sheet("Apartado B")
putexcel A3 = "2019" A15 = "2020" B2 = "Grupo ECOICOP" C2 = "Gasto medio por hogar" D2 = "Distribución del gasto" E2 = "Diferencia anual" F2 = "Variación anual" G2 = "Gasto medio por hogar" H2 = "Distribución del gasto" I2 = "Diferencia anual" J2 = "Variación anual" C1 = "Submuestra" G1 = "España"
local codigos ""Alimentos y bebidas no alcoholicas" "Bebidas alcoholicas y tabaco" "Vestido y calzado" "Vivienda, agua, electricidad, gas y otros combustibles" "Muebles, articulos del hogar y articulos para el mantenimiento corriente del hogar" "Sanidad" "Transporte" "Comunicaciones" "Ocio y cultura" "Enseñanza" "Restaurantes y hoteles" "Otros bienes y servicios""
foreach value of numlist 1/12 {
	scalar row2019 = `value' + 2
	scalar row2020 = `value' + 14
	local select: word `value'  of `codigos'
	putexcel B`=row2019' = "`select'"
	putexcel B`=row2020' = "`select'"
}
putexcel (A3:A14), merge hcenter vcenter
putexcel (A15:A26), merge hcenter vcenter
putexcel (C1:F1), merge hcenter vcenter
putexcel (G1:J1), merge hcenter vcenter
putexcel C3:C26, nformat("#,##0 €")
putexcel D3:D26, nformat("0.00%")
putexcel E3:E26, nformat("#,##0 €")
putexcel F3:F26, nformat("0.00%")
putexcel G3:G26, nformat("#,##0 €")
putexcel H3:H26, nformat("0.00%")
putexcel I3:I26, nformat("#,##0 €")
putexcel J3:J26, nformat("0.00%")
putexcel close

restore

// ii. - España
preserve
* Loop y collapse
foreach var of varlist gastomon* {
    gen med`var' = `var' / factor
}

collapse (mean) medgastomon01-medgastomon12, by(anoenc)

reshape long medgastomon, i(anoenc) j(codigo) string

destring codigo, replace

label define codigo 1 "Alimentos y bebidas no alcoholicas" 2 "Bebidas alcoholicas y tabaco" 3 "Vestido y calzado" 4 "Vivienda, agua, electricidad, gas y otros combustibles" 5 "Muebles, articulos del hogar y articulos para el mantenimiento corriente del hogar" 6 "Sanidad" 7 "Transporte" 8 "Comunicaciones" 9 "Ocio y cultura" 10 "Enseñanza" 11 "Restaurantes y hoteles" 12 "Otros bienes y servicios"

label values codigo codigo

rename medgastomon medgasto

* Calcular la suma total por año
egen sum_medgasto = total(medgasto), by(anoenc)

* Calcular la distribución porcentual
gen dist_medgasto = medgasto / sum_medgasto
drop sum_medgasto

* Ordenamos para la diferencia absoluta y la tasa de variación
sort codigo anoenc

* Calcular la diferencia absoluta
gen medgasto_diff = medgasto - medgasto[_n-1]
replace medgasto_diff = . if anoenc == 2019

gen medgasto_var = (medgasto - medgasto[_n-1]) / medgasto[_n-1]
replace medgasto_var = . if anoenc == 2019

* Volvemos al orden anterior
sort anoenc codigo

display "Tabla de la media de gasto, la distribución porcentual, la diferencia y variación anual, por año y código"
table (anoenc codigo), stat(sum medgasto dist_medgasto medgasto_diff medgasto_var) nototals nformat(%20.2f)

* Convertimos el dataset a matriz
mkmat medgasto-medgasto_var, mat(table2)

putexcel set tablas_video1.xlsx, modify sheet("Apartado B")
putexcel G3 = matrix(table2)
putexcel close

restore



// APARTADO C

// i. - Submuestra
preserve
* Eliminamos la parte que no queremos en la submuestra
keep if caprov == "1"
keep if sexosp == "1"

* Loop y collapse
foreach var of varlist gastomon* {
    gen med`var' = `var' / factor
}

collapse (mean) medgastomon01-medgastomon12, by(anoenc)

reshape long medgastomon, i(anoenc) j(codigo) string

destring codigo, replace

label define codigo 1 "Alimentos y bebidas no alcoholicas" 2 "Bebidas alcoholicas y tabaco" 3 "Vestido y calzado" 4 "Vivienda, agua, electricidad, gas y otros combustibles" 5 "Muebles, articulos del hogar y articulos para el mantenimiento corriente del hogar" 6 "Sanidad" 7 "Transporte" 8 "Comunicaciones" 9 "Ocio y cultura" 10 "Enseñanza" 11 "Restaurantes y hoteles" 12 "Otros bienes y servicios"

label values codigo codigo

rename medgastomon medgasto

* Calcular la suma total por año
egen sum_medgasto = total(medgasto), by(anoenc)

* Calcular la distribución porcentual
gen dist_medgasto = 100 * medgasto / sum_medgasto
drop sum_medgasto

* Ordenamos para la diferencia absoluta y la tasa de variación
sort codigo anoenc

* Calcular la diferencia absoluta
gen medgasto_diff = medgasto - medgasto[_n-1]
replace medgasto_diff = . if anoenc == 2019

gen medgasto_var = (medgasto - medgasto[_n-1]) / medgasto[_n-1]
replace medgasto_var = . if anoenc == 2019

* Volvemos al orden anterior
sort anoenc codigo

* Gráfico submuestra
graph hbar dist_medgasto, over(codigo, sort(dist_medgasto)  label(labsize(small))) by(anoenc, style(combine)title("Gráfico 1. Distribución del gasto medio por hogar para la submuestra", pos(6)) note("", pos(7))) blabel(bar, color(white) size(3) format(%3.2f) orientation(horizontal) position(inside)) xsize(12) ytitle("")
graph export "video1_graph1.png", width(4800) height(1800) replace
graph close

restore

// ii. - España
preserve
* Loop y collapse
foreach var of varlist gastomon* {
    gen med`var' = `var' / factor
}

collapse (mean) medgastomon01-medgastomon12, by(anoenc)

reshape long medgastomon, i(anoenc) j(codigo) string

destring codigo, replace

label define codigo 1 "Alimentos y bebidas no alcoholicas" 2 "Bebidas alcoholicas y tabaco" 3 "Vestido y calzado" 4 "Vivienda, agua, electricidad, gas y otros combustibles" 5 "Muebles, articulos del hogar y articulos para el mantenimiento corriente del hogar" 6 "Sanidad" 7 "Transporte" 8 "Comunicaciones" 9 "Ocio y cultura" 10 "Enseñanza" 11 "Restaurantes y hoteles" 12 "Otros bienes y servicios"

label values codigo codigo

rename medgastomon medgasto

* Calcular la suma total por año
egen sum_medgasto = total(medgasto), by(anoenc)

* Calcular la distribución porcentual
gen dist_medgasto = 100 * medgasto / sum_medgasto
drop sum_medgasto

* Ordenamos para la diferencia absoluta y la tasa de variación
sort codigo anoenc

* Calcular la diferencia absoluta
gen medgasto_diff = medgasto - medgasto[_n-1]
replace medgasto_diff = . if anoenc == 2019

gen medgasto_var = (medgasto - medgasto[_n-1]) / medgasto[_n-1]
replace medgasto_var = . if anoenc == 2019

* Volvemos al orden anterior
sort anoenc codigo

* Gráfico España
graph hbar dist_medgasto, over(codigo, sort(dist_medgasto)  label(labsize(small))) by(anoenc, style(combine)title("Gráfico 2. Distribución del gasto medio por hogar para España", pos(6)) note("", pos(7))) blabel(bar, color(white) size(3) format(%3.2f) orientation(horizontal) position(inside)) xsize(12) ytitle("")
graph export "video1_graph2.png", width(4800) height(1800) replace
graph close

restore