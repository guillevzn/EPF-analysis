// 2.0
// CODIGO ECOICOP: 09.1.3
clear all

global datos "Z:\Documents\Mercados, estrategias y regulación\Stata\Datos"
cd "$datos\"
set trace on

// PASOS PREVIOS


* Definición de la función para el reshape
program define reshape_data, rclass

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
	keep if CODIGO == "09131" | CODIGO == "09132" | CODIGO == "09133" | CODIGO == "09134" | CODIGO == "08201" | CODIGO == "08202" | CODIGO == "08203" | CODIGO == "08204"
	reshape wide GASTO-GASTNOM5, i(NUMERO) j(CODIGO) string // Reorganizamos de formato largo a ancho desde GASTO a GASTMON5
	save "GASTOS`year'_EQUIPOS.dta", replace
	}
	// HOGAR
	else if strpos("`bbdd'", "EPFhogar") > 0 {
    display "La BB.DD. es la de características del hogar. No hay que hacer 'reshape'."
	local year = substr("`bbdd'", -8, 4)
	save "HOGAR`year'_EQUIPOS.dta", replace
	*keep if CAPROV == "1"
	}
	// MIEMBROS HOGAR
	else if strpos("`bbdd'", "EPFmhogar") > 0 {
    display "La BB.DD. es la de miembros del hogar."
	local year = substr("`bbdd'", -8, 4)
	reshape wide CATEGMH-ADULTO, i(NUMERO) j(NORDEN)
	save "MHOGAR`year'_EQUIPOS.dta", replace
	}
	
    return local bbdd "`bbdd'"

end

* Definición de la función para limpiar la BBDD y quedarnos con los productos y la estructura necesaria
program define clean_data, rclass
	
	// Definimos todos los códigos en una local
	
	local codigos 09131 09132 09133 09134 08201 08202 08203 08204
	local nombres _ordenadores _accesorios _software _otrosequipos _fijo _movil _otrostelefonos _reparaciontelefonos
	local labels_nom ""Ordenadores personales" "Accesorios para equipos del procesamiento de la información" "Software"  "Calculadores y otro material de procesamiento de la informacion" "Equipos de telefono fijo" "Equipos de telefono movil" "Otros equipos de telefono y fax" "Reparacion de equipos de telefono y fax""
	
	// Hacemos un loop para crear las variables con otros nombres
	* También las individualizamos con factor
	forvalues j = 1/8 {
	    local codigo: word `j' of `codigos'
	    local nombre: word `j' of `nombres'
		local label_nom: word `j' of `labels_nom'
		
		* Anuales
		gen gastomon`nombre' = gastomon`codigo' / factor
		replace gastomon`nombre' = 0 if gastomon`nombre' == .
		label variable gastomon`nombre' "Gasto en `label_nom'"
		
		* Cantidad es una dummy ya que no disponemos de cantidades exactas
		gen cantidad`nombre' = (gastomon`nombre' > 0)
		label variable cantidad`nombre' "Cantidad de `label_nom'"
		/*
		gen cantidad`nombre' = cantidad`codigo' / factor
		replace cantidad`nombre' = 0 if gastomon`nombre' == 0
		label variable cantidad`nombre' "Cantidad de `label_nom'"
		*/
		
		* Anuales per cápita
		gen gpc`nombre' = (gastomon`codigo' / factor) / nmiemb
		replace gpc`nombre' = 0 if gpc`nombre' == .
		label variable gpc`nombre' "Gasto per capita en `label_nom'"

	}
	* Nos interesan los hogares que no consuman para también poder analizarlos
	
	// Quitamos todas las variables no necesarias
	
	foreach num of local codigos {
		foreach i of varlist gasto`num' {
			drop `i'
		}
		foreach i of varlist porcendes`num' {
			drop `i'
		}
		foreach i of varlist porcenimp`num' {
			drop `i'
		}
		foreach i of varlist gastnom1`num' {
			drop `i'
		}
		foreach i of varlist gastnom2`num' {
			drop `i'
		}
		foreach i of varlist gastnom3`num' {
			drop `i'
		}
		foreach i of varlist gastnom4`num' {
			drop `i'
		}
		foreach i of varlist gastnom5`num' {
			drop `i'
		}
		* Con código ya no es necesario
		foreach i of varlist gastomon`num' {
			drop `i'
		}
		foreach i of varlist cantidad`num' {
			drop `i'
		}
	}
	
	// Creamos una variable para la categoría general de Equipos de procesamiento de la informacion
	egen gastomon_equipos = rowtotal(gastomon_ordenadores gastomon_accesorios gastomon_software gastomon_otrosequipos)
	label variable gastomon_equipos "Gasto en Equipos de procesamiento de la informacion"
	
	gen cantidad_equipos = (gastomon_equipos > 0)
	label variable cantidad_equipos "Cantidad de Equipos de procesamiento de la informacion"
	
	egen gpc_equipos = rowtotal(gpc_ordenadores gpc_accesorios gpc_software gpc_otrosequipos)
	label variable gpc_equipos "Gasto per capita en Equipos de procesamiento de la informacion"
	
	// Creamos una variable para la categoría general de Equipos de telefono y fax
	egen gastomon_telefono = rowtotal(gastomon_fijo gastomon_movil gastomon_otrostelefonos gastomon_reparaciontelefonos)
	label variable gastomon_telefono "Gasto en Equipos de telefono y fax"
	
	gen cantidad_telefono = (gastomon_equipos > 0)
	label variable cantidad_telefono "Cantidad de Equipos de telefono y fax"
	
	egen gpc_telefono = rowtotal(gpc_fijo gpc_movil gpc_otrostelefonos gpc_reparaciontelefonos)
	label variable gpc_telefono "Gasto per capita en Equipos de telefono y fax"
	
	// No necesitamos crear la variable precio porque no tenemos cantidades
	
	
	// Renta neta percibida en el hogar
	/*
	Podríamos hacer como en la explicación de los vídeos, hace una aproximación,
	pero en la BD nos proporcionan la renta neta mensual de los hogares
	*/
	gen gastot_pc = (gastot / factor) / nmiemb
	
	
	gen renta = impexac * 12 // impexac es mensual
	
	gen renta_pc = renta / nmiemb
	
	
	label variable renta "Renta neta del hogar anual"
	
	label variable renta_pc "Renta neta del hogar anual per capita"
	
	// Número de miembros percibidores de ingresos
	replace numperi = . if numperi == -9
	
	label variable numperi "Miembros percibidores de ingresos"
	
	
	// Número de miembros del hogar
	label variable nmiemb "Numero de miembros del hogar"
	
	
	// Limpieza de datos sustentados principal
	destring sexosp, replace
	replace sexosp = 0 if sexosp == 6
	replace sexosp = . if sexosp == -9
	
	label variable sexosp "Sexo del sustentador principal"
	
	label define sexosp 0 "Mujer" 1 "Hombre"
	label values sexosp sexosp
	
	
	// Limpieza de datos del nivel de estudios
	destring estudiossp, replace
	
	label variable estudiossp "Nivel de estudios del sustentador principal"
	
	label define estudiossp 1 "Analfabeto" 2 "Educacion primaria" 3 "Educacion secundaria" 4 "Bachiller o FP" 5 "Grado superior" 6 "Graduado" 7 "Master o Licenciado" 8 "Doctorado"
	label values estudiossp estudiossp
	
	// La edad media del hogar
	* No nos interesa únicamente la edad del sustentador principal
	egen edad_hogar = rowmean(edad1 edad2 edad3 edad4 edad5 edad6 edad7 edad8 edad9 edad10 edad11)
	
	label variable edad_hogar "Edad media del hogar"
	
	// Número de estudiantes del hogar
	replace numestu = . if numestu == -9
	
	label variable numestu "Numero de estudiantes en el hogar"
	
	// Densidad poblacional o núcleo urbano
	destring densidad, replace
	
	label variable densidad "Densidad poblacional"
	
	label define densidad 1 "Zona densamente poblada" 2 "Zona intermedia" 3 "Zona diseminada"
	label values densidad densidad
	
	
end

* Definición de la función para el merge
program define merge_data, rclass

    // Argumentos de entrada
    local using ""
    if "`1'" != "" local using "`1'"
	
    // Obtener el año
    local year "`using'"
    
	// Usamos y unimos las BB.DD. anteriormente creadas
    use "HOGAR`year'_EQUIPOS.dta", clear
    merge 1:1 NUMERO ANOENC using "MHOGAR`year'_EQUIPOS.dta"
    drop _merge
    merge 1:1 NUMERO ANOENC using "GASTOS`year'_EQUIPOS.dta"
	drop _merge
    foreach var of varlist *{
         rename `var' `=lower("`var'")'
    }
	
	// Ejecutamos la función de clean_data
	clean_data
	
	order anoenc numero gastomon* cantidad* gpc* renta renta_pc nmiemb numperi sexosp estudiossp edad_hogar numestu densidad
	
	sort numero
	
    save "EPF`year'_EQUIPOS.dta", replace
    
    display "EPF`year'_EQUIPOS.dta ya se ha creado."
end

* Definición de la función para el append
program define append_data, rclass

    // Argumentos de entrada
    local year1 ""
    if "`1'" != "" local year1 "`1'"
    local year2 "`2'"

    // Carga el primer archivo de datos
    use "EPF`year1'_EQUIPOS.dta", clear

    // Utiliza el comando append para unir el segundo archivo de datos
    append using "EPF`year2'_EQUIPOS.dta", force
	
	order anoenc numero gastomon* gpc* renta renta_pc nmiemb numperi sexosp estudiossp edad_hogar numestu densidad
	
	sort anoenc numero

    // Guarda el archivo unido en un nuevo archivo
    save "EPF`year1'_`year2'_EQUIPOS.dta", replace

    // Muestra un mensaje indicando que la unión se completó
    display "La unión de los datos ha sido completada."

end


* Llamada a la función de reshape
reshape_data "EPFgastos_2019.dta"
reshape_data "EPFhogar_2019.dta"
reshape_data "EPFmhogar_2019.dta"

reshape_data "EPFgastos_2020.dta"
reshape_data "EPFhogar_2020.dta"
reshape_data "EPFmhogar_2020.dta"


* Llamada a la función merge_data
merge_data "2019"

merge_data "2020"


* Creamos una BB.DD. general
append_data "2019" "2020"
