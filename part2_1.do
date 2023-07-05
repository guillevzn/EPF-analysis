// 2.1
clear all

global datos "Z:\Documents\Mercados, estrategias y regulación\Stata\Datos"
global graficos "Z:\Documents\Mercados, estrategias y regulación\Stata\Video 2\Gráficos"
cd "$datos\"

*ssc install asdoc
*ssc install extremes

use "EPF2019_EQUIPOS.dta"

cd "$graficos\"


//////////////////////////////////// FASE 1 ////////////////////////////////////

// APARTADO A
sum gastomon_*
corr gastomon_*

sum gastomon_equipos if gastomon_equipos > 0

sum gpc_*
corr gpc_*

sum gpc_equipos if gpc_equipos > 0


// APARTADO B
/*
	Podríamos hacer como en la explicación de los vídeos, hace una aproximación,
	pero en la BD nos proporcionan la renta neta mensual de los hogares
*/
corr renta_pc gastot_pc	


global x nmiemb numperi i.sexosp i.estudiossp edad_hogar numestu i.densidad

/*
Tenemos la renta per cápita anual, el número de miembros en el hogar,
el número de miembros percibidores de ingresos en el hogar,
el sexo del sustentador principal, los estudios del sustentador principal,
la edad media del hogar (media de la fila en cuestión - rowmean),
el número de estudiantes en el hogar y la densidad poblacional.
*/



//////////////////////////////////// FASE 2 ////////////////////////////////////


// PASO 1
* Determinemos cual de los modelos se ajusta mejor a nuestros datos

* Generamos las variables que necesitamos
gen ln_gpc_equipos = ln(gpc_equipos)
label variable ln_gpc_equipos "ln(`: var lab gpc_equipos')"

gen ln_renta_pc = ln(renta_pc)
label variable ln_renta_pc "ln(`: var lab renta_pc')"

gen renta_pc2 = renta_pc^2
label variable renta_pc2 "(`: var lab renta_pc')^2"

/*
* Modelo 1
regress gpc_equipos renta_pc $x, robust
estimates store reg1
testparm renta_pc $x
outreg2 using previas_regresiones_video2.xls, replace ctitle("Linear") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Modelo 2
regress gpc_equipos ln_renta_pc $x, robust
estimates store reg2
testparm ln_renta_pc $x
outreg2 using previas_regresiones_video2.xls, append ctitle("Lin-Log") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Modelo 3
regress ln_gpc_equipos renta_pc $x, robust
estimates store reg3
testparm renta_pc $x
outreg2 using previas_regresiones_video2.xls, append ctitle("Log-Lin") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Modelo 4
regress ln_gpc_equipos ln_renta_pc $x, robust
estimates store reg4
testparm ln_renta_pc $x
outreg2 using previas_regresiones_video2.xls, append ctitle("Logaritmico") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Modelo 5
regress gpc_equipos renta_pc renta_pc2 $x, robust
estimates store reg5
testparm renta_pc renta_pc2 $x
outreg2 using previas_regresiones_video2.xls, append ctitle("Cuadratico") label addstat("F-test", `r(F)', "Prob > F", `r(p)')
*/


// APARTADO A
preserve
local variables gpc_equipos renta_pc
foreach var of local variables {
    gen `var'_missing = `var'
	label variable `var'_missing "`: var lab `var'' (no nulos)"
    replace `var'_missing = . if `var' == 0
}
local variables1 gpc_equipos gpc_equipos_missing renta_pc renta_pc_missing
foreach var of local variables1 {
    local i = `i' + 1
    hist `var', freq name(jar`i', replace) ytitle("Frecuencia") scale(0.85) yla(, format(%5.0f) ang(h))
	local jar  `jar'  jar`i' 
}
local jar jar1 jar2 jar3 jar4
graph combine jar1 jar2, ycommon name(g1, replace)
graph combine jar3 jar4, ycommon name(g2, replace)
graph combine g1 g2, col(1) scale(0.85) title("Gráfico 1. Distribuciones de las variables gasto y renta per cápita (anual)", pos(6)) note("Fuente: INE (EPF)", pos(7)) ysize(14) xsize(20) ycommon
graph export "video2_graph1.png", width(2800) height(1800) replace
graph combine jar2 jar4, col(2) scale(0.85) title("Gráfico 2. Distribuciones de las variables gasto y renta per cápita (anual)", pos(6)) note("Fuente: INE (EPF)", pos(7)) ysize(14) xsize(20)
graph export "video2_graph2.png", width(2800) height(1800) replace
graph close
restore

/*
Aquí en este momento fue en el que nos dimos cuenta que era necesario
eliminar los valores nulos del gasto, no solo porque queremos analizar de
aquellos que sí lo consumen sino también porque distorsionaría enormemente
nuestro objetivo.
*/

* Quitamos los que el gasto sea == 0
drop if gastomon_equipos == 0
/*
Hemos truncado la base de datos, para centrarnos en la relación entre la renta
y el gasto de las personas que sabemos que si consumen.
Es posible que eso nos genere una estimación inconsistente y sesgada, pero lo
estudiaremos en vídeos posteriores.
*/

scatter gpc_equipos renta_pc, ytitle("Gasto per cápita en Equipos") title("Gráfico 3. Dispersión del gasto y renta", pos(6)) note("Fuente: INE (EPF)", pos(7)) xsize(22) ysize(16)
graph export "video2_graph3.png", width(2800) height(1800) replace
graph close

drop if gpc_equipos > 10000

/*
Seguimos estudiando los datos, y en este caso con el método del Z-score para
detectar posibles anomalías o outliers en los mismos.
*/
* Valoración de valores extremos por Z-score
egen zscore_gpc_equipos = std(gpc_equipos)
egen zscore_renta_pc = std(renta_pc)

graph twoway (scatter gpc_equipos renta_pc, msymbol(Oh) msymbol(p) ytitle("Gasto")) (lowess gpc_equipos renta_pc) (qfit gpc_equipos renta_pc), legend(order(2 3) label(2 "lowess Gasto - Renta") label(3 "Ajuste cuadrático") col(1)) title("Todas las observaciones") name(zscore1, replace)
graph twoway (scatter gpc_equipos renta_pc, msymbol(Oh) msymbol(p) ytitle("Gasto")) (lowess gpc_equipos renta_pc) (qfit gpc_equipos renta_pc) if zscore_renta_pc < 3, legend(order(2 3) label(2 "lowess Gasto - Renta") label(3 "Ajuste cuadrático") col(1)) title("Observaciones Z-score Renta < 3") name(zscore2, replace)
graph twoway (scatter gpc_equipos renta_pc, msymbol(Oh) msymbol(p) ytitle("Gasto")) (lowess gpc_equipos renta_pc) (qfit gpc_equipos renta_pc) if zscore_gpc_equipos < 3, legend(order(2 3) label(2 "lowess Gasto - Renta") label(3 "Ajuste cuadrático") col(1)) title("Observaciones Z-score Gasto < 3") name(zscore3, replace)
graph twoway (scatter gpc_equipos renta_pc, msymbol(Oh) msymbol(p) ytitle("Gasto")) (lowess gpc_equipos renta_pc) (qfit gpc_equipos renta_pc) if zscore_renta_pc < 3 & zscore_gpc_equipos < 3, legend(order(2 3) label(2 "lowess Gasto - Renta") label(3 "Ajuste cuadrático") col(1)) title("Observaciones Z-score Renta y Gasto < 3") name(zscore4, replace)

graph combine zscore1 zscore2 zscore3 zscore4, scale(0.80) col(2) title("Gráfico 4. Valores extremos (Z-score)", pos(6)) note("Fuente: INE (EPF)", pos(7)) xsize(22) ysize(16)
graph export "video2_graph4.png", width(2800) height(1800) replace
graph close

/*
En este gráfico estamos viendo un gráfico de puntos suavizado por lowess para ver
con mayor facilidad el peso de los outliers y con ajuste cuadrático.
Tenemos presencia de valores extremos tanto para la renta como para el gasto,
pero estos datos al filtrarlo con el Z-score se modifica ligeramente la
tendencia original, por lo que al mantenerlos seguramente aporten información
al modelo.
*/


// APARTADO B
* Tablas de summarize
replace gpc_equipos = . if gpc_equipos == 0
asdoc sum gpc_equipos renta_pc nmiemb numperi edad_hogar numestu, stat(N mean sd min max) label replace save(video1_tabla1-1.rft) title(Estadistica descriptiva para las variables continuas - Parte 1)
asdoc sum gpc_equipos renta_pc nmiemb numperi edad_hogar numestu, stat(p1 p10 p25 p50 p75 p99) label replace save(video1_tabla1-2.rft) title(Estadistica descriptiva para las variables continuas - Parte 2)
asdoc sum i.sexosp i.estudiossp i.densidad, label replace save(video1_tabla2.rft) title(Estadistica descriptiva para las variables no continuas)


// APARTADO C
/*
Vamos a hacer un ajuste de polinomio local (lpoly) para ver mejor la tendencia
no lineal de los datos, que nos servirá para determinar la relación entre las
variables, así como una posible dispersión.
Asimismo lo representaremos con una curva suavizada por el modelo LOWESS, para
ver también como es desde un punto de vista más suavizado.
En este caso obviamente ambos modelos van de la mano.
*/
graph twoway (scatter gpc_equipos renta_pc, msize(small)), name(lpoly1, replace)
graph twoway (lpoly gpc_equipos renta_pc) (lowess gpc_equipos renta_pc), legend(order(1 2) label(1 "lploy Gasto - Renta") label(2 "lowess Gasto - Renta") col(1)) name(lpoly2, replace)

graph combine lpoly1 lpoly2, scale(0.80) col(2) title("Gráfico 5. Relación de gasto en equipos y renta per cápita", pos(6)) note("Fuente: INE (EPF)", pos(7)) xsize(22) ysize(16)
graph export "video2_graph5.png", width(2800) height(1800) replace
graph close

* Un gráfico de correlación también nos ayudará
pwcorr gpc_equipos renta_pc
return list

putexcel set pwcorr_video2.xlsx, replace sheet("Apartado C", replace)
putexcel B2 = matrix(r(C)) A2 = "Gasto equipos" A3 = "Renta" B1 = "Gasto equipos" C1 = "Renta"
putexcel close

/*
La correlación es positiva, lo cual ya son buenas noticias, pero obviamente
de ser la única variable a tomar en cuenta ya que esta no es especialmente fuerte.
No estamos determinando causalidad en ningún momento.
*/


// APARTADO D
// Después de quitar los valores nulos y todos los cambios volvemos a ejecutar las regresiones
* Modelo 1
regress gpc_equipos renta_pc $x, robust
estimates store lin
testparm renta_pc $x
outreg2 using regresiones_video2.xls, replace ctitle("Linear") label addstat("F-test", `r(F)', "Prob > F", `r(p)')


* Modelo 2
regress gpc_equipos ln_renta_pc $x, robust
estimates store linlog
testparm ln_renta_pc $x
outreg2 using regresiones_video2.xls, append ctitle("Lin-Log") label addstat("F-test", `r(F)', "Prob > F", `r(p)')


* Modelo 3
regress ln_gpc_equipos renta_pc $x, robust
estimates store loglin
testparm renta_pc $x
outreg2 using regresiones_video2.xls, append ctitle("Log-Lin") label addstat("F-test", `r(F)', "Prob > F", `r(p)')


* Modelo 4
regress ln_gpc_equipos ln_renta_pc $x, robust
estimates store loglog
testparm ln_renta_pc $x
outreg2 using regresiones_video2.xls, append ctitle("Logaritmico") label addstat("F-test", `r(F)', "Prob > F", `r(p)')


* Modelo 5
regress gpc_equipos renta_pc renta_pc2 $x, robust
estimates store sq
testparm renta_pc renta_pc2 $x
outreg2 using regresiones_video2.xls, append ctitle("Cuadratico") label addstat("F-test", `r(F)', "Prob > F", `r(p)')



// APARTADO E
**



// APARTADO F
* Modelo 1
estimates restore lin

scalar blin = _b[renta_pc]
predict p_lin


* Modelo 2
estimates restore linlog

scalar blinlog = _b[ln_renta_pc]
predict p_linlog


* Modelo 3
estimates restore loglin

scalar bloglin = _b[renta_pc]
predict p_loglin
predict res_loglin, res
gen p_eres_loglin = exp(res_loglin)
qui sum p_eres_loglin
scalar sres_loglin = r(mean)
gen p_eloglin = exp(p_loglin) * sres_loglin
/*
1º cogemos la beta de la renta
2º hacemos predicciones sobre la variable y sus residuos
3º pasamos de cambios porcentuales a unitarios en la
predicción de sus residuos, aplicando la
exponencial y nos quedamos con la media de este resultado
4º hacemos el mismo cambio de cambios porcentuales a unitarios
aplicando la exponencial, pero esta vez a la predicción de la
variable y la multiplicamos por su media
*/


* Modelo 4
estimates restore loglog

scalar bloglog = _b[ln_renta_pc]
predict p_loglog
predict res_loglog, res
gen p_eres_loglog = exp(res_loglog)
qui sum p_eres_loglog
scalar sres_loglog = r(mean)
gen p_eloglog = exp(p_loglog) * sres_loglog
/*
Realizamos lo mismo que antes ya sucede lo mismo, que la variable dependiente
está expresada en logaritmos.
*/


* Modelo 5
estimates restore sq

scalar bs1 = _b[renta_pc]
scalar bs2 = _b[renta_pc2]
predict p_sq


/*
Para el caso de lin-log y el cudrático necesitaremos la media de la variable
independiente renta.
*/
qui sum renta_pc
scalar mrenta_pc = r(mean)

/*
Las medias de las predicciones del modelo lineal, lin-log y cuadrático
solo serán necesarias para las elasticidades
*/
qui sum p_lin
scalar mp_lin = r(mean)

qui sum p_linlog
scalar mp_linlog = r(mean)

qui sum p_sq
scalar mp_sq = r(mean)

/*
Para el caso del log-lin y log-log cogemos la media de lo
previamente calculado sobre la variable dependiente
*/
qui sum p_eloglin
scalar mp_eloglin = r(mean)

qui sum p_eloglog
scalar mp_eloglog = r(mean)


* Efecto marginales en media
scalar me_lin = blin // M.E. lineal

scalar me_linlog = blinlog / mrenta_pc // M.E. lin-log

scalar me_loglin = bloglin * mp_eloglin // M.E. log-lin

scalar me_loglog = bloglog * mp_eloglog / mrenta_pc // M.E. log-log

scalar me_sq = bs1 + 2 * bs2 * mrenta_pc // M.E. cuadrático

scalar list me_lin me_linlog me_loglin me_loglog me_sq

matrix marginal_effects = me_lin\ me_linlog\ me_loglin\ me_loglog\ me_sq
putexcel set efectos_marginales_video2.xlsx, sheet("En media", replace) replace
putexcel B2 = matrix(marginal_effects) B1 = "Efectos marginales en media" A2 = "Lin-Lin" A3 = "Lin-Log" A4 = "Log-Lin" A5 = "Log-Log" A6 = "Cuadrático"
putexcel close


* Efecto marginales para cada individuo
gen me_lin = blin // M.E. lineal

gen me_linlog = blinlog / renta_pc // M.E. lin-log

gen me_loglin = bloglin * p_eloglin // M.E. log-lin (recordamos la importancia de hacerlo con la exponencial)

gen me_loglog = bloglog * p_eloglog / renta_pc // M.E. log-log

gen me_sq = bs1 + 2 * bs2 * renta_pc // M.E. cuadrático

tabstat me_lin me_linlog me_loglin me_loglog me_sq, s(mean p10 p50 p90) save
return list

putexcel set efectos_marginales_video2.xlsx, sheet("Cada individuo", replace) modify
putexcel B2 = matrix(r(StatTotal)) A2 = "Media" A3 = "P10" A4 = "Mediana" A5 = "P90" B1 = "Lin-Lin" C1 = "Lin-Log" D1 = "Log-Lin" E1 = "Log-Log" F1 = "Cuadrático"
putexcel close




// APARTADO G

* Elasticidades en media
scalar es_lin = blin * mrenta_pc / mp_lin // Elas. lineal

scalar es_linlog = blinlog / mp_linlog // Elas. lin-log

scalar es_loglin = bloglin * mrenta_pc // Elas. log-lin

scalar es_loglog = bloglog // Elas. log-log

scalar es_sq = (bs1 + 2 * bs2 * mrenta_pc) * mrenta_pc / mp_sq // Elas. cuadrático

scalar list es_lin es_linlog es_loglin es_loglog es_sq

matrix elasticities = es_lin\ es_linlog\ es_loglin\ es_loglog\ es_sq
putexcel set elasticidades_video2.xlsx, sheet("En media", replace) replace
putexcel B2 = matrix(elasticities) B1 = "Elasticidades en media" A2 = "Lin-Lin" A3 = "Lin-Log" A4 = "Log-Lin" A5 = "Log-Log" A6 = "Cuadrático"
putexcel close


* Elasticidades para cada individuo
gen es_lin = blin * renta_pc / p_lin // Elas. lineal

gen es_linlog = blinlog / p_linlog // Elas. lin-log

gen es_loglin = bloglin * renta_pc // Elas. log-lin

gen es_loglog = bloglog // Elas. log-log

gen es_sq = (bs1 + 2 * bs2 * renta_pc) * renta_pc / p_sq // Elas. cuadrático

tabstat es_lin es_linlog es_loglin es_loglog es_sq, s(mean p10 p50 p90) save
return list

putexcel set elasticidades_video2.xlsx, sheet("Cada individuo", replace) modify
putexcel B2 = matrix(r(StatTotal)) A2 = "Media" A3 = "P10" A4 = "Mediana" A5 = "P90" B1 = "Lin-Lin" C1 = "Lin-Log" D1 = "Log-Lin" E1 = "Log-Log" F1 = "Cuadrático"
putexcel close




// APARTADO H
pwcorr gpc_equipos p_lin p_linlog p_loglin p_loglog p_sq
return list

putexcel set pwcorr_video2.xlsx, modify sheet("Apartado H.ii", replace)
putexcel B2 = matrix(r(C)) A2 = "Gasto observado" A3 = "Est. lin-lin" A4 = "Est. lin-log" A5 = "Est. log-lin" A6 = "Est. log-log" A7 = "Est. cuadrático" B1 = "Gasto observado" C1 = "Est. lin-lin" D1 = "Est. lin-log" E1 = "Est. log-lin" F1 = "Est. log-log" G1 = "Est. cuadrático"
putexcel close



// APARTADO I
* Dependiente de la renta y X
preserve
drop if p_sq < 0
graph twoway (scatter renta_pc p_sq), xtitle("Gasto en equipos per cápita") ytitle("Renta per cápita") title("Modelo cuadrático - Multivariable") name(sq1, replace)
restore

* Solo dependiente de la renta
preserve

regress gpc_equipos renta_pc renta_pc2, robust
predict p_sq2

drop if p_sq2 < 0

graph twoway (scatter renta_pc p_sq2), xtitle("Gasto en equipos per cápita") ytitle("Renta per cápita") title("Modelo cuadrático - Univariable") name(sq2, replace)

restore

graph combine sq1 sq2, ycommon xcommon title("Gráfico 6. Curva de Engel estimada", pos(6)) note("Fuente: INE (EPF)", pos(7)) xsize(12)
graph export "video2_graph6.png", width(5000) height(1800) replace
graph close




// APARTADO J

* Tipo de hogar 1: hogar con un sustentador
preserve

*global x2 nmiemb numperi sexosp estud1-estud8 edad_hogar numestu den1-den3
global x2 numperi

replace renta_pc = round(renta_pc, 1)

replace edad_hogar = round(edad_hogar, 1)

regress gpc_equipos renta_pc renta_pc2 $x2 if numperi == 1, robust
collapse gpc_equipos renta_pc2 $x2, by(renta_pc)
predict p_sq3

drop if p_sq3 < 0

graph twoway (scatter renta_pc p_sq3), xtitle("Gasto en equipos per cápita") ytitle("Renta per cápita") title("Hogar tipo 1: un percibidor") name(sq3, replace)

restore


* Tipo de hogar 2: hogar con 2 sutentadores
preserve

*global x2 nmiemb numperi sexosp estud1-estud8 edad_hogar numestu den1-den3
global x2 numperi

replace renta_pc = round(renta_pc, 1)

replace edad_hogar = round(edad_hogar, 1)

regress gpc_equipos renta_pc renta_pc2 $x2 if numperi == 3, robust
collapse gpc_equipos renta_pc2 $x2, by(renta_pc)
predict p_sq4

drop if p_sq4 < 0

graph twoway (scatter renta_pc p_sq4), xtitle("Gasto en equipos per cápita") ytitle("Renta per cápita") title("Hogar tipo 2: 3 percibidores") name(sq4, replace)

restore


graph combine sq3 sq4, ycommon xcommon title("Gráfico 7. Curva de Engel estimada para dos tipos de hogares", pos(6)) note("Fuente: INE (EPF)", pos(7)) xsize(12)
graph export "video2_graph7.png", width(5000) height(1800) replace
graph close


//////////////////////////////////// FASE 3 ////////////////////////////////////


// APARTADO A
tabstat renta_pc, s(p25 p50 p75 p99)
sum renta_pc, detail
scalar p25renta_pc = r(p25)
scalar p75renta_pc = r(p75)

hist renta_pc, freq xline(`=p25renta_pc') xline(`=p75renta_pc') bin(50) blcolor(black%5) bcolor(%35) ytitle("Frecuencia") title("Gráfico 8. Histograma de la renta", pos(6)) note("Fuente: INE (EPF)", pos(7))
graph export "video2_graph8.png", width(2800) height(1800) replace
graph close


* 3 principales percentiles
gen renta_pc11 = renta_pc * (renta_pc < `=p25renta_pc')
label variable renta_pc11 "Renta per capita hasta `=p25renta_pc' eur."

gen renta_pc12 = renta_pc * (renta_pc >= `=p25renta_pc' & renta_pc < `=p75renta_pc')
label variable renta_pc12 "Renta per capita `=p25renta_pc' - `=p75renta_pc' eur."

gen renta_pc13 = renta_pc * (renta_pc >= `=p75renta_pc')
label variable renta_pc13 "Renta per capita desde `=p75renta_pc' eur."

global kinked1_renta renta_pc11 renta_pc12 renta_pc13


* Modelo por tramos
regress gpc_equipos $kinked1_renta $x, robust
estimates store kinked1
testparm $kinked1_renta $x
outreg2 using kinkedreg_video2.xls, replace ctitle("Modelo por tramos") label addstat("F-test", `r(F)', "Prob > F", `r(p)')



// APARTADO B
* Modelo de interacciones
regress gpc_equipos c.renta_pc#i.estudiossp $x, robust
estimates store interacciones
testparm c.renta_pc#i.estudiossp $x
outreg2 using interaccionesreg_video2.xls, replace ctitle("Modelo de interacciones") label addstat("F-test", `r(F)', "Prob > F", `r(p)')
