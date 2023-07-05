// 3
clear all

global datos "Z:\Documents\Mercados, estrategias y regulación\Stata\Datos"
global graficos "Z:\Documents\Mercados, estrategias y regulación\Stata\Video 3\Gráficos"
cd "$datos\"


use "EPF2019_EQUIPOS.dta", clear

cd "$graficos\"
*ssc install psmatch2
*ssc install estout
/*
A diferencia del video anterior vamos a usar la BD completa teniendo en cuenta
los datos de gasto que no consumen nada.
*/

gen renta_pc2 = renta_pc^2
label variable renta_pc2 "(`: var lab renta_pc')^2"


// APARTADO A
// i.
summarize gpc_equipos if gpc_equipos == 0
scalar nulos_gpc = r(N)

summarize gpc_equipos
scalar total_gpc = r(N)

display "Porcentaje de valores nulos " %3.2f scalar(nulos_gpc) / scalar(total_gpc) * 100 "%"

drop if gpc_equipos > 10000


// ii.
global x nmiemb numperi i.sexosp i.estudiossp edad_hogar numestu i.densidad
* Variables factor a multiples dummies
* Estudios del sustentador principal
local estudios_valores 1 2 3 4 5 6 7 8

foreach val of local estudios_valores {
    // Generar la variable dummy correspondiente
    gen estud`val' = (estudiossp == `val')
	label variable estud`val' "`: label (estudiossp) `val''"
}

* Densidad de la población
local densidad_valores 1 2 3

foreach val of local densidad_valores {
    // Generar la variable dummy correspondiente
    gen densidad`val' = (densidad == `val')
	label variable densidad`val' "`: label (densidad) `val''"
}

global x_alt nmiemb numperi sexosp estud1-estud8 edad_hogar numestu densidad1-densidad3

* Dummy para saber si el hogar consume equipos
gen cons_equipos = (gpc_equipos > 0)

* Ejecutamos el comando pstest (Propensity Score Test)
pstest renta_pc renta_pc2 $x_alt, t(cons_equipos) raw


/*
La columna <Treated> se refiere a aquellos individuos que sí consumen el
bien en cuestión. El grupo de <Control> se refiere a aquellos no consumieron
el bien en cuestión en nuestra BD. Mientras el <%bias> muestra el porcentaje de
sesgo relativo en la media de la variable entre el grupo de tratamiento y el
grupo de control. El sesgo relativo se refiere a la diferencia porcentual entre
las medias del grupo de tratamiento y el grupo de control. Si el sesgo es
negativo, significa que la media en el grupo de tratamiento es menor que en el
grupo de control. Si el sesgo es positivo, significa que la media en el grupo 
de tratamiento es mayor que en el grupo de control.

Los resultados indican que hay diferencias significativas entre los grupos de
tratamiento y control en todas las variables medidas exceptuándo el nivel de
densidad intermedio (2) ya que no podemos rechazar la hipótesis nula de que hay
diferencia significativa entre el grupo tratado (o que consumen) del de control
(no consumen).

La relación V(C)/V(T) es el cociente de la varianza del grupo de control (V(C))
y la varianza del grupo tratado (V(T)). Esta relación es importante en los
estudios de impacto porque indica si las diferencias observadas entre los grupos
tratados y de control son debidas a la intervención en sí misma o simplemente al
azar. Si V(C)/V(T) es menor a 1, indica que el grupo de control es más homogéneo
que el grupo tratado y, por lo tanto, es más probable que cualquier diferencia
observada sea significativa. Si es mayor a 1, indica que el grupo tratado es más
homogéneo y que las diferencias observadas son menos confiables como medida del
impacto de la intervención. Aunque seguiremos usando los valores ajustados del
p-valor.

Si se observa que hay diferencias significativas en las medias de las variables
entre los dos grupos, entonces se puede decir que existen diferencias en las
características X entre los individuos que consumen el bien "j" frente a los
que no lo consumen.
*/

* Estimación lowess compradores y no compradores
graph twoway (lowess gpc_equipos renta_pc if gpc_equipos > 0) || (lowess gpc_equipos renta_pc), xtitle("Renta anual per cápita") ytitle("Gasto anual en ordenadores") title("Gráfico 1. Estimación por lowess de la relación del consumo con la renta", pos(6)) note("Fuente: INE (EPF)", pos(7)) legend(order(1 "Compradores" 2 "Todas las observaciones")) xsize(8)
graph export "video3_graph1.png", width(3800) height(1800) replace
graph close

tab cons_equipos, missing


// iii.
* Modelo con todas las observaciones
regress gpc_equipos renta_pc renta_pc2 $x, robust
estimates store reg1
testparm renta_pc $x
outreg2 using regresiones_video3.xls, replace ctitle("Modelo con todas las observaciones") label addstat("F-test", `r(F)', "Prob > F", `r(p)')


* Modelo con obsevaciones de gasto positivas
regress gpc_equipos renta_pc renta_pc2 $x if cons_equipos == 1, robust
estimates store reg2
testparm renta_pc $x
outreg2 using regresiones_video3.xls, append ctitle("Modelo con consumo") label addstat("F-test", `r(F)', "Prob > F", `r(p)')


	// a.
qui sum renta_pc
scalar mrenta_pc = r(mean)

* Reg1
estimates restore reg1

scalar reg1_blin = _b[renta_pc]
scalar reg1_blin2 = _b[renta_pc2]
predict reg1_p_lin

qui sum reg1_p_lin
scalar reg1_mp_lin = r(mean)

* Reg2
estimates restore reg2

scalar reg2_blin = _b[renta_pc]
scalar reg2_blin2 = _b[renta_pc2]
predict reg2_p_lin

qui sum reg2_p_lin
scalar reg2_mp_lin = r(mean)

* Elasticidades en media
scalar reg1_es_lin = (reg1_blin + 2 * reg1_blin2 * mrenta_pc) * mrenta_pc / reg1_mp_lin // Elas. cuadrático - reg1
scalar reg2_es_lin = (reg2_blin + 2 * reg2_blin2 * mrenta_pc) * mrenta_pc / reg2_mp_lin // Elas. cuadrático - reg2

scalar list reg1_es_lin reg2_es_lin

matrix elasticities = reg1_es_lin\ reg2_es_lin
putexcel set elasticidades_video3.xlsx, sheet("En media", replace) replace
putexcel B2 = matrix(elasticities) B1 = "Elasticidades en media" A2 = "Todas las observaciones" A3 = "Sin obs. nulas"
putexcel close

* Elasticidades para cada individuo
gen reg1_es_lin = (reg1_blin + 2 * reg1_blin2 * mrenta_pc) * renta_pc / reg1_p_lin // Elas. cuadrático - reg1
gen reg2_es_lin = (reg2_blin + 2 * reg2_blin2 * mrenta_pc) * renta_pc / reg2_p_lin // Elas. cuadrático - reg2

tabstat reg1_es_lin reg2_es_lin, s(mean p10 p50 p90) save
return list

putexcel set elasticidades_video3.xlsx, sheet("Cada individuo", replace) modify
putexcel B2 = matrix(r(StatTotal)) A2 = "Media" A3 = "P10" A4 = "Mediana" A5 = "P90" B1 = "Todas las observaciones" C1 = "Sin obs. nulas"
putexcel close




// APARTADO B
/*
Nosotros para el modelo de Heckman no tenemos observaciones de cantidades de
consumo, por lo que en lugar de esa variable usaremos la variable de consumo
de ordenadores que hemos generado(=1 si gasto !=0).
Creemos que esto va a ser más eficaz que hacer una estimación de las cantidades
por la naturaleza de este grupo, ya que lo normal es comprarse un dispositivo
o dos (teléfono y ordenadores, o ereader, etc) al año.
La CTA informa que la penetración de los ordenadores personales en los hogares
de Estados Unidos se sitúa en torno al 80%, con una media de 2,4 ordenadores
por hogar(https://shop.cta.tech/collections/research/products/2023-u-s-consumer-technology-software-services-forecast-april-2023).
Este dato podemos extrapolarlo.
*/
sum cons_equipos

/*
El grupo de variables X afectan tanto a la decisión de consumir como a la cantidad
de consumo, mientra que el grupo de variables Z afectan únicamente a la decisión
de consumir.
La elección de qué variables incluir en X y en Z dependerá del contexto
específico del problema y de las hipótesis que se quieran testear en el modelo.
*/
probit cons_equipos c.renta_pc#c.renta_pc $x
/*
Nos dará idicaciones sobre cuales pueden ser relevantes en la decisión
de consumo, si son variables significativas.
*/
global z c.renta_pc c.renta_pc#c.renta_pc nmiemb numperi edad_hogar numestu

/*
Para el margen intensivo ya habíamos calculado la regresión cuando el consumo
es mayor que cero y podíamos ver la variables que eran relevantes en el modelo.
*/
global x c.renta_pc c.renta_pc#c.renta_pc edad_hogar numestu

/*
Es necesario que Z > X para evitar colinealidad.
*/


// i.
heckman gpc_equipos $x, select(cons_equipos = $z) nolog twostep
estimates store heck1


// ii.
/*
Las variables seleccionadas son porque son las representativas individualmente
tanto en el modelo cuadrático como en el modelo probit.
*/


// iii.
/*
El modelo sí nos dice que existe un sesgo de selección endógena ya que rho
es distinto de cero, y theta es estadísticamente significativo.
*/


// iv.
* Hacemos el modelo sin el twostep
heckman gpc_equipos $x, select(cons_equipos = $z) nolog difficult iterate(1000)
estimates store heck2


// v.
* Condicional
predict g_cond, ycond
sum gpc_equipos g_cond
sum gpc_equipos g_cond if gpc_equipos > 0

* Esperado
predict g_exp, yexpected
sum gpc_equipos g_exp

* Probabilidad de selección o ser observado
predict gtot_pr1, psel
sum gtot_pr1 cons_equipos

* Elasticidad ycond
margins, eyex(renta_pc) predict(ycond) at(renta_pc = (500(5000)50500)) atmeans vsquish
marginsplot, title("Heckman: Elasticidad Demanda a la renta: ycond") xtitle("Renta anual per cápita") ytitle("Porcentaje (%)")
graph export "heck_elas_yc.png", width(3800) height(1800) replace

* Elasticidad yexpected
margins, eyex(renta_pc) predict(yexpected) at(renta_pc = (500(5000)50500)) atmeans vsquish
marginsplot, title("Heckman: Elasticidad Demanda a la renta: yexpected") xtitle("Renta anual per cápita") ytitle("Porcentaje (%)")
graph export "heck_elas_ye.png", width(3800) height(1800) replace

/*
La sensibilidad de la demanda a los cambios en la renta es mayor para hogares
con ingresos más altos.
*/


// vi.
qui reg gpc_equipos $x if gpc_equipos > 0

margins, eyex(renta_pc) at(renta_pc = (500(5000)50500)) atmeans vsquish
marginsplot, title("MCO: Elasticidad Demanda a la renta, Gasto > 0") xtitle("Renta anual per cápita") ytitle("Porcentaje (%)")
graph export "mco_elas.png", width(3800) height(1800) replace



// vii.
/*
La elasticidad condicional obtenida es significativamente menor que la
elasticidad esperada, lo que sugiere que puede haber sesgo de selección endógena.
*/


// viii.
gen cons_telefono = (gpc_telefono > 0)

tab cons_telefono cons_equipos


* Estimamos los modelos
heckman gpc_equipos cons_telefono $x, select(cons_equipos = cons_telefono $z) nolog difficult iterate(1000)
estimates store heck3
testparm cons_telefono $x
outreg2 using regresiones_sesgo_video3.xls, replace ctitle("Modelo Heckman") label addstat("Prob > F", `r(p)')

qui reg gpc_equipos cons_telefono $x
estimates store mco_all
testparm cons_telefono $x
outreg2 using regresiones_sesgo_video3.xls, append ctitle("Modelo MCO") label addstat("Prob > F", `r(p)')

qui reg gpc_equipos cons_telefono $x if gpc_equipos > 0
estimates store mco_cond
testparm cons_telefono $x
outreg2 using regresiones_sesgo_video3.xls, append ctitle("Modelo MCO Condicional") label addstat("Prob > F", `r(p)')


esttab heck3 mco_all mco_cond
