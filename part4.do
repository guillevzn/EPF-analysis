// 4
clear all

global datos "Z:\Documents\Mercados, estrategias y regulación\Stata\Datos"
global graficos "Z:\Documents\Mercados, estrategias y regulación\Stata\Video 4\Gráficos"
cd "$datos\"


use "EPF2019_EQUIPOS.dta", clear

cd "$graficos\"


/*
Choice framework:
- Binomial: Consumir o no equipos de procesamiento de la información
- Multinomial: consumir entre ordenadores, accesorios, software y otros equipos
*/

// Pasos previos
* Volvemos a quitar el valor extremo
drop if gpc_equipos > 10000

* Dummy para saber si el hogar consume equipos
gen cons_equipos = (gpc_equipos > 0)
label variable cons_equipos "Consume equipos de procesamiento"

label define cons_equipos 0 "No consume" 1 "Si consume"
label values cons_equipos cons_equipos

* Media y percentiles renta
qui sum renta_pc, detail

scalar renta_pc_mean = r(mean)
scalar renta_pc_p1 = r(p1)
scalar renta_pc_p10 = r(p10)
scalar renta_pc_p25 = r(p25)
scalar renta_pc_p50 = r(p50)
scalar renta_pc_p75 = r(p75)
scalar renta_pc_p90 = r(p90)
scalar renta_pc_p99 = r(p99)

* Media y percentiles edad_hogar
qui sum edad_hogar, detail

scalar edad_hogar_mean = r(mean)
scalar edad_hogar_p1 = r(p1)
scalar edad_hogar_p10 = r(p10)
scalar edad_hogar_p25 = r(p25)
scalar edad_hogar_p50 = r(p50)
scalar edad_hogar_p75 = r(p75)
scalar edad_hogar_p90 = r(p90)
scalar edad_hogar_p99 = r(p99)



// FASE 1: Modelo sobre las características del individuo. Modelo de elección discreta binomial. J=2. Decisión de Consumo.
global x renta_pc c.renta_pc#c.renta_pc nmiemb numperi i.sexosp i.estudiossp edad_hogar c.edad_hogar#c.edad_hogar numestu i.densidad
global vars renta_pc nmiemb numperi i.sexosp i.estudiossp edad_hogar numestu i.densidad

// 1.
tabstat renta_pc nmiemb numperi sexosp estudiossp edad_hogar numestu densidad, stats(mean p10 p25 p50 p75 p90) by(cons_equipos)

sum renta_pc nmiemb numperi sexosp estudiossp edad_hogar numestu densidad, detail


// 2.
* Vemos las variables estadísticamente significativas
stepwise, pr(0.1): logit cons_equipos $x, vce(robust)

* Reformulamos sin estudios
global x renta_pc c.renta_pc#c.renta_pc nmiemb numperi i.sexosp edad_hogar c.edad_hogar#c.edad_hogar numestu i.densidad
global vars renta_pc nmiemb numperi i.sexosp edad_hogar numestu i.densidad

logit cons_equipos $x, vce(robust)
estimates store logit1
predict p_logit1, pr


// 3.
margins, dydx($vars)


// 4.

// i.
margins, dydx(renta_pc) at(renta_pc = (`=renta_pc_p1' `=renta_pc_p10' `=renta_pc_p25' `=renta_pc_p50' `=renta_pc_p75' `=renta_pc_p90' `=renta_pc_p99'))
marginsplot, title("Logit: Efectos marginales en tramos de la renta") xtitle("Renta anual per cápita") ytitle("Efecto en Pr(Cosumo de equipos)")  xlabel(, angle(45)) ylabel(0.000002(0.000002)0.000006)
graph export "efecto_marginal_logit.png", width(3800) height(2800) replace


// ii.
margins, eyex(renta_pc) at(renta_pc = (`=renta_pc_p1' `=renta_pc_p10' `=renta_pc_p25' `=renta_pc_p50' `=renta_pc_p75' `=renta_pc_p90' `=renta_pc_p99'))
marginsplot, title("Logit: Elasticidades en tramos de la renta") xtitle("Renta anual per cápita") ytitle("Efecto en Pr(Cosumo de equipos)")  xlabel(, angle(45))
graph export "elasticidad_logit.png", width(3800) height(2800) replace


// 5.

// i.
sum p_logit1 cons_equipos

preserve

gen renta_pc_round = renta_pc

replace renta_pc_round = round(renta_pc_round, 500)

collapse p_logit1, by(renta_pc_round)

graph twoway (scatter p_logit1 renta_pc_round , msize(vsmall)) (lowess p_logit1 renta_pc_round) , title("Predicción vs renta") ytitle("Predicción de la decisión de consumir equipos") xtitle("Renta del hogar per cápita") legend(order(2 "lowess observaciones y predicciones"))
graph export "prediccion_logit.png", width(3800) height(2800) replace

restore


// ii.
estat classification


// iii.
lroc, title ("Curva ROC") xtitle("1 - especificidad") ytitle("Sensibilidad")
graph export "roc_logit.png", width(3800) height(2800) replace


// iv.
lsens, title("Decisión de compra") xtitle("Límite de probabilidad") ytitle("Sensibilidad/especificidad") legend(order(1 "Sensibilidad" 2 "Especificidad"))
graph export "lsens_logit.png", width(3800) height(2800) replace

// v.



// FASE 2: Modelo sobre las características del individuo. Modelo de elección discreta multinomial.
// Definimos alternativa
// Variables de pesos
local nombres _ordenadores _accesorios _software _otrosequipos

foreach var of local nombres {
    gen p`var' = gpc`var' / gpc_equipos
}

* Vemos posibles distribuciones
preserve

egen p_ordyacc = rowtotal(p_ordenadores p_accesorios)
egen p_ordysoft = rowtotal(p_ordenadores p_software)
egen p_ordyotros = rowtotal(p_ordenadores p_otrosequipos)

contract p_ordyacc p_ordysoft p_ordyotros p_ordenadores p_accesorios p_software p_otrosequipos

gsort - _freq

restore

* Definimos la variable
gen byte alt = .
replace alt = 1 if gpc_equipos == 0
replace alt = 2 if p_ordenadores >= 0.75 & gpc_equipos > 0
replace alt = 3 if p_accesorios >= 0.75 & gpc_equipos > 0
replace alt = 4 if p_software >= 0.75 & gpc_equipos > 0
replace alt = 5 if p_otrosequipos >= 0.75 & gpc_equipos > 0
replace alt = 6 if p_ordenadores > 0 & p_ordenadores < 0.75 & p_accesorios > 0 & p_accesorios < 0.75 & p_software > 0 & p_software < 0.75 & p_otrosequipos > 0 & p_otrosequipos < 0.75 & gpc_equipos > 0
replace alt = 7 if alt == .

label variable alt "Alternativas de consumo"

label define alt 1 "No consume equipos" 2 "Principalmente ordenadores" 3 "Principalmente accesorios" 4 "Principalmente software" 5 "Principalmente otros equipos"  6 "Todos (sin consumo principal)" 7 "Otro"
label values alt alt



// 1.
tab alt

/*
Después de haber visto las distribuciones y ver como funciona esta especificación
creemos que no conviene segmentar más porque puede ser excesivamente
específico y no aportar información relevante.
*/


// 2.
tabstat renta_pc nmiemb numperi sexosp edad_hogar numestu densidad, stats(mean p10 p25 p50 p75 p90) by(alt)


// 3.


// 4.
stepwise, pr(0.1): mlogit alt $x, base(1) nolog

mlogit alt $x, base(1) nolog
estimate store mlogit1

// i.
test [1]renta_pc = [2]renta_pc
test [1]renta_pc = [4]renta_pc

test [1]numperi = [2]numperi
test [1]numperi = [3]numperi

test [1]edad_hogar = [5]edad_hogar
test [1]edad_hogar = [7]edad_hogar


// ii.


// iii.
margins, dydx(renta_pc) atmeans


// 5.
mlogit alt $x, base(1) nolog rrr


// 6.


// 7.
estimates restore mlogit1
margins, dydx(renta_pc) at(renta_pc = (`=renta_pc_p1' `=renta_pc_p10' `=renta_pc_p25' `=renta_pc_p50' `=renta_pc_p75' `=renta_pc_p90' `=renta_pc_p99'))
marginsplot, title("Mlogit: Efectos marginales en tramos de la renta") xtitle("Renta anual per cápita") ytitle("Efecto en la probabilidad") legend(order(1 "Alternativa 1" 2 "Alternativa 2" 3 "Alternativa 3" 4 "Alternativa 4" 5 "Alternativa 5" 6 "Alternativa 6" 7 "Alternativa 7")) xlabel(, angle(45)) noci
graph export "efecto_marginal_mlogit.png", width(4000) height(2800) replace


// 8.
* Grupos de renta
gen byte grenta = .
replace grenta = 0 if renta_pc < `=renta_pc_p1'
replace grenta = 1 if renta_pc >= `=renta_pc_p1' & renta_pc < `=renta_pc_p10'
replace grenta = 2 if renta_pc >= `=renta_pc_p10' & renta_pc < `=renta_pc_p25'
replace grenta = 3 if renta_pc >= `=renta_pc_p25' & renta_pc < `=renta_pc_p50'
replace grenta = 4 if renta_pc >= `=renta_pc_p50' & renta_pc < `=renta_pc_p75'
replace grenta = 5 if renta_pc >= `=renta_pc_p75' & renta_pc < `=renta_pc_p90'
replace grenta = 6 if renta_pc >= `=renta_pc_p90' & renta_pc < `=renta_pc_p99'
replace grenta = 7 if renta_pc >= `=renta_pc_p99'

global x_grupos i.grenta nmiemb numperi i.sexosp i.estudiossp edad_hogar numestu i.densidad

mlogit alt $x_grupos, base(1) nolog

mlogit alt $x_grupos, base(1) nolog rrr


// 9.
estimates restore mlogit1

* Efecto marginal
margins, dydx(edad_hogar) at(edad_hogar = (`=edad_hogar_p1' `=edad_hogar_p10' `=edad_hogar_p25' `=edad_hogar_p50' `=edad_hogar_p75' `=edad_hogar_p90' `=edad_hogar_p99'))
marginsplot, title("Efectos marginales de la edad media sobre las alternativas") xtitle("Edad media del hogar") ytitle("Efecto en la probabilidad") legend(order(1 "Alternativa 1" 2 "Alternativa 2" 3 "Alternativa 3" 4 "Alternativa 4" 5 "Alternativa 5" 6 "Alternativa 6" 7 "Alternativa 7")) xlabel(, angle(45)) noci
graph export "efecto_marginal_edad_mlogit.png", width(4000) height(2800) replace

* Elasticidad
margins, eyex(edad_hogar) at(edad_hogar = (`=edad_hogar_p1' `=edad_hogar_p10' `=edad_hogar_p25' `=edad_hogar_p50' `=edad_hogar_p75' `=edad_hogar_p90' `=edad_hogar_p99'))
marginsplot, title("Elasticidades de la edad media sobre las alternativas") xtitle("Edad media del hogar") ytitle("Efecto en la probabilidad") legend(order(1 "Alternativa 1" 2 "Alternativa 2" 3 "Alternativa 3" 4 "Alternativa 4" 5 "Alternativa 5" 6 "Alternativa 6" 7 "Alternativa 7")) xlabel(, angle(45)) noci
graph export "elasticidad_edad_mlogit.png", width(4000) height(2800) replace
