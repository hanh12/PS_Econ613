*HW2
*2.1 Data Creation

set obs  10000
set seed 100
gen X1 = runiform(1,3)
gen X2 = rgamma(3,2)
gen X3 = rbinomial(10000,0.3)
gen eps = rnormal(2,1)
gen Y = 0.5 + 1.2*X1 + (-0.9)*X2 + 0.1*X3 + eps
gen ydum = 0
replace ydum=1 if Y>299.4644

*2.2 OLS
*2.2.1
reg Y X1
*2.2.2
set obs 10000
egen const = fill(1 1 1 1 1 1 1 1 1 1)
reg Y const X1 X2 X3,noconstant
*2.2.3 bootstrap
bootstrap,reps(49):regress Y X1 X2 X3
bootstrap,reps(499):regress Y X1 X2 X3

*2.4 Discrete Choice & 2.5 marginal effects & delta method
*probit
probit ydum X1 X2 X3
margins, dydx(*) atmeans vce(delta)
*logit
logit ydum X1 X2 X3
margins, dydx(*) atmeans vce(delta)

*linear probability model
regress ydum X1 X2 X3, vce(robust)

*2.5 bootstrap
probit ydum X1 X2 X3, vce(bootstrap, reps(499))
logit ydum X1 X2 X3, vce(bootstrap, reps(499))

*HW 3
*3.1 
*question 1
clear
insheet using/Users/jane/Desktop/613/HW3/product.csv
sort hhid
save data1,replace
clear
insheet using/Users/jane/Desktop/613/HW3/demos.csv
save data2,replace
sort hhid
merge 1:m hhid using data1.dta
summarize ppk_stk pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub ppk_tub pfl_tub phse_tub
*question 2
tabulate choice
*question 3
tabulate income choice

*3.2&4
*clogit
clear
insheet using/Users/jane/Desktop/613/HW3/product.csv
sort hhid
save data1,replace
clear
insheet using/Users/jane/Desktop/613/HW3/demos.csv
save data2,replace
sort hhid
merge 1:m hhid using data1.dta
gen n = 4470
gen v2 = _n

rename (ppk_stk pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub ppk_tub pfl_tub phse_tub)(c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)
reshape long c,i(v2) j(price)

gen dum = cond(price == choice,1,0)

asclogit dum c,case(v2) alternatives(price)

est sto c_logit
estat mfx

*3.3&4

asclogit d, case(v2) alternatives(price) casevar(income)

est sto m_logit
estat mfx

*3.5
asmixlogit dum, random(c) casevar(income) alternatives(price) case(v2)
estimate store haha

drop if choice == 10
drop if price == 10
asmixlogit dum, random(c) casevar(income) alternative(price) case(v2)
estimate store hahapartial
hausman hahapartial haha, alleqs constant

*HW 4
*question 1
clear
insheet using /Users/jane/Desktop/613/HW4/KoopTobias.csv,names
xtset personid timetrnd
bysort personid: gen t = _n
tabulate timetrnd logwage if personid == 1
tabulate timetrnd logwage if personid == 2
tabulate timetrnd logwage if personid == 3
tabulate timetrnd logwage if personid == 4
tabulate timetrnd logwage if personid == 5

*question 2
*between estimator
xtreg logwage educ potexper, re

*within estimator
xtreg logwage educ potexper,be 

*first time difference estimator
*As we take the first difference in Stata, the default setting is to take the difference for balanced data-
*for example when personid=1, Stata only take difference between t=5 and t=6, and omit other timetrend. 
gen logwage_D = D.logwage
gen educ_D = D.educ
gen potexper_D = D.potexper
xtreg logwage_D educ_D potexper_D, fe

































