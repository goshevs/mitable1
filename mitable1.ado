********************************************************************************
*** Re-engineering table1 for mi data


webuse mheart7s0, clear
replace hsgrad = round(3 * runiform())  // redefine the var

/* This is output from table1
mi unset
table1, by(smokes) vars(bmi conts \ age contn %2.1f \ hightar cat)

  +-----------------------------------------------------------------------------------------------------------------------+
  | Factor                                  Level   smokes = 0                    smokes = 1                      p-value |
  |-----------------------------------------------------------------------------------------------------------------------|
  | N                                               90                            59                                      |
  |-----------------------------------------------------------------------------------------------------------------------|
  | Body Mass Index, kg/m^2, median (IQR)           24.90184 (21.8849, 27.2528)   24.42343 (22.08045, 28.29732)      0.86 |
  |-----------------------------------------------------------------------------------------------------------------------|
  | Age, in years, mean (SD)                        56.6 (10.8)                   58.4 (12.3)                        0.39 |
  |-----------------------------------------------------------------------------------------------------------------------|
  | Smokes high tar cigarettes              0       90 (100%)                     30 (54%)                         <0.001 |
  |                                         1       0 (0%)                        26 (46%)                                |
  +-----------------------------------------------------------------------------------------------------------------------+


*** List of tests:
** contn -- ttest; anova
** conts -- Wilcoxon rank-sum; Kruskal-Wallis
** cat/bin -- Chi2 (tab, chi2)
** cate/bine -- Fisher exact (tab, exact)
** 


contn - continuous, normally distributed
conts - continuous, skew
cat   - categorical, groups compared using Pearson's chi-squared
cate  - categorical, groups compared using Fisher's exact test
bin   - binary, groups compared using Pearson's chi-squared
bine  - binary, groups compared using Fisher's exact test

*/
	
	
mi impute chained ///
	(regress) bmi age ///
	(logit, conditional(if smokes==1)) hightar ///
    (logit) smokes = attack hsgrad female, add(2)



capture program drop mitable1
program define mitable1, rclass
	
	syntax anything [, by(varname) ] 
	
	*** Parse the input and post to sreturn
	_inputParser `"`anything'"'
	
	*** Retrieve varname, vartype and varformat
	foreach varname in `:s(macros)' {
		
		di _n "VARIABLE: `varname'"
		
		local properties "`s(`varname')'"
		gettoken vartype varformat: properties, parse(" ")
		local varformat = trim("`varformat'")
		
		if "`vartype'" == "Contn" {
			if "`by'" == "" {
				qui mi estimate: regress `varname' // take mean from here
				                                   // test for ~= 0 
			}
			else {
				mi estimate: regress `varname' i.`by'  // take test of difference from here
				mi estimate, cmdok: _tab1eMargins, mod(`e(cmdline_mi)') // take predicted means from here
			}
		}
		else if "`vartype'" == "Conts" {
			if "`by'" == "" {
				mi estimate: qreg `varname'
			}
			else {
				mi estimate: qreg `varname' i.`by'                         // take test of difference from here
				mi estimate, cmdok: _tab1eMargins, mod(`e(cmdline_mi)')    // take predicted medians from here
			}
		}
		else if "`vartype'" == "Cat" {
			*** Here expand on types
		}
		else {
			di in r "`varname': `vartype' is not a valid vartype"
			exit 489
		}
	}
	
	
end


capture program drop _inputParser
program define _inputParser, sclass
	
	args inputString
	
	while regexm(`"`inputString'"', `"([a-zA-Z0-9\_]+)[ ]*=[ ]*\"([a-zA-Z0-9\%\.\, ]+)\""') {
		local myvalue `=regexs(0)'
		local varname `=regexs(1)'
		local cond `=regexs(2)'
		gettoken vartype varformat: cond
		local varformat = trim("`varformat'")
		local inputString = subinstr(`"`inputString'"', `"`myvalue'"', "", .)
		
		sreturn local `varname' = "`vartype' `varformat'"
	}
end

capture program drop _tab1eMargins
program define _tab1eMargins, eclass properties(mi)
	
	syntax, MODel(string)
	
	gettoken left right: model, parse(":")
	local right = subinstr("`right'", ":", "", .)

	`right'

	if regexm("`e(cmdline)'", "i\.([a-zA-Z0-9\_]+)") {
		local myBy `=regexs(1)'
	}
	
	margins `myBy', post 
		
end 


mitable1 (age = "Conts varformat") (bmi = "Cat"), by(smokes)

	
	
		 
		 
exit

	
********************************************************************************

*** Sample size
qui mi estimate: mean age
di "Sample size: `e(N)'"

*** Sample size by group
mi xeq: gen id = 1
mi estimate: total id, over(smokes)


** Contn	


** Contn
mi estimate: regress age i.smokes  // take test of difference from here
mi estimate, cmdok: _tab1margins age smokes, mod(`e(cmd_mi)') // take predicted means from here

** Conts
mi estimate: qreg bmi i.smokes             // take test of difference from here
mi estimate, cmdok: _tab1margins bmi smokes, mod(`e(cmd_mi)') // take predicted medians from here


** Nominal/Ordinal/Binary
**** Nominal
mi estimate: mlogit hsgrad i.smokes 
mi estimate, cmdok: _tab1margins hsgrad smokes, mod(`e(cmd_mi)')

**** Ordinal
mi estimate: ologit hsgrad i.smokes 
mi estimate, cmdok: _tab1margins hsgrad smokes, mod(`e(cmd_mi)')

**** Binary
mi estimate: mlogit hightar i.smokes 
mi estimate, cmdok : _tab1margins hightar smokes, mod(`e(cmd_mi)')




