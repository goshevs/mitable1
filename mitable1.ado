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

	
	
********************************************************************************

*** Sample size
qui mi estimate: mean age
di "Sample size: `e(N)'"

*** Sample size by group
mi xeq: gen id = 1
mi estimate: total id, over(smokes)


** Contn	
capture program drop _tab1margins
program _tab1margins, eclass properties(mi)
	syntax varlist, MODel(string)
	tokenize `varlist'
	
	`model' `1' i.`2' 
	margins `2', post 
	
		
end 

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




