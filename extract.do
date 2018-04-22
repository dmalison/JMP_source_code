clear all
set more off

*** LOAD BASELINE DATA ***
{
	use idnum ///
		/// Constructed variables
		cm1edu cm1bsex cm1ethrace cm1age cm1relf cm1hhinc cm1adult cm1kids ///
		m1intmon m1intyr ///
		m1a4 m1a12 m1a12a ///
		m1b1a-m1b1b m1b6a-m1b6f m1b7a-m1b7e  m1b12a-m1b12f m1b13a-m1b13e m1b23a-m1b23f m1b25a-m1b25e ///
		m1f6 ///
		m1h3 m1h3a  ///
		m1i1 m1i3 m1i4 m1i4a ///
		m1natwt ///
		using "~/data/Fragile_Families/coremerged/ff_pub_merge2.dta" 

	/// Use core merged files because they are updated more recently

	merge 1:1 idnum using "~/data/Fragile_Families/coremerged/ff_pub_merge2.dta", nogen ///
		keepusing( ///
		/// Constructed variables (see documentation) 
		cf1edu cf1ethrace cf1age cf1hhinc cf1adult cf1kids ///
		f1a4 f1a6 f1a6a ///
		f1b1a f1b1b f1b6a-f1b6f f1b7a-f1b7e f1b12a-f1b12f f1b13a-f1b13e f1b23a-f1b23f f1b25a-f1b25e ///
		f1h3 f1h3a ///
		f1i1 ///
		f1natwt ///
		) 
}
*** GENERATE BASELINE COVARIATES ***
{
	* Mother's education
		gen educ_cat_m = cm1edu  if cm1edu >= 0
	* Father's education
		gen educ_cat_f = cf1edu  if cf1edu >= 0
	* Mother's race
		gen race_m = cm1ethrace if cm1ethrace >= 0
		// recode others as white because only <5% of sample
		// 64% of the "others" group are Asian
		recode race_m (4 = 1) 
	* Father's race
		gen race_f = cf1ethrace if cf1ethrace >= 0
		// recode others as white because only <5% of sample
		// 55% of the "others" group are Asian
		recode race_f (4 = 1)
	* How often mother attends religious services
		gen religious_m = m1f6 if m1f6 >= 0
		replace religious_m = 5 if religious_m == .
	* Child gender
		gen female = (cm1bsex == 2) if cm1bsex >= 0
		replace female = 0 if female == .
	* Mother's age at birth
		gen birthage_m = cm1age if cm1age >= 0
	* How long parents knew each other before pregnancy (in years)
		// use mother's report
		gen rellength = m1b1a if m1b1a >= 0 & m1b1b < 0
		replace rellength = m1b1b/12 if m1b1a < 0 & m1b1b >= 0
		replace rellength = m1b1a + m1b1b/12 if m1b1a >= 0 & m1b1b >= 0
		// generate father's report
		gen rellength_f = f1b1a if f1b1a >= 0 & f1b1b < 0
		replace rellength_f = f1b1b/12 if f1b1a < 0 & f1b1b >= 0
		replace rellength_f = f1b1a + f1b1b/12 if f1b1a >= 0 & f1b1b >= 0
		// use father's report if mother reports knowing father before age 5 or if mother report is missing
		replace rellength = rellength_f if (rellength + 5 > birthage_m) & rellength != . & birthage_m != . & rellength_f < .
		replace rellength = rellength_f if rellength == .
		// topcode if report knowing each other before age 5
		replace rellength = birthage_m - 5 if rellength + 5 > birthage_m & rellength != . & birthage_m != .
		drop rellength_f
	* M has children with other father
		// mark as zero if no other children
		gen M_oth_chd_wo_F = 0 if m1a12 == 2 
		// mark as one if more children than father
		replace M_oth_chd_wo_F = 1 if m1a12a > f1a6a & m1a12a >= 0 & f1a6a >=0 
	* F has children with other mother
		// mark as zero if no other children
		gen F_oth_chd_wo_M = 0 if f1a6 == 2 
		// mark as one if more children than mother
		replace F_oth_chd_wo_M = 1 if m1a12a < f1a6a & m1a12a >= 0 & f1a6a >=0 
}
*** GENERATE BASELINE RELATIONSHIP STATUS ***
{
	gen relstatus_yr0 = cm1relf if cm1relf > 0

	recode relstatus_yr0  ///
		(1 = 1) /// Married
		(2 = 2) /// Cohabitating
		(3 = 3) /// romantic, not cohabitating
		(4 = 4) /// friends
		(5/7 = 5) /// hardly talk, never talk, father unknown

	*   relationship status variables R_0, R_1, ...  are coded as binary (either romantically involved or not)
	*   separation is an absorbing state, so R_t = 0 if R_{t-1} = 0

	gen R_0 = (relstatus_yr0 <= 3) if relstatus_yr0 < .
	gen R_1 = 0 if R_0 == 0 
	gen R_2 = 0 if R_0 == 0
	gen R_3 = 0 if R_0 == 0
	gen R_4 = 0 if R_0 == 0
	gen R_5 = 0 if R_0 == 0
}
*** GENERATE BASELINE RELATIONSHIP QUALITY MEASUREMENTS ***
{
	*** Three-category measurements

	local i_cat3 = 1

	local vars /// How often, if at all, do you have open disagreements about:
		a /// Money? 
		b /// Spending time together?
		c /// Sex?
		d /// The pregnancy? 
		e /// Drinking or drug use 
		f /// Being faithful?....
		// coded as often (1), sometimes (2), or never (3)

	foreach var of local vars {

		gen M_R_0_cat3_`i_cat3' = .

		replace M_R_0_cat3_`i_cat3' = m1b6`var'   if m1b6`var'  > 0 & m1b6`var'  < . // Parents who are no longer together
		replace M_R_0_cat3_`i_cat3' = m1b12`var'  if m1b12`var' > 0 & m1b12`var' < . // Parents who are living with baby's father
		replace M_R_0_cat3_`i_cat3' = m1b23`var'  if m1b23`var' > 0 & m1b23`var' < . // Parents who are married

		local i_cat3 = `i_cat3' + 1 

	}

	local vars /// Now, think about how (FATHER/MOTHER) behaves towards you. For each statement I read, please tell me how often he behaves this way.
		a /// He/She was fair and willing to compromise when you had a disagreement?
		c /// He/She expressed affection or love for you?
		e /// He/She encouraged or helped you to do things that were important to you?
		// coded as often (1), sometimes (2), or never (3)

	foreach var of local vars {

		gen M_R_0_cat3_`i_cat3' = .

		replace M_R_0_cat3_`i_cat3' = 4 - m1b7`var' if m1b7`var' > 0 & m1b7`var' < .    // Parents who are no longer together
		replace M_R_0_cat3_`i_cat3' = 4 - m1b13`var' if m1b13`var' > 0 & m1b13`var' < . // Parents who are living with baby's father
		replace M_R_0_cat3_`i_cat3' = 4 - m1b25`var' if m1b25`var' > 0 & m1b25`var' < . // Parents who are married

		local i_cat3 = `i_cat3' + 1 

	}

	local vars /// Thinking about your past relationship with [BABY’S FATHER/MOTHER], how often would you say that
		d /// He/She insulted or criticized you or your ideas? 
		// coded as often (1), sometimes (2), or never (3)

	foreach var of local vars {

		gen M_R_0_cat3_`i_cat3' = .

		replace M_R_0_cat3_`i_cat3' = m1b7`var'   if m1b7`var' > 0 & m1b7`var' < . // Parents who are no longer together
		replace M_R_0_cat3_`i_cat3' = m1b13`var'  if m1b13`var' > 0 & m1b13`var' < . // Parents who are living with baby's father
		replace M_R_0_cat3_`i_cat3' = m1b25`var'  if m1b25`var' > 0 & m1b25`var' < . // Parents who are married

		local i_cat3 = `i_cat3' + 1 

	}

	// b asks about domestic violence
	// removed because reported at very low frequencies and may measure something else

	egen theta_R_0_3 = rowtotal(M_R_0_cat3*)
	egen theta_R_0_3_ = rownonmiss(M_R_0_cat3*)
	gen theta_R_0 = (theta_R_0_3 - theta_R_0_3_)/2
	replace theta_R_0 = theta_R_0/theta_R_0_3_
	drop theta_R_0_*
}
*** KEEP BASELINE VARIABLES ***
{
	local vars0 idnum educ_cat_m-F_oth_chd_wo_M R* M_R_0* theta_R_0*
	keep `vars0'
}
*** MERGE YEAR 1 FOLLOW UP ***
{
	merge 1:1 idnum using "~/data/Fragile_Families/coremerged/ff_pub_merge2.dta", nogen ///
		keepusing( ///
		m2intmon m2intyr ///
		m2a3 m2a7 m2a7a m2a12 ///
		m2b17a-m2b17f m2b18a-m2b18h m2b41 m2b42a-m2b42h m2b43a-m2b43f /// 	
		m2c1a m2c1b m2c2 m2c2a m2c2g m2c3a-m2c3h m2c10 m2c23? m2c24 m2c24a m2c26 /// 
		m2d4 m2d4a m2d6a-m2d6l m2d8a-m2d8l ///
		m2e2 m2e2c ///
		m2f6 /// 
		m2k1 m2k3 m2k3a* ///
		m2natwt  ///
		cm2relf cm2hhinc cm2adult cm2kids) 
	merge 1:1 idnum using "~/data/Fragile_Families/coremerged/ff_pub_merge2.dta", nogen ///
		keepusing( ///
		f2a5 f2a6 f2a6a f2a11 ///	
		f2b16a-f2b16f f2b17a-f2b17h f2b35 f2b36a-f2b36h  f2b37a-f2b37f ///
		f2d3 f2d3a f2d5a-f2d5i f2d7a-f2d7i ///
		f2k1a ///
		f2natwt ///
		)
}
*** GENERATE YEAR 1 RELATIONSHIP STATUS ***
{
	gen relstatus_yr1 = cm2relf if cm2relf > 0
	recode relstatus_yr1 ///
		(1 = 1)   /// married
		(2 = 2)   /// cohabitating
		(3/4 = 3) /// romantic, not cohabitating
		(6 = 4)   /// friends
		(7/8 = 5) /// not in relationship, father unknown
		(5 = 6)   /// separated/widowed/divorced

	replace R_1 = (relstatus_yr1 <= 3) if relstatus_yr1 < . & R_1 == .

	gen returner = 0
	replace returner = 1 if R_0 == 0 & relstatus_yr1 <= 3

	gen social_f = 0
	replace social_f = 1 if m2e2 == 1

	* update with father's report if missing

	replace R_1 = 1 if R_1 == . & (f2a5 == 1 | f2a5 == 2)
	replace R_1 = 0 if R_1 == . & (f2a5 >= 3 & f2a5 <= 5)

	replace R_2 = 0 if R_1 == 0
	replace R_3 = 0 if R_1 == 0
	replace R_4 = 0 if R_1 == 0
	replace R_5 = 0 if R_1 == 0

}
*** GENERATE COVARIATES ***
{
	* M has other children with father
		// If parents are in a relationship at child's birth, 
		// then assume any children mother gives birth to in year 1 are to child's father
		gen siblings = (m2a12 >= 2) if m2a12 >= 0 & m2a12 < .
		replace siblings = 0 if (m2f6 == 1 | m2f6 == 3) & m2a12 == 2 & R_0 == 1 
		// Use father's report if mother's report missing
		replace siblings = (f2a11 >= 2) if f2a11 > 0 & f2a11 < . & siblings == . 
		
	* M has children with other father
		// If parents are not in a relationship at child's birth, 
		// then assume any children mother gives birth to in year 1 are to different father
		replace M_oth_chd_wo_F = (m2c24 == 1) if m2c24 > 0 & m2c24 < . & M_oth_chd_wo_F == .
		replace M_oth_chd_wo_F = 0 if (m2f6 == 1 | m2f6 == 3) & m2c24a == 1 & R_0 == 0

	* F has children with other mother
		replace F_oth_chd_wo_M = (m2c26 == 1) if m2c26 > 0 & m2c26 < . & F_oth_chd_wo_M == .
		egen half_siblings = rowmax(M_oth_chd_wo_F F_oth_chd_wo_M)

	* Update father's schooling
		replace educ_cat_f = 1 if (f2k1a == 2 | f2k1a == 3) & educ_cat_f == .
		replace educ_cat_f = 2 if (f2k1a == 4 | f2k1a == 5) & educ_cat_f == .
		replace educ_cat_f = 3 if (f2k1a == 6) & educ_cat_f == .
		replace educ_cat_f = 4 if (f2k1a == 8) & educ_cat_f == .
}
*** GENERATE YEAR 1 RELATIONSHIP QUALITY MEASUREMENTS ***
{
	*** Five-category measurements ***

	local i_cat5 = 1

	/// In general, would you say that your relationship with him/her is excellent (1), very good (2), good (3), fair (4), or poor (5)?

	gen M_R_1_cat5_`i_cat5' = 6 - m2d4 if m2d4 > 0 & m2d4 < . // Re-coded so that 1 is poor, 5 is excellent
	local i_cat5 = `i_cat5' + 1

	/// How often do you and [FATHER/MOTHER] argue about things that are important to you?
		/// Coded as Always (1), Often (2), Sometimes (3), Rarely (4), or Never (5)

	gen M_R_1_cat5_`i_cat5' = m2d4a if m2d4a > 0 & m2d4a < .
	local i_cat5 = `i_cat5' + 1

	*** Three-category measurements

	local i_cat3 = 1

	local vars /// Now, think about how (FATHER/MOTHER) behaves towards you. For each statement I read, please tell me how often he behaves this way.
		a /// He/She was fair and willing to compromise when you had a disagreement?
		b /// He/She expressed affection or love for you?
		d /// He/She encouraged or helped you to do things that were important to you?
		k /// He/She listens to you when you need someone to talk to
		l /// He/She really understands your hurts and joys
		// coded as often (3), sometimes (2), or never (1)

	foreach var of local vars {

		gen M_R_1_cat3_`i_cat3' = .

		replace M_R_1_cat3_`i_cat3' = 4 - m2d6`var' if m2d6`var' > 0 & m2d6`var' < . // Parents who are married or romantically involved
		replace M_R_1_cat3_`i_cat3' = 4 - m2d8`var' if m2d8`var' > 0 & m2d8`var' < . // Parents who are no longer together

		local i_cat3 = `i_cat3' + 1 

	}

	local vars /// Thinking about your past relationship with [BABY’S FATHER/MOTHER], how often would you say that
		c /// He/She insulted or criticized you or your ideas? 
		e /// He/She tries to keep you from seeing or talking with your friends or family 
		f /// He/She tries to prevent you from going to work or school
		g /// He/She withholds money, makes you ask for money, or takes your money
		// coded as often (3), sometimes (2), or never (1)

	foreach var of local vars {

		gen M_R_1_cat3_`i_cat3' = .

		replace M_R_1_cat3_`i_cat3' = m2d6`var' if m2d6`var' > 0 & m2d6`var' < .
		replace M_R_1_cat3_`i_cat3' = m2d8`var' if m2d8`var' > 0 & m2d8`var' < .

		local i_cat3 = `i_cat3' + 1 

	}	

	// h,i,j ask about domestic violence
	// removed because reported at very low frequencies and may measure something else

	* Place measurements on 0-1 scale and then average

	egen theta_R_1_3  = rowtotal(M_R_1_cat3*)
	egen theta_R_1_3_ = rownonmiss(M_R_1_cat3*)
	replace theta_R_1_3 = (theta_R_1_3 - theta_R_1_3_)/2
	egen theta_R_1_5  = rowtotal(M_R_1_cat5*)
	egen theta_R_1_5_ = rownonmiss(M_R_1_cat5*)
	replace theta_R_1_5 = (theta_R_1_5 - theta_R_1_5_)/4
	gen theta_R_1 = 1/(theta_R_1_3_ + theta_R_1_5_) * (theta_R_1_3 + theta_R_1_5)

	replace theta_R_1 = 0 if R_0 == 0
	drop theta_R_1_*
}
*** GENERATE YEAR 1 NON-COGNITIVE ABILITY MEASUREMENTS ***
{
	*** Five-category measurements ***

	local vars ///
		/// Using a scale from 1 to 5, where 1 means “not at all like your child,”  5 means “very much like your child,” 
		/// and 2, 3, and 4 mean somewhere in between, tell me how well each statement describes (CHILD).
		b /// Child often fusses and cries
		d /// Child gets upset easily
		e /// Child reacts strongly when upset

	local i_cat5 = 1

	foreach var of local vars {

		gen M_N_1_cat5_`i_cat5' = .

		replace M_N_1_cat5_`i_cat5' = 6 - m2b17`var'  if m2b17`var' > 0 & m2b17`var' < . // Mothers who live with their children
		replace M_N_1_cat5_`i_cat5' = 6 - m2b43`var'  if m2b43`var' > 0 & m2b43`var' < . // Mothers who do not live with their children

		local i_cat5 = `i_cat5' + 1

	}

	foreach var of local vars {

		gen M_N_1_cat5_`i_cat5' = .

		replace M_N_1_cat5_`i_cat5' = 6 - f2b16`var'  if f2b16`var' > 0 & f2b16`var' < . // Fathers who live with their children
		replace M_N_1_cat5_`i_cat5' = 6 - f2b37`var'  if f2b37`var' > 0 & f2b37`var' < . // Fathers who do not live with their children

		local i_cat5 = `i_cat5' + 1

	}

	* Average non-missing measurements

	egen theta_N_1 = rowmean(M_N_1_cat5*)

}
*** KEEP YEAR 1 VARIABLES ***
{
	local vars1 `vars0' siblings half_siblings M_R_1_* M_N_1_* theta_R_1 theta_N_1 returner social_f
	keep `vars1'
}
*** MERGE YEAR 3 FOLLOW UP ***
{
	merge 1:1 idnum using "~/data/Fragile_Families/3_year_followup/InHome3yr.dta", nogen ///
		keepusing( ///
		a27a-a27g ///
		c1a-c1h c6b-c6f ///
		m1-m50 ///
		ppvtstd ppvtstd_m tvipstd tvipstd_m)

	merge 1:1 idnum using "~/data/Fragile_Families/coremerged/ff_pub_merge2.dta", nogen ///
		keepusing( ///
		m3intmon m3intyr ///
		m3a2 m3a4 m3a4a1 ///
		m3b4* m3b31 m3b32* ///
		m3c1a-m3c1c m3c2 m3c2a m3c2e m3c3a-m3c3m ///
		m3d4 m3d5 m3d7a-m3d7l m3d9a-m3d9l ///
		m3e2 m3e2f* ///
		cm3relf cm3hhinc cm3adult cm3kids cm3cogsc ///
		m3natwt)

	merge 1:1 idnum using "~/data/Fragile_Families/coremerged/ff_pub_merge2.dta", nogen ///
		keepusing( ///
		f3a4 f3a4a1 ///
		f3b4* f3b31 f3b32* ///
		f3c1 f3c2 f3c2e f3c3a-f3c3m  ///
		f3d4 f3d5 f3d7a-f3d7l f3d9a-f3d9l /// 
		cf3cogsc ///
		f3natwt )

}
*** GENERATE YEAR 3 RELATIONSHIP STATUS ***
{
	gen relstatus_yr3 = cm3relf if cm3relf > 0
	recode relstatus_yr3 ///
		(1 = 1) 	/// married
		(2 = 2) 	/// cohabitating
		(3/4 = 3) 	/// romantic, not cohabitating
		(6 = 4) 	/// friends
		(7/8 = 5) 	/// not in relationship, father unknown
		(5 = 6) 	/// separated/widowed/divorced

	replace R_2 = (relstatus_yr3 <= 3) if relstatus_yr3 < . & R_2 == . 

	replace returner = 1 if R_1 == 0 & relstatus_yr3 <= 3

	replace social_f = 1 if m3e2 == 1

	replace R_2 = 1 if R_2 == . & (f3a4 == 1 | f3a4 == 2)
	replace R_2 = 0 if R_2 == . & ((f3a4 >= 3 & f3a4 <= 6) | f3a4 == -14)

	replace R_3 = 0 if R_2 == 0
	replace R_4 = 0 if R_2 == 0
	replace R_5 = 0 if R_2 == 0
}
*** GENERATE YEAR 3 RELATIONSHIP QUALITY MEASUREMENTS *** 
{
	*** Five-category measurements *** 

	local i_cat5 = 1

	/// In general, would you say that your relationship with him/her is excellent, very good, good, fair, or poor?

	gen M_R_2_cat5_`i_cat5' = 6 - m3d4 if m3d4 > 0 & m3d4 < .
	local i_cat5 = `i_cat5' + 1

	/// How often do you and [FATHER/MOTHER] argue about things that are important to you?

	gen M_R_2_cat5_`i_cat5' = m3d5 if m3d5 > 0 & m3d5 < .
	local i_cat5 = `i_cat5' + 1

	*** Three-category measurements ***

	local i_cat3 = 1

	local vars /// Now, think about how (FATHER/MOTHER) behaves towards you. For each statement I read, please tell me how often he behaves this way.
		a /// He/She was fair and willing to compromise when you had a disagreement?
		b /// He/She expressed affection or love for you?
		d /// He/She encouraged or helped you to do things that were important to you?
		k /// He/She listens to you when you need someone to talk to
		l /// He/She really understands your hurts and joys

	foreach var of local vars {

		gen M_R_2_cat3_`i_cat3' = .

		replace M_R_2_cat3_`i_cat3' = 4 - m3d7`var' if m3d7`var' > 0 & m3d7`var' < .
		replace M_R_2_cat3_`i_cat3' = 4 - m3d9`var' if m3d9`var' > 0 & m3d9`var' < .

		local i_cat3 = `i_cat3' + 1 

	}	

	local vars /// Thinking about your past relationship with [BABY’S FATHER/MOTHER], how often would you say that
		c /// He/She insulted or criticized you or your ideas? 
		e /// He/She tries to keep you from seeing or talking with your friends or family 
		f /// He/She tries to prevent you from going to work or school
		g /// He/She withholds money, makes you ask for money, or takes your money

	foreach var of local vars {

		gen M_R_2_cat3_`i_cat3' = .

		replace M_R_2_cat3_`i_cat3' = m3d7`var' if m3d7`var' > 0 & m3d7`var' < .
		replace M_R_2_cat3_`i_cat3' = m3d9`var' if m3d9`var' > 0 & m3d9`var' < .

		local i_cat3 = `i_cat3' + 1 

	}

	egen theta_R_2_3  = rowtotal(M_R_2_cat3*)
	egen theta_R_2_3_ = rownonmiss(M_R_2_cat3*)
	replace theta_R_2_3 = (theta_R_2_3 - theta_R_2_3_)/2
	egen theta_R_2_5  = rowtotal(M_R_2_cat5*)
	egen theta_R_2_5_ = rownonmiss(M_R_2_cat5*)
	replace theta_R_2_5 = (theta_R_2_5 - theta_R_2_5_)/4
	gen theta_R_2 = 1/(theta_R_2_3_ + theta_R_2_5_) * (theta_R_2_3 + theta_R_2_5)
	replace theta_R_2 = 0 if R_1 == 0
	drop theta_R_2_*

}
*** GENERATE YEAR 3 NON-COGNITIVE ABILITY MEASUREMENTS ***
{
	local vars ///
		/// Is each statement not true (so far as you know) (0), somewhat or sometimes true (1), very true or
		/// often true for (CHILR) (2)?
		m2c  /// (He/She) can't stand waiting, wants everything now
		m5   /// (He/She) is defiant 
		m6   /// (His/Her) demands must be met immediately 
		m6b  /// (He/She) destroys things belonging to his family or oth children
		m7   /// (He/She) is disobedient 
		m13  /// (He/She) doesn't seem to feel guilty after misbehaving
		m14  /// (He/She) is easily frustrated 
		m18  /// (He/She) gets in many fights 
		m21  /// (He/She) hits others 
		m21a /// (He/She) hurts animals or people without meaning to 
		m23  /// (He/She) has angry moods 
		m26a /// (He/She) physically attacks people
		m28  /// Punishment doesn't change (his/her) behavior 
		m30  /// (He/She) screams a lot 
		m33  /// (He/She) is selfish or won't share 
		m39  /// (He/She) is stubborn, sullen, or irritable 
		m41  /// (He/She) has temper tantrums or hot temper 
		m44  /// (He/She) is uncooperative 
		m48  /// (He/She) wants a lot of attention 
		/// above items taken from CBCL 1.5-5 Aggressive Behavior scale (2000)



	local i_cat3 = 1

	foreach var of local vars {

		gen M_N_2_cat3_`i_cat3' = .

		replace M_N_2_cat3_`i_cat3' = 3 - `var' if `var' >= 0 & `var' < .

		local i_cat3 = `i_cat3' + 1

	}

	egen theta_N_2 = rowmean(M_N_2_cat3*)

}
*** GENERATE YEAR 3 COGNITIVE ABILITY MEASUREMENTS ***
{

	gen M_C_2_1 = ppvtstd if ppvtstd > 0 
	replace M_C_2_1 = tvipstd if M_C_2_1 == . & tvipstd > 0

	gen M_C_2_2 = ppvtstd_m if ppvtstd_m > 0
	replace M_C_2_2 = tvipstd_m if M_C_2_2 == . & tvipstd_m > 0

	replace cm3cogsc = . if cm3cogsc <= 0
	replace cf3cogsc = . if cf3cogsc <= 0

	gen M_C_2_3 = cm3cogsc
	gen M_C_2_4 = cf3cogsc

	* standardize and average measurements

	egen M_C_2_1_ = std(M_C_2_1)
	egen M_C_2_2_ = std(M_C_2_2)
	egen M_C_2_3_ = std(M_C_2_3)
	egen M_C_2_4_ = std(M_C_2_4)

	egen theta_C_2 = rowmean(M_C_2_?_)

	drop M_C_2_?_

}
*** KEEP YEAR 3 VARIABLES ***
{
	local vars2 `vars1' M_R_2_* M_N_2_* M_C_2_* theta_R_2 theta_N_2 theta_C_2
	keep `vars2'
}
*** MERGE YEAR 5 FOLLOW UP ***
{
	merge 1:1 idnum using "~/data/Fragile_Families/coremerged/ff_pub_merge2.dta", nogen ///
		keepusing( ///
		m4intmon m4intyr ///
		m4a2 m4a4 m4a4c m4a7a1 m4a7a2 m4a8a1 m4a8a2 ///
		m4b4a1-m4b4a8 m4b4b1-m4b4b19 m4b25 m4b26a-m4b26h m4b29a1-m4b29a19 /// 
		m4c1a m4c1b m4c2 m4c2a m4c2e m4c3a-m4c3h  ///
		m4d4 m4d6a-m4d6i m4d7a-m4d7p m4d9a-m4d9p ///
		m4e2 m4e2f* m4e2h ///
		cm4relf cm4hhinc cm4adult cm4kids ///
		m4natwt)

	merge 1:1 idnum using "~/data/Fragile_Families/coremerged/ff_pub_merge2.dta", nogen ///
		keepusing( ///
		f4intyr ///
		f4a4 f4a4c f4a7a1 f4a7a2 f4a8a1 f4a8a2 ///
		f4b4a1-f4b4a8 f4b25 f4b26a-f4b26h f4b4b1-f4b4b19 f4b29a1-f4b29a19  ///
		f4c1a f4c2 f4c2e f4c3a-f4c3h ///
		f4d4 f4d6a-f4d6f f4d7a-f4d7p f4d9a-f4d9p ///
		f4natwt )

	merge 1:1 idnum using "~/data/Fragile_Families/5_year_followup/inhome5yr2011.dta", nogen ///
		keepusing( ///
		c1a-c17c ///
		l1-l66 ///
		ppvtstd ppvtstd_m wjss22 tvipstd_m ///
		)

	merge 1:1 idnum using "~/data/Fragile_Families/5_year_followup/ff_kteachersurvey_fnlpub.dta", nogen ///
		keepusing( ///
		kind_a5-kind_a7 ///
		kind_b1a-kind_b1i ///
		)
}
*** GENERATE YEAR 5 RELATIONSHIP STATUS ***
{
	gen relstatus_yr5 = cm4relf if cm4relf > 0
	recode relstatus_yr5 ///
		(1 = 1) 	/// married
		(2 = 2) 	/// cohabitating
		(3/4 = 3) 	/// romantic, not cohabitating
		(6 = 4) 	/// friends
		(7/8 = 5) 	/// not in relationship, father unknown
		(5 = 6) 	/// separated/widowed/divorced

	replace R_3 = (relstatus_yr5 <= 3) if relstatus_yr5 < . & R_3 == . 

	replace returner = 1 if R_2 == 0 & relstatus_yr5 <= 3

	replace social_f = 1 if m4e2 == 1

	replace R_3 = 1 if R_3 == . & (f4a4 == 1 | f4a4 == 2)
	replace R_3 = 0 if R_3 == . & ((f4a4 >= 3 & f4a4 <= 6) | f4a4 == -14)

	replace R_4 = 0 if R_3 == 0
	replace R_5 = 0 if R_3 == 0

	* If separation occurred more than 2 years ago, update R_2 to 0

	replace R_2 = 0 if (m4intyr - m4a4c > 2)   & R_2 == . & m4intyr > 0 & m4a4c  > 0 // death
	replace R_2 = 0 if (m4intyr - m4a7a2 > 2)  & R_2 == . & m4intyr > 0 & m4a7a2 > 0 // divorce
	replace R_2 = 0 if (m4intyr - m4a8a2 > 2)  & R_2 == . & m4intyr > 0 & m4a8a2 > 0 // break-up

	* If separation occurred less than 2 years ago, update R_2 to 1

	replace R_2 = 1 if (m4intyr - m4a4c <= 2)  & R_2 == . & m4intyr > 0 & m4a4c  > 0 // death
	replace R_2 = 1 if (m4intyr - m4a7a2 <= 2) & R_2 == . & m4intyr > 0 & m4a7a2 > 0 // divorce
	replace R_2 = 1 if (m4intyr - m4a8a2 <= 2) & R_2 == . & m4intyr > 0 & m4a8a2 > 0 // break-up

	* Use father's report to fill in missings

	replace R_2 = 0 if (f4intyr - f4a4c > 2)   & R_2 == . & f4intyr > 0 & f4a4c  > 0
	replace R_2 = 0 if (f4intyr - f4a7a2 > 2)  & R_2 == . & f4intyr > 0 & f4a7a2 > 0
	replace R_2 = 0 if (f4intyr - f4a8a2 > 2)  & R_2 == . & f4intyr > 0 & f4a8a2 > 0

	replace R_2 = 1 if (f4intyr - f4a4c <= 2)  & R_2 == . & f4intyr > 0 & f4a4c  > 0
	replace R_2 = 1 if (f4intyr - f4a7a2 <= 2) & R_2 == . & f4intyr > 0 & f4a7a2 > 0
	replace R_2 = 1 if (f4intyr - f4a8a2 <= 2) & R_2 == . & f4intyr > 0 & f4a8a2 > 0

}
*** GENERATE YEAR 5 RELATIONSHIP QUALITY MEASUREMENTS *** 
{
	*** Five-category measurements ***

	local i_cat5 = 1

	/// In general, would you say that your relationship with him/her is excellent, very good, good, fair, or poor?

	gen M_R_3_cat5_`i_cat5' = 6 - m4d4 if m4d4 > 0 & m4d4 < .

	local i_cat5 = `i_cat5' + 1

	local vars /// 
		/// Would you say you strongly disagree (1), disagree (2), neither agree nor disagree (3), agree (4), or strongly agree (5)?
		a /// My relationship with (FATHER) is more important to me than almost anything else in my life
		c /// I like to think of (FATHER) and me more as a couple than as two separate people
		d /// I want this relationship to stay strong no matter what rough times we may encounter
		e /// I am happy with my sexual relationship with (FATHER)
		f /// I can trust that (FATHER) will not cheat on me with other people

	foreach var of local vars {

		gen M_R_3_cat5_`i_cat5' = .
		replace M_R_3_cat5_`i_cat5' = m4d6`var'  if m4d6`var' > 0 & m4d6`var' < .

		local i_cat5 = `i_cat5' + 1

	}

	local vars ///
		/// Would you say you strongly disagree (1), disagree (2), neither agree nor disagree (3), agree (4), or strongly agree (5)?
		b /// I may not want to be with (FATHER) a few years from now

	foreach var of local vars {

		gen M_R_3_cat5_`i_cat5' = .
		replace M_R_3_cat5_`i_cat5' = 6 - m4d6`var'  if m4d6`var' > 0 & m4d6`var' < .

		local i_cat5 = `i_cat5' + 1

	}

	*** Three-category measurements ***

	local i_cat3 = 1

	local vars ///
		g /// (HOW OFTEN HAVE) you thought your relationship with (FATHER) might be in trouble? 
		h /// You and (FATHER) discussed ending your relationship?
		i /// You talked to a close friend or relative about breaking up with (FATHER)?
		/// Coded as often (1), sometimes (2), and never (3)

	foreach var of local vars {

		gen M_R_3_cat3_`i_cat3' = .
		replace M_R_3_cat3_`i_cat3' = m4d6`var'  if m4d6`var' > 0 & m4d6`var' < .

		local i_cat3 = `i_cat3' + 1

	}

	local vars /// Now, think about how (FATHER/MOTHER) behaves towards you. For each statement I read, please tell me how often he behaves this way.
		a /// He/She was fair and willing to compromise when you had a disagreement?
		b /// He/She expressed affection or love for you?
		d /// He/She encouraged or helped you to do things that were important to you?
		o /// He/She listens to you when you need someone to talk to
		p /// He/She really understands your hurts and joys

	foreach var of local vars {

		gen M_R_3_cat3_`i_cat3' = .

		replace M_R_3_cat3_`i_cat3' = 4-m4d7`var'  if m4d7`var' > 0 & m4d7`var' < .
		replace M_R_3_cat3_`i_cat3' = 4-m4d9`var' if m4d9`var' > 0 & m4d9`var' < .	

		local i_cat3 = `i_cat3' + 1

	}	

	local vars /// Thinking about your past relationship with [BABY’S FATHER/MOTHER], how often would you say that
		c /// He/She insulted or criticized you or your ideas? 
		e /// He/She tries to keep you from seeing or talking with your friends or family 
		f /// He/She tries to prevent you from going to work or school
		g /// He/She withholds money, makes you ask for money, or takes your money
		k /// He withholds sex to try to control your behavior
		l /// He insults or criticizes you for not taking good enough care of the child or your home. 

	foreach var of local vars {

		gen M_R_3_cat3_`i_cat3' = .

		replace M_R_3_cat3_`i_cat3' = m4d7`var'  if m4d7`var' > 0 & m4d7`var' < .
		replace M_R_3_cat3_`i_cat3' = m4d9`var' if m4d9`var' > 0 & m4d9`var' < .	

		local i_cat3 = `i_cat3' + 1

	}

	egen theta_R_3_3  = rowtotal(M_R_3_cat3*)
	egen theta_R_3_3_ = rownonmiss(M_R_3_cat3*)
	replace theta_R_3_3 = (theta_R_3_3 - theta_R_3_3_)/2
	egen theta_R_3_5  = rowtotal(M_R_3_cat5*)
	egen theta_R_3_5_ = rownonmiss(M_R_3_cat5*)
	replace theta_R_3_5 = (theta_R_3_5 - theta_R_3_5_)/4
	gen theta_R_3 = 1/(theta_R_3_3_ + theta_R_3_5_) * (theta_R_3_3 + theta_R_3_5)
	replace theta_R_3 = 0 if R_2 == 0
	drop theta_R_3_*

}
*** GENERATE YEAR 5 NON-COGNITIVE MEASUREMENTS ***
{
	local vars 11 12 13 16 

	foreach var of numlist `vars'{
		replace m4b4b`var' = m4b29a`var' - 1 if m4b4b`var' < 0 
		// combine mother reports for those who live and do not live with child
	}

	local i_cat3 = 1

	local vars ///
		/// For each item I read, please tell me if this is not true (0), somewhat or sometimes true (1),
		/// very true or often true (2) for (CHILR)? 
		l1  /// (child) argues a lot 
		l2  /// (child) brags or boasts 
		l7  /// (child) is cruel, bullies and shows meanness to others
		m4b4b16 /// (child) wants a lot of attention?
		l9  /// (child) destroys (his/her) own things 
		l10 /// (child) destroys things belonging to family or others 
		l12 /// (child) is disobedient at home 
		l13 /// (child) is disobedient at school or in childcare 
		l16 /// (child) is easily jealous 
		l21 /// (child) gets in many fights 
		l33 /// (child) physically attacks people
		l40 /// (child) screams a lot
		l45 /// (child) shows off or clowns around 
		m4b4b11 /// (child) is stubborn, sullen or irritable?
		m4b4b12 /// (child) has sudden changes in mood or feelings?
		l56 /// (child) talks too much 
		l57 /// (child) teases a lot
		m4b4b13 /// (child) has temper tantrums or a hot temper?
		l59 /// (child) threatens people 
		l62 /// (child) is unusually loud 
		/// above items from CBCL 4-18 Aggressive Behavior scale (1991)

	local i_cat3 = 1

	foreach var of local vars {

		gen M_N_3_cat3_`i_cat3' = .

		replace M_N_3_cat3_`i_cat3' = 3 - `var' if `var' >= 0 & `var' < .

		local i_cat3 = `i_cat3' + 1

	}

	egen theta_N_3 = rowmean(M_N_3_cat3*)

}
*** GENERATE YEAR 5 COGNITIVE ABILITY MEASUREMENTS ***
{

	gen M_C_3_1 = ppvtstd if ppvtstd > 0
	replace M_C_3_1 = tvipstd if M_C_3_1 == . & tvipstd > 0

	gen M_C_3_2 = wjss22 if wjss22 > 0

	local vars ///
		kind_a5  /// How would you rate the child in language and literacy skills? 
		kind_a6  /// How would you rate the child in science and social studies? 
		kind_a7  /// How would you rate the child in mathematical skills?
		kind_b1a /// Understands and interprets a story or other text read to him/her
		kind_b1b /// Easily and quickly names all upper– and lower-case letters of the alphabet.
		kind_b1c /// Reads simple books independently
		kind_b1d /// Demonstrates an understanding of some of the conventions of print 
		kind_b1e /// Recognizes distinct differences in habits and living patterns between him/herself and other groups of people he/she knows 
		kind_b1f /// Forms explanations based on observations and explorations 
		kind_b1g /// Sorts, classifies, and compares math materials by various rules and attributes 
		kind_b1h /// Shows an understanding of the relationship between quantities 
		kind_b1i /// Uses a variety of strategies to solve math problems 

	local i_cat5 = 1

	foreach var of local vars {

		gen M_C_3_cat5_`i_cat5' = .

		replace M_C_3_cat5_`i_cat5' = `var' if `var' >= 0 & `var' <= 5

		local i_cat5 = `i_cat5' + 1

	}

	egen M_C_3_3 = rowmean(M_C_3_cat5*)

	drop M_C_3_cat5*

	* standardize and average measurements

	egen M_C_3_1_ = std(M_C_3_1)
	egen M_C_3_2_ = std(M_C_3_2)
	egen M_C_3_3_ = std(M_C_3_3)

	egen theta_C_3 = rowmean(M_C_3_1_ M_C_3_2_ M_C_3_3_)

	drop M_C_3_?_

}
*** KEEP YEAR 5 VARIABLES ***
{
	local vars3 `vars2' M_R_3_* M_N_3_* M_C_3_* theta_R_3 theta_N_3 theta_C_3
	keep `vars3'
}
*** MERGE YEAR 9 FOLLOW UP ***
{
	merge 1:1 idnum using "~/data/Fragile_Families/9_year_followup/ff_y9_pub1.dta", nogen ///
		keepusing( ///
		/// Child questions
		k5a2a-k5a2f k5a3a-k5a3f ///
		k5f1a-k5f1q k5g2a-k5g2n /// 
		/// Mother questions
		m5a2 m5a4 m5a4b2 m5a4h2 m5a4j2 ///
		m5b2 m5b2a ///
		m5c1 m5c5a-m5c5c m5c6a-m5c6p ///
		m5d2 m5d2h ///
		cm5edu cm5relf cm5hhinc cm5adult cm5kids cm5intmon cm5intyr cm5pcgrel /// constructed
		/// Father questions
		f5a4 f5a4b2 f5a4h2 f5a4j2 ///
		f5c1 f5c6a-f5c6p ///
		cf5edu cf5intyr ///
		/// Test results
		hv5_ppvtss hv5_wj9ss hv5_wj10ss hv5_dsss ///
		/// Primary care-giver questions
		pcg5idstat ///
		p5q3a-p5q3do /// 
		p5i1a-p5i1j p5i2a-p5i2e p5i30a p5i30c p5i31a-p5i31j p5i32a-p5i32c ///
		p5l3a-p5l3j ///
		/// Teacher questions
		t5b1a-t5b1ad ///
		t5b3a-t5b3l t5b4a-t5b4ab ///
		t5c1 t5c6 t5c7p t5c13a-t5c13c t5c16 ///
		)

	merge 1:1 idnum using "~/data/Fragile_Families/9_year_followup/ff_y9_pubweights082013STATA.dta", nogen ///
		keepusing(m5natwt f5natwt)
}
*** GENERATE YEAR 9 RELATIONSHIP STATUS ***
{
	gen relstatus_yr9 = cm5relf if cm5relf > 0
	recode relstatus_yr9 ///
		(1 = 1) 	/// married
		(2 = 2) 	/// cohabitating
		(3/4 = 3) 	/// romantic, not cohabitating
		(6 = 4) 	/// friends
		(7/8 = 5) 	/// not in relationship, father unknown
		(5 = 6) 	/// separated/widowed/divorced

	replace R_4 = (relstatus_yr9 <= 3) if relstatus_yr9 < . & R_4 == . 

	replace returner = 1 if R_3 == 0 & relstatus_yr9 <= 3

	replace social_f = 1 if m5d2 == 1

	replace R_4 = 1 if R_3 == . & (f5a4 == 1 | f5a4 == 4 | f5a4 == 5)
	replace R_4 = 0 if R_3 == . & (f5a4 == 2 | f5a4 == 3 | f5a4 == 6 | f5a4 == 7 | f5a4 == -14)

	replace R_5 = 0 if R_4 == 0

	replace R_2 = 0 if (cm5intyr - m5a4b2 > 6)  & R_2 == . & cm5intyr > 0 & m5a4b2 > 0
	replace R_2 = 0 if (cm5intyr - m5a4h2 > 6)  & R_2 == . & cm5intyr > 0 & m5a4h2 > 0
	replace R_2 = 0 if (cm5intyr - m5a4j2 > 6)  & R_2 == . & cm5intyr > 0 & m5a4j2 > 0

	replace R_2 = 1 if (cm5intyr - m5a4b2 <= 6) & R_2 == . & cm5intyr > 0 & m5a4b2 > 0
	replace R_2 = 1 if (cm5intyr - m5a4h2 <= 6) & R_2 == . & cm5intyr > 0 & m5a4h2 > 0
	replace R_2 = 1 if (cm5intyr - m5a4j2 <= 6) & R_2 == . & cm5intyr > 0 & m5a4j2 > 0

	replace R_3 = 0 if R_2 == 0

	replace R_3 = 0 if (cm5intyr - m5a4b2 > 4)  & R_3 == . & cm5intyr > 0 & m5a4b2 > 0
	replace R_3 = 0 if (cm5intyr - m5a4h2 > 4)  & R_3 == . & cm5intyr > 0 & m5a4h2 > 0
	replace R_3 = 0 if (cm5intyr - m5a4j2 > 4)  & R_3 == . & cm5intyr > 0 & m5a4j2 > 0

	replace R_3 = 1 if (cm5intyr - m5a4b2 <= 4) & R_3 == . & cm5intyr > 0 & m5a4b2 > 0
	replace R_3 = 1 if (cm5intyr - m5a4h2 <= 4) & R_3 == . & cm5intyr > 0 & m5a4h2 > 0
	replace R_3 = 1 if (cm5intyr - m5a4j2 <= 4) & R_3 == . & cm5intyr > 0 & m5a4j2 > 0

	replace R_2 = 0 if (cf5intyr - f5a4b2 > 6)  & R_2 == . & cf5intyr > 0 & f5a4b2 > 0
	replace R_2 = 0 if (cf5intyr - f5a4h2 > 6)  & R_2 == . & cf5intyr > 0 & f5a4h2 > 0
	replace R_2 = 0 if (cf5intyr - f5a4j2 > 6)  & R_2 == . & cf5intyr > 0 & f5a4j2 > 0

	replace R_2 = 1 if (cf5intyr - f5a4b2 <= 6) & R_2 == . & cf5intyr > 0 & f5a4b2 > 0
	replace R_2 = 1 if (cf5intyr - f5a4h2 <= 6) & R_2 == . & cf5intyr > 0 & f5a4h2 > 0
	replace R_2 = 1 if (cf5intyr - f5a4j2 <= 6) & R_2 == . & cf5intyr > 0 & f5a4j2 > 0

	replace R_3 = 0 if R_2 == 0

	replace R_3 = 0 if (cf5intyr - f5a4b2 > 4)  & R_3 == . & cf5intyr > 0 & f5a4b2 > 0
	replace R_3 = 0 if (cf5intyr - f5a4h2 > 4)  & R_3 == . & cf5intyr > 0 & f5a4h2 > 0
	replace R_3 = 0 if (cf5intyr - f5a4j2 > 4)  & R_3 == . & cf5intyr > 0 & f5a4j2 > 0

	replace R_3 = 1 if (cf5intyr - f5a4b2 <= 4) & R_3 == . & cf5intyr > 0 & f5a4b2 > 0
	replace R_3 = 1 if (cf5intyr - f5a4h2 <= 4) & R_3 == . & cf5intyr > 0 & f5a4h2 > 0
	replace R_3 = 1 if (cf5intyr - f5a4j2 <= 4) & R_3 == . & cf5intyr > 0 & f5a4j2 > 0

}
*** GENERATE YEAR 9 RELATIONSHIP QUALITY MEASUREMENTS ***
{
	*** Five-category measurements ***

	local i_cat5 = 1

	/// In general, would you say that your relationship with him/her is excellent, very good, good, fair, or poor?

	gen M_R_4_cat5_`i_cat5' = 6 - m5c1 if m5c1 > 0 & m5c1 < .

	local i_cat5 = `i_cat5' + 1

	*** Three-category measurements ***

	local i_cat3 = 1

	local vars ///
		a /// Thought your relationship with {FATHER} might be in trouble? 
		b /// You and {FATHER} discussed ending your relationship?
		c /// Talked to a close friend or relative about breaking up with {FATHER}?

	foreach var of local vars {

		gen M_R_4_cat3_`i_cat3' = .

		replace M_R_4_cat3_`i_cat3' = m5c5`var'  if m5c5`var' > 0 & m5c5`var' < .

		local i_cat3 = `i_cat3' + 1

	}

	local vars ///
		a /// He/She was fair and willing to compromise when you had a disagreement?
		b /// He/She expressed affection or love for you?
		d /// He/She encouraged or helped you to do things that were important to you?
		o /// He/She listens to you when you need someone to talk to
		p /// He/She really understands your hurts and joys

	foreach var of local vars {

		gen M_R_4_cat3_`i_cat3' = .

		replace M_R_4_cat3_`i_cat3' = 4-m5c6`var'  if m5c6`var' > 0 & m5c6`var' < .

		local i_cat3 = `i_cat3' + 1

	}	

	local vars /// Thinking about your past relationship with [BABY’S FATHER/MOTHER], how often would you say that
		c /// He/She insulted or criticized you or your ideas? 
		e /// He/She tries to keep you from seeing or talking with your friends or family 
		f /// He/She tries to prevent you from going to work or school
		g /// He/She withholds money, makes you ask for money, or takes your money
		k /// He withholds sex to try to control your behavior
		l /// He insults or criticizes you for not taking good enough care of the child or your home. 

	foreach var of local vars {

		gen M_R_4_cat3_`i_cat3' = .

		replace M_R_4_cat3_`i_cat3' = m5c6`var'  if m5c6`var' > 0 & m5c6`var' < .

		local i_cat3 = `i_cat3' + 1

	}

	egen theta_R_4_3  = rowtotal(M_R_4_cat3*)
	egen theta_R_4_3_ = rownonmiss(M_R_4_cat3*)
	replace theta_R_4_3 = (theta_R_4_3 - theta_R_4_3_)/2
	egen theta_R_4_5  = rowtotal(M_R_4_cat5*)
	egen theta_R_4_5_ = rownonmiss(M_R_4_cat5*)
	replace theta_R_4_5 = (theta_R_4_5 - theta_R_4_5_)/4
	gen theta_R_4 = 1/(theta_R_4_3_ + theta_R_4_5_) * (theta_R_4_3 + theta_R_4_5)
	replace theta_R_4 = 0 if R_3 == 0
	drop theta_R_4_*

	egen theta_R_4_c = rowmean(M_R_4_cat3_1-M_R_4_cat3_3 M_R_4_cat3_6) 
}
*** GENERATE YEAR 9 NON-COGNITIVE MEASUREMENTS ***
{

	local vars ///
		/// For each item, please report whether this is Not True (so far as you know) (1), Somewhat or Sometimes
		/// True (2), OR Very True or Often True for (CHILR) (3)? 
		p5q3c  /// Child argues a lot
		p5q3o  /// Child is cruel, bullies, or shows meanness to others
		p5q3r  /// Child demands a lot of attention
		p5q3s  /// Child destroys his or her own things
		p5q3t  /// Child destroys things belonging to family or others
		p5q3u  /// Child is disobedient at home
		p5q3v  /// Child is disobedient at school
		p5q3aj /// Child gets in many fights
		p5q3bc /// Child physically attacks people
		p5q3bn /// Child screams a lot
		p5q3cf /// Child is stubborn, sullen, or irritable
		p5q3cg /// Child has sudden changes in mood or feelings
		p5q3ch /// Child sulks a lot
		p5q3ci /// Child is suspicious
		p5q3cn /// Child teases a lot
		p5q3co /// Child has temper tantrums or a hot temper
		p5q3cq /// Child threatens people
		p5q3cw /// Child is unusually loud
		// CBCL 6-18 Aggressive Behavior (2001)

	local i_cat3 = 1

	foreach var of varlist `vars' {

		gen M_N_4_cat3_`i_cat3' = .

		replace M_N_4_cat3_`i_cat3' = 4 - `var' if `var' >= 0 & `var' < .

		local i_cat3 = `i_cat3' + 1
	}

	*local i_cat2 = 1

	*gen damage_yr9 = (k5f1a == 1) if k5f1a <= 2 & k5f1a >= 0
	*gen shoplift_yr9 = (k5f1b == 1) if k5f1b <= 2 & k5f1b >= 0
	*gen stole_yr9 = (k5f1c == 1) if k5f1c <= 2 & k5f1c >= 0
	*gen cheat_yr9 = (k5f1d == 1) if k5f1d <= 2 & k5f1d >= 0
	*gen fight_yr9 = (k5f1e == 1) if k5f1e <= 2 & k5f1e >= 0
	*gen hurtani_yr9 = (k5f1f == 1) if k5f1f <= 2 & k5f1f >= 0
	*gen trespass_yr9 = (k5f1g == 1) if k5f1g <= 2 & k5f1g >= 0
	*gen runaway_yr9 = (k5f1h == 1) if k5f1h <= 2 & k5f1h >= 0
	*gen skipschool_yr9 = (k5f1i == 1) if k5f1i <= 2 & k5f1i >= 0
	*gen drank_yr9 = (k5f1j == 1) if k5f1j <= 2 & k5f1j >= 0
	*gen pot_yr9 = (k5f1k == 1) if k5f1k <= 2 & k5f1k >= 0
	*gen smoke_yr9 = (k5f1l == 1) if k5f1l <= 2 & k5f1l >= 0
	*gen suspend_yr9 = (k5f1m == 1) if k5f1m <= 2 & k5f1m >= 0
	*gen vandal_yr9 = (k5f1n == 1) if k5f1n <= 2 & k5f1n >= 0
	*gen arson_yr9 = (k5f1o == 1) if k5f1o <= 2 & k5f1o >= 0
	*gen nopay_yr9 = (k5f1p == 1) if k5f1p <= 2 & k5f1p >= 0
	*gen rocks_yr9 = (k5f1q == 1) if k5f1q <= 2 & k5f1q >= 0

	*gen specialed_yr9 = (t5c1 == 1) if t5c1 <= 2 & t5c1 >= 0

	*gen workhard_yr9 = (t5c16 >= 3) if t5c16 < . & t5c16 >= 0

	*gen bip_yr9 = (t5c7p == 1) if t5c7p >= 0 & t5c7p < .
	*gen ADHD_yr9 = (t5c6 == 1) if t5c6 >= 0 & t5c6 < .

	*foreach var of varlist damage_yr9-ADHD_yr9{

	*	gen M_N_4_cat2_`i_cat2' = .

	*	replace M_N_4_cat2_`i_cat2' = 1 -`var' if `var' >= 0 & `var' < .

	*	local i_cat2 = `i_cat2' + 1

	*}

	*egen theta_N_4_3  = rowtotal(M_N_4_cat3*)
	*egen theta_N_4_3_ = rownonmiss(M_N_4_cat3*)
	*replace theta_N_4_3 = (theta_N_4_3 - theta_N_4_3_)/2
	*egen theta_N_4_2  = rowtotal(M_N_4_cat2*)
	*egen theta_N_4_2_ = rownonmiss(M_N_4_cat2*)
	*gen theta_N_4_ = 1/(theta_N_4_3_ + theta_N_4_2_) * (theta_N_4_3 + theta_N_4_2)

	egen theta_N_4_  = rowmean(M_N_4_cat3*)
	egen theta_N_4 = std(theta_N_4_)
	drop theta_N_4_*

}
*** GENERATE YEAR 9 COGNITIVE MEASUREMENTS ***
{

	gen M_C_4_1 = hv5_ppvtss if hv5_ppvtss > 0
	gen M_C_4_2 = hv5_wj9ss if hv5_wj9ss > 0
	gen M_C_4_3 = hv5_wj10ss if hv5_wj10ss > 0
	gen M_C_4_4 = hv5_dsss if hv5_dsss > 0

	* standardize and average measurements

	egen M_C_4_1_ = std(M_C_4_1)
	egen M_C_4_2_ = std(M_C_4_2)
	egen M_C_4_3_ = std(M_C_4_3)
	egen M_C_4_4_ = std(M_C_4_4)

	egen theta_C_4 = rowmean(M_C_4_?_)

	drop M_C_4_?_ 

}
*** GENERATE YEAR 9 OUTCOMES ***
{
	local vars ///
		t5c13a /// language and literacy skills
		t5c13b /// science and social studies
		t5c13c /// mathematics skills
	
	local i_cat5 = 1

	foreach var of varlist `vars' {
		gen outcome_yr9_cat5_`i_cat5' = .
		replace outcome_yr9_cat5_`i_cat5' = `var' if `var' >= 0 & `var' < .
		local i_cat5 = `i_cat5' + 1
	}
	
	local vars ///
		t5c1   /// Is this child currently receiving special education services through an Individualized Education Program (IEP)?
		t5c6   /// Is this child receiving any special education or related services because of Attention Deficit/Hyperactivity Disorder (ADD/ADHD)
		t5c7p  /// Behavior Intervention Plan (BIP), in or out of the classroom
		k5f1a  /// Purposely damaged or destroyed property that wasn’t yours?
		k5f1b  /// Taken or stolen something that didn’t belong to you from another person or from a store?
		k5f1c  /// Taken some money at home that did not belong to you, like from your mothers’ purse or from your parents’ dresser?
		k5f1d  /// Cheated on a school test?
		k5f1e  /// Had a fist fight with another person? 
		k5f1f  /// Hurt an animal on purpose? 
		k5f1g  /// Gone into somebody's garden, backyard, house or garage when you were not supposed to be there?
		k5f1h  /// Run away from home? 
		k5f1i  /// Skipped school without an excuse?
		k5f1j  /// Secretly taken a sip of wine, beer, or liquor?
		/// k5f1k  /// Smoked marijuana, grass, pot, weed?
		/// k5f1l  /// Smoked a cigarette or used tobacco?
		k5f1m  /// Been suspended or expelled from school? 
		k5f1n  /// Written things or sprayed paint on walls or sidewalks or cars? 
		k5f1o  /// Purposely set fire to a building, a car, or other property or tried to do so?
		k5f1p  /// Avoided paying for things such as movies, bus or subway rides, or food?
		k5f1q  /// Thrown rocks or bottles at people or cars? 

	local i_cat2 = 1

	foreach var of varlist `vars'{
		gen outcome_yr9_cat2_`i_cat2' = .
		replace outcome_yr9_cat2_`i_cat2' = (`var' == 1) if `var' >= 0 & `var' < .
		local i_cat2 = `i_cat2' + 1
	}
}
*** KEEP YEAR 9 VARIABLES ***
{
	local vars4 `vars3' M_R_4_* M_N_4_* M_C_4_* theta_R_4 theta_N_4 theta_C_4 outcome_yr9* 
	keep `vars4'
}
*** MERGE YEAR 15 FOLLOW UP ***
{
	merge 1:1 idnum using "~/data/Fragile_Families/15_year_followup/FF_Y15_pub.dta", nogen ///
		keepusing( ///
		/// Primary caregiver survey
			/// Constructed variables
			cp6mrelf cp6intyr ///
			/// Section B: Youth health and behavior
			p6b70 ///
			/// Section C: Youth education
			p6c16 p6c17 p6c19 p6c20 p6c21 p6c25 ///
			/// Section E: Household structure and family relationships
			p6e2b p6e6b p6e7b p6e8 ///
		/// Teen survey
			/// Introduction
			k6z2 ///
			/// Section B: Education
			k6b18 k6b20? k6b27 k6b29 ///
			/// Section D: Health and health behavior
			k6d40 k6d48 k6d61? ///
			/// Section E: Neighborhood
			k6e36 ///
			/// Section F: Risky behaviors -- sexual activity and illegal drug use
			k6f26 k6f63 ///
		)
}
*** GENERATE YEAR 15 RELATIONSHIP STATUS ***
{
	gen relstatus_yr15 = cp6mrelf if cp6mrelf > 0

	// Recode for consistency with other waves
	recode relstatus_yr15 ///
		(1 = 1) 	/// married
		(2 = 2) 	/// cohabitating
		(3/4 = 3) 	/// romantic, not cohabitating
		(6 = 4) 	/// friends
		(7/8 = 5) 	/// not in relationship, father unknown
		(5 = 6) 	/// separated/widowed/divorced

	// Indicator for whether parents are still in a relationship
	replace R_5 = (relstatus_yr15 <= 3) if relstatus_yr15 < . & R_5 == . 
	// Use teen report if PCG's report unavailable
	replace R_5 = (k6z2 == 1) if R_5 == . & k6z2 != -9

	// Update whether parents got back together 
	replace returner = 1 if R_4 == 0 & relstatus_yr15 <= 3

	// Update whether PCG has another partner
	replace social_f = 1 if p6e8 == 1

	// Update relationship status in year 3 if missing and parent died before year 15
	replace R_2 = 0 if (cp6intyr - p6e2b >  12)  & R_2 == . & cp6intyr > 0 & p6e2b > 0
	replace R_2 = 1 if (cp6intyr - p6e2b <= 12)  & R_2 == . & cp6intyr > 0 & p6e2b > 0
	// Update relationship status in year 3 if missing and parents divorced/separated before year 15
	replace R_2 = 0 if (cp6intyr - p6e6b >  12)  & R_2 == . & cp6intyr > 0 & p6e6b > 0
	replace R_2 = 1 if (cp6intyr - p6e6b <= 12)  & R_2 == . & cp6intyr > 0 & p6e6b > 0
	// Update relationship status in year 3 if missing and parents broke up before year 15
	replace R_2 = 0 if (cp6intyr - p6e7b >  12)  & R_2 == . & cp6intyr > 0 & p6e7b > 0
	replace R_2 = 1 if (cp6intyr - p6e7b <= 12)  & R_2 == . & cp6intyr > 0 & p6e7b > 0
	// Update relationship status in year 5 and year 9 if not in a relationship at year 3
	replace R_3 = 0 if R_2 == 0
	replace R_4 = 0 if R_3 == 0

	// Update relationship status in year 5 if missing and parent died before year 15
	replace R_3 = 0 if (cp6intyr - p6e2b >  10)  & R_3 == . & cp6intyr > 0 & p6e2b > 0
	replace R_3 = 1 if (cp6intyr - p6e2b <= 10)  & R_3 == . & cp6intyr > 0 & p6e2b > 0
	// Update relationship status in year 5 if missing and parents divorced/separated before year 15
	replace R_3 = 0 if (cp6intyr - p6e6b >  10)  & R_3 == . & cp6intyr > 0 & p6e6b > 0
	replace R_3 = 1 if (cp6intyr - p6e6b <= 10)  & R_3 == . & cp6intyr > 0 & p6e6b > 0
	// Update relationship status in year 5 if missing and parents broke up before year 15
	replace R_3 = 0 if (cp6intyr - p6e7b >  10)  & R_3 == . & cp6intyr > 0 & p6e7b > 0
	replace R_3 = 1 if (cp6intyr - p6e7b <= 10)  & R_3 == . & cp6intyr > 0 & p6e7b > 0
	// Update relationship status in year 9 if not in a relationship at year 5
	replace R_4 = 0 if R_3 == 0

	// Update relationship status in year 9 if missing and parent died before year 15
	replace R_4 = 0 if (cp6intyr - p6e2b >  6)  & R_4 == . & cp6intyr > 0 & p6e2b > 0
	replace R_4 = 1 if (cp6intyr - p6e2b <= 6)  & R_4 == . & cp6intyr > 0 & p6e2b > 0
	// Update relationship status in year 5 if missing and parents divorced/separated before year 15
	replace R_4 = 0 if (cp6intyr - p6e6b >  6)  & R_4 == . & cp6intyr > 0 & p6e6b > 0
	replace R_4 = 1 if (cp6intyr - p6e6b <= 6)  & R_4 == . & cp6intyr > 0 & p6e6b > 0
	// Update relationship status in year 5 if missing and parents broke up before year 15
	replace R_4 = 0 if (cp6intyr - p6e7b >  6)  & R_4 == . & cp6intyr > 0 & p6e7b > 0
	replace R_4 = 1 if (cp6intyr - p6e7b <= 6)  & R_4 == . & cp6intyr > 0 & p6e7b > 0
}
*** GENERATE YEAR  15 OUTCOMES ***
{
	* Four category outcomes
	local vars ///
		k6b20a  /// English or language arts? 
		k6b20b  /// Math? 
		k6b20c  /// History or social studies?
		k6b20d  /// Science? 
		
	local i_cat4 = 1

	foreach var of varlist `vars' {
		gen outcome_yr15_cat4_`i_cat4' = .
		replace outcome_yr15_cat4_`i_cat4' = 5 - `var' if `var' >= 0 & `var' < 5
		local i_cat4 = `i_cat4' + 1
	}
	
	* Binary outcomes
	
	// recode deliquent behavior as binary (rare to be higher than 1, binaries easier to interpret)
	local vars ///
		k6d61a	/// Paint graffiti or signs on someone else’s property or in a public place?
		k6d61b	/// Deliberately damage property that didn’t belong to you? 
		k6d61c	/// Take something from a store without paying for it?
		k6d61d 	/// Get into a serious physical fight?
		k6d61e	/// Hurt someone badly enough to need bandages or care from a doctor or nurse?
		k6d61f	/// Drive a car without its owner’s permission?
		k6d61g	/// Steal something worth more than $50?
		k6d61h 	/// Go into a house or building to steal something?
		k6d61i	/// Use or threaten to use a weapon to get something from someone?
		k6d61j	/// Sell marijuana or other drugs?
		k6d61k 	/// Steal something worth less than $50?
		k6d61l	/// Take part in a fight where a group of your friends was against another group?
		k6d61m	/// Were you loud, rowdy, or unruly in a public place? 
		
	foreach var of varlist `vars' {
		recode `var' (1 = 0) (2/4 = 1)
	}
	
	// recode sex variable to account for skip pattern
	recode k6f26 (-6 = 1)
	
	local vars ///
		k6d61a	/// Paint graffiti or signs on someone else’s property or in a public place?
		k6d61b	/// Deliberately damage property that didn’t belong to you? 
		k6d61c	/// Take something from a store without paying for it?
		k6d61d 	/// Get into a serious physical fight?
		k6d61e	/// Hurt someone badly enough to need bandages or care from a doctor or nurse?
		k6d61f	/// Drive a car without its owner’s permission?
		k6d61g	/// Steal something worth more than $50?
		k6d61h 	/// Go into a house or building to steal something?
		k6d61i	/// Use or threaten to use a weapon to get something from someone?
		k6d61j	/// Sell marijuana or other drugs?
		k6d61k 	/// Steal something worth less than $50?
		k6d61l	/// Take part in a fight where a group of your friends was against another group?
		k6d61m	/// Were you loud, rowdy, or unruly in a public place? 
		p6b70 	/// Has {YOUTH} ever been arrested? 
		p6c16 	/// Remedial math
		p6c17 	/// Remedial English
		p6c19 	/// Gifted and talented program
		p6c20 	/// Special education or related services
		p6c21 	/// Has {YOUTH} ever been suspended or expelled? 
		p6c25 	/// Since {MONTH AND YEAR COHORT CITY FIELDED IN YR 9} has {YOUTH} repeated any grades?
		k6b18 	/// Have you ever taken any honors courses in school? 
		k6b27 	/// Did you ever skip school for a full day without an excuse?
		k6b29 	/// Have you been suspended or expelled from school in the past two years? 
		k6d40 	/// Ever smoked an entire cigarette?
		k6d48 	/// Ever drank alcohol more than two times without parents?
		k6e36 	/// Have you ever been arrested or taken into custody by the police? 
		k6f26 	/// Have you ever had sexual intercourse with anyone, that is, made love, had sex, or gone all the way? 
		k6f63 	/// Ever tried marijuana?
		
	local i_cat2 = 1

	foreach var of varlist `vars'{
		gen outcome_yr15_cat2_`i_cat2' = .
		replace outcome_yr15_cat2_`i_cat2' = (`var' == 1) if `var' >= 0 & `var' < .
		local i_cat2 = `i_cat2' + 1
		}
}
*** KEEP YEAR 15 VARIABLES ***
{
	local vars5 `vars4' outcome_yr15_*
}
*** KEEP VARIABLES FOR ANALYSIS ***
{
* M or F has children with other father

local covariates ///
      educ_cat_m ///
      race_m ///
      birthage_m ///
      educ_cat_f ///
      race_f ///
      rellength ///      
      siblings ///
      half_siblings ///
      female ///
      religious_m

misstable sum `covariates'

keep `covariates'  R_? theta_?_* M_?_?_* outcome* returner social_f
}
*** KEEP OBSERVATION FOR ANALYSIS ***
{
	egen miss = rowmiss(`covariates')
	gen in_sample = (miss == 0)
	keep if in_sample == 1
	drop miss in_sample
}
*** CONSTRUCT VARIABLES ***
{
* if parents are together before and after a missed interview, assume together in the missed period

replace R_0 = 1 if R_0 == . & (R_1 == 1 | R_2 == 1 | R_3 == 1 | R_4 == 1 | R_5 == 1)
replace R_1 = 1 if R_1 == . & (R_2 == 1 | R_3 == 1 | R_4 == 1 | R_5 == 1)
replace R_2 = 1 if R_2 == . & (R_3 == 1 | R_4 == 1 | R_5 == 1)
replace R_3 = 1 if R_3 == . & (R_4 == 1 | R_5 == 1)
replace R_4 = 1 if R_4 == . & (R_5 == 1)

* assume parents are together until first reported separation

replace R_0 = 1 if R_0 == . & (R_1 != . | R_2 != . | R_3 != . | R_4 != . | R_5 != .)
replace R_1 = 1 if R_1 == . & (R_2 != . | R_3 != . | R_4 != . | R_5 != .)
replace R_2 = 1 if R_2 == . & (R_3 != . | R_4 != . | R_5 != .)
replace R_3 = 1 if R_3 == . & (R_4 != . | R_5 != .)
replace R_4 = 1 if R_4 == . & (R_5 != .)

* standardize latent variables

egen theta_R_0_ = std(theta_R_0)
drop theta_R_0
rename  theta_R_0_ theta_R_0

forvalues i = 0/4{
	foreach var of varlist M_R_`i'*{
		replace `var' = . if R_`i' == 0
	}
	replace theta_R_`i' = . if R_`i' == . | R_`i' == 0
	egen theta_R_`i'_ = std(theta_R_`i')
	drop theta_R_`i'
	rename theta_R_`i'_ theta_R_`i'
	replace theta_R_`i' = 0 if R_`i' == 0 
}

forvalues i = 1/4{
	replace theta_N_`i' = . if R_`i' == .
	egen theta_N_`i'_ = std(theta_N_`i')
	drop theta_N_`i'
	rename theta_N_`i'_ theta_N_`i'
}

forvalues i = 2/4{
	replace theta_C_`i' = . if R_`i' == .
	egen theta_C_`i'_ = std(theta_C_`i')
	drop theta_C_`i'
	rename theta_C_`i'_ theta_C_`i'
}


* Code as missing if relationship status unknown
foreach var of varlist M_R_3_* M_N_3_* M_C_3_*{
	replace `var' = . if R_3 == .
}

foreach var of varlist M_R_4_* M_N_4_* M_C_4_* outcome_*{
	replace `var' = . if R_4 == .
}

}
saveold "~/data/Fragile_Families/extract/extract_noretro.dta", version(12) replace

