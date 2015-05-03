# MSDFinalProject
Modeling Social Data - Final Project

Steps taken to clean data so far:
1) Look only at top 6 countries of interest: Iraq, Pakistan, Afghanistan, India, Philippines and United States - These make up ~65% of entire dataset

2) Remove columns where there are more than half the rows with NA

3) Remove columns with "_txt" since these are coded numerically

4) Remove other columns with detailed location information or characters:
"eventid", "provstate", "city","latitude","longitude","specificity",
"location","summary","targsubtype1","motive","weapdetail","propcomment","scite1","scite2","dbsource"

5) Of the columns that include international information, remove those where more than half the rows are missing or incomplete:
"INT_LOG","INT_IDEO","INT_ANY"

6) Assign numeric values to gname (Perpetrator Group Name), corp1 (corporate entity/government agency that was targeted) and target1
(Specific person, building, installation, targeted) and remove the columns with corresponding character information.

7) Rearrange columns so that the label "Success" is at far right

Perform subset selection using regsubsets from leaps package:
1) using 20 features:

Looking at the features selection based on highest Adjusted R squared:

 coef(reg.model, max.adjR)

  (Intercept)         iyear        imonth      extended       country        region     doubtterr      multiple
 1.453921e+01 -6.797289e-03 -1.275971e-03 -4.361199e-02 -2.124055e-04  1.353064e-02  3.851092e-02  1.458877e-02
      suicide   attacktype1       claimed     weaptype1  weapsubtype1         nkill       nkillus        nwound
-5.733472e-02  5.950978e-02  1.409924e-02 -2.568232e-02 -3.589514e-03  5.944115e-03  2.243717e-02 -3.727316e-04
     property     ishostkid    corp.index   gname.index target1.index
-3.172962e-03 -3.209989e-02 -8.390188e-06  1.552925e-04 -2.156886e-06

AIC returns the same features, however BIC returns these:

 (Intercept)         iyear       country        region     doubtterr       suicide   attacktype1       claimed
 1.329242e+01 -6.182874e-03 -2.139940e-04  1.319593e-02  3.966771e-02 -5.740017e-02  5.792416e-02  1.467604e-02
    weaptype1  weapsubtype1         nkill      property     ishostkid    corp.index   gname.index target1.index
-2.376420e-02 -3.555021e-03  5.294363e-03 -3.172898e-03 -4.586415e-02 -8.741322e-06  1.649393e-04 -2.230543e-06

Looks like imonth, extended, multiple, nkillus, nwound are all not returned.

[![bic_subsets](http://nandinishah.github.io/MSDFinalProject/bic_subsets.png)](http://nandinishah.github.io/MSDFinalProject/bic_subsets.png)
[![cp_subsets](http://nandinishah.github.io/MSDFinalProject/cp_subsets.png)](http://nandinishah.github.io/MSDFinalProject/cp_subsets.png)
[![adjr2_subsets](http://nandinishah.github.io/MSDFinalProject/adjr2_subsets.png)](http://nandinishah.github.io/MSDFinalProject/adjr2_subsets.png)




