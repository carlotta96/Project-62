data bw;                                                                                                                              
infile 'C:\Users\cdp6\Desktop\my.data.txt';                                                                        
input id date gestation marital ed ded parity race age drace dage wt inc time number BMI dBMI;                                                         
run;                                                                                                                    
proc print data=bw;                                                                                                                   
run;
*I have uploaded the "clean" data; 

title "Model";
proc reg data=bw;
model wt = date gestation marital ed ded parity race age drace dage inc time number BMI dBMI/clm clb selection=stepwise collin vif;
run; 
quit;
*note:
clm: computes 100(1-alpha)% confidence limits for the expected value of the dependent variable 
clb: computes 100(1-alpha)% confidence limits for the parameter estimates 
collin: produces collinearity analysis 
vif: computes variance-inflation factors; 

*At step 6 we will have our final model with the following variables:
date (we can delete it)
gestation
parity
race
time
number;

