EEMB 174/274: Bayesian Analysis of Biological Data
========================================================
author: Stephen Proulx
date: Jan 3, 2022
autosize: true

Remote learning and covid
========================================================



- We're all doing our best given each of our unique circumstances
- Synchronous class meetings are for working on examples and asking questions
- All zoom sessions recorded for asynchronous use
- Let us know how we can help you succeed this term

How will you access the course?
========================================================

- [Gauchospace](https://gauchospace.ucsb.edu/)- announcements and quizzes
- Gradescope- Turning in assignments (turn in both Rmarkdown and pdf)
- Gauchocast- viewing videos after class
- [RStudio VM](http://bit.ly/3hyqx4Y)- assignments and demostrations delivered here via github


Grading
========================================================
Category    | Percentage     
------------- | ------------- 
Homework  |  30%  
Lab Reports | 45%   
Quizzes  | 15%  
Midterm | 5%
Final | 5%  

Textbook and other readings
=========================================================
Required textbook: [Statistical Rethinking](https://xcelab.net/rm/statistical-rethinking/)-- [Library e-copy](https://ucsb-primo.hosted.exlibrisgroup.com/permalink/f/1egv95m/01UCSB_ALMA51345854830003776). 

Optional but interesting:  
-[Theory and Reality, by Peter Godfrey-Smith](https://read.amazon.com/kp/embed?asin=B003URRG0W&preview=newtab&linkCode=kpe&ref_=cm_sw_r_kb_dp_TYQ8FbR4EH3XQ)  
-[Ecological Models and data in R, Ben Bolker](https://read.amazon.com/kp/embed?asin=B005KLQY6I&preview=newtab&linkCode=kpe&ref_=cm_sw_r_kb_dp_Z3Q8FbB0QWV3F)  
-[Bayesian Data Analysis, Gelman et al. ] (https://www.amazon.com/dp/1439840954/ref=cm_sw_em_r_mt_dp_81Q8Fb8W0HMZS)  
-[R for Data Science, Wickham and Grolemund](https://read.amazon.com/kp/embed?asin=B01NAJAEN5&preview=newtab&linkCode=kpe&ref_=cm_sw_r_kb_dp_S4Q8FbHDD70YR)  
-[Statistical Rethinking with brms and tidy, Kurz](https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/) 





Software
=================
*    R  
*    [RStudio](https://rstudio.com/products/rstudio/)  
*    [RStan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)  
*    Tidyverse package for R  
*    [Rethinking package for R](https://github.com/rmcelreath/rethinking)


Alternative Software
=========
- [rstanarm](https://mc-stan.org/rstanarm/) Has pre-compiled Stan programs, uses glm syntax
- [brms](https://mc-stan.org/users/interfaces/brms) Creates stan code using lme4 like syntax
- [bayesplot](http://mc-stan.org/bayesplot/reference/bayesplot-package.html) Tools for plotting posterior samples
- [PyStan](https://pystan.readthedocs.io/en/latest/api.html) Python interface for Stan
- [Shiny](https://shiny.rstudio.com/) Make apps to perform analysis with user input



How can you teach programming?
=========================================================
- Is it like opening a can of beans? Or is it more like giving someone a recipe book?
- We aim to get somewhere in between: 
  + Here's the can opener and some abstract guidance
  + Watch me do everything and try not to fall asleep


Breakout: Two Truths and a Lie, biology style
========================================================
- Go to your breakout rooms and meet your peers
- Come up with two true biology/ecology/evolution facts, use any sources you like
- Come up with one untrue biology statement (fact check that it is indeed false as best you can)
- Choose a spokesperson to convincingly present your facts 
- We will poll the class to see if they can detect the lie 

Breakout: Experience with chance
========================================================
- Discuss with group 3 experiences in the last month where an outcome could be considered chance.
- Discuss how you could convince a space alien that such an event was in some sense random.
- Pick one of these to report back on and write two sentences to paste into the chat.

Breakout: Load VM
========================================================
Use this link to start your VM: [http://bit.ly/3hyqx4Y](http://bit.ly/3hyqx4Y).  
Add a directory to store your work. You will want to use the "export" function to save copies of your work on your own computer.

Navigate to the "In Class Material" folder and find today's date. Clic on the file "app.R". Once it is loaded into the code tab you can run this shiny app by pressing the "Run App" button.


Baye's Rule 
======================
$$
Pr(A|B) = \frac{Pr(B|A) Pr(A)}{Pr(B)}
$$


Applied to cold/covid symptoms
$$
Pr(\mathrm{covid}|\mathrm{symptoms})= \frac{Pr(\mathrm{symptoms}|\mathrm{covid}) Pr(\mathrm{covid})}{Pr(\mathrm{symptoms})}
$$

Symptoms and Cause
======================

$$
Pr(\mathrm{covid}|\mathrm{symptoms})= \frac{Pr(\mathrm{symptoms}|\mathrm{covid}) Pr(\mathrm{covid})}{Pr(\mathrm{symptoms})}
$$

If covid is rare, how likely is it that I have covid given that I have symptoms?
$$
Pr(\mathrm{covid}|\mathrm{symptoms})= \frac{0.30  0.001 }{0.02*1 + 0.001*0.3 } = 0.015 
$$

If covid is common, how likely is it that I have covid given that I have symptoms?
$$
Pr(\mathrm{covid}|\mathrm{symptoms})= \frac{0.30  0.1 }{0.02*1 + 0.1*0.3 } = 0.6
$$

