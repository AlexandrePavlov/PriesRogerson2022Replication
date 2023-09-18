#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import numpy as np
from scipy.optimize import fsolve

global pigrdpts,pigrid,pigriddiv,yb,yg,beta,b,gam,eta,pkh,lam,alph,pi0,kh,sigeps,A,kr

# Function for calculating ergodic distribution
# 5 "states": unemployed, paying k_h and unknown type, not paying k_h and
# unknown type, paying k_h and known type, not paying k_h and known type
def calculate_mean_wages():
    
    global mean_wages_first_qtr,mean_wages_first_year,mean_wages_third_year,mean_wages_fifth_year
    
    tmatrix = np.array([[1-pf*probmatch, lam+(1-lam)*alph*(1-probg),lam+(1-lam)*alph*(1-probg),lam,lam],\
            [pf*probmatch, (1-lam)*(1-pkh)*(1-alph),0,0,0],\
            [0,(1-lam)*(1-alph)*pkh, (1-lam)*(1-alph), 0, 0],\
            [0,(1-lam)*(1-pkh)*alph*probg, 0, (1-lam)*(1-pkh), 0],\
            [0,(1-lam)*pkh*alph*probg, (1-lam)*alph*probg, (1-lam)*pkh, (1-lam)]])

    # steady state distribution
    mtr = np.eye(5) - tmatrix
    mtr[4,0:5] = 1
    invmtr = np.linalg.inv(mtr)
    erg = invmtr[:,4]
    u = erg[0]                 # unemployment
    e0pi = erg[1]              # matches paying k_h, unknown type
    e1pi = erg[2]              # matches not paying k_h, unknown type
    e01 = erg[3]               # matches paying k_h, known type
    e11 = erg[4]               # matches not paying k_h, known type

    # Mean wages
    meanwages=(e01*w0[-1]+e11*w1[-1]+((e0pi*w0+e1pi*w1)*H_pi_dist)@(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]))/(1-u)

    # calculate the distribution across the 5 states, as a function of tenure

    nperiods = 260  # number of periods to calculate profile
    tendist = np.zeros((5,nperiods))
    tendist[1,0] = 1
    
    trans1 = tmatrix
    trans1[:,0] = 0

    for i in range(1,nperiods):
        tendist[:,i] = tmatrix@tendist[:,i-1]

    # calculate average wages as a function of tenure:

    ave_wg_ten_profile = [w0*H_pi_dist@np.transpose(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]),\
                          w1*H_pi_dist@np.transpose(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]),\
                          w0[-1], w1[-1]]@tendist[1:5,:]/np.sum(tendist[1:5,:],axis=0)
    
    mean_wages_first_qtr = np.sum([w0*H_pi_dist@np.transpose(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]),\
                                    w1*H_pi_dist@np.transpose(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]),\
                                    w0[-1],w1[-1]]@tendist[1:5,:13])/np.sum(tendist[1:5,:13])

    mean_wages_first_year = np.sum([w0*H_pi_dist@np.transpose(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]),\
                                    w1*H_pi_dist@np.transpose(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]),\
                                    w0[-1],w1[-1]]@tendist[1:5,:52])/np.sum(tendist[1:5,:52])

    mean_wages_third_year = np.sum([w0*H_pi_dist@np.transpose(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]),\
                                    w1*H_pi_dist@np.transpose(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]),\
                                    w0[-1],w1[-1]]@tendist[1:5,104:156])/np.sum(tendist[1:5,104:156])

    mean_wages_fifth_year = np.sum([w0*H_pi_dist@np.transpose(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]),\
                                    w1*H_pi_dist@np.transpose(pigrid>pibar)/np.sum(H_pi_dist[pigrid>pibar]),\
                                    w0[-1],w1[-1]]@tendist[1:5,208:260])/np.sum(tendist[1:5,208:260])
    
    return

###############################################################################

def solvemodel(x,targs,vers,show):
    from scipy.optimize import fsolve 
    from scipy.stats import norm
    import numpy as np
    
    global pe,pf,u,e0,e1,w0,w1,tendist,probmatch,probg,H_pi_dist,pibar,theta,oneqhazrate,twoqhazrate,threeplushazrate,hiresrate,jfr,meanvacdur
    
    if vers == 1:
        localpi0 = pi0
        localkh = kh
        localsigeps = x[0]
        localA = x[1]
        localkr = x[2]
    elif vers == 2:
        localpi0 = x[0]
        localkh = kh
        localsigeps = sigeps
        localA = x[1]
        localkr = x[2]
    elif vers == 3:
        localpi0 = pi0
        localkh = x[2]
        localsigeps = x[0]
        localA = x[1]
        localkr = kr

    # Compute the cumulative distribution of pi, conditional on normally distributed signal (z).
    # This must be re-computed every time the values of sigeps or pi0 change (which is every time
    # this function is called).

    # Find value of z such that pi equals a particular value of pigriddiv
    zz = np.zeros(pigrdpts + 1)
    for j in range(pigrdpts + 1):
        zz[j] = fsolve(lambda z: pigriddiv[j] - localpi0*np.exp(-(z-yg)**2/2/localsigeps**2)/(localpi0*np.exp(-(z-yg)**2/2/localsigeps**2) + (1-localpi0)*np.exp(-(z-yb)**2/2/localsigeps**2)),0,xtol=1e-16)

    # Probabilities of getting a value of pi between two successive values of pigriddiv
    H_pi_dist = localpi0*norm.cdf(zz[1:],yg,localsigeps) + (1-localpi0)*norm.cdf(zz[1:],yb,localsigeps) - (localpi0*norm.cdf(zz[:-1],yg,localsigeps) + (1-localpi0)*norm.cdf(zz[:-1],yb,localsigeps))
    H_pi_dist = H_pi_dist/np.sum(H_pi_dist)  # normalize, to ensure elements sum to 1 exactly.

    ####################################################

    xx = 1/2000  # adjustment factor for convergence of pe
    pe = 0.2    # initial value for pe

    # Finding total surplus value function by value function iteration
    # Create/initiate value functions
    S0old = np.zeros(len(pigrid))
    S1old = np.zeros(len(pigrid))
    Je0old = np.zeros(len(pigrid))
    Je1old = np.zeros(len(pigrid))
    Juold = 0
    Ve0old = np.zeros(len(pigrid))
    Ve1old = np.zeros(len(pigrid))
    Vuold = 0
    Vu = 0

    difpe = 1
    tol = 1e-8
    while abs(difpe) > tol:

        # Rate at which firms meet unemployed workers (p.273)
        pf = localA**(1/(1-gam))*pe**((-gam)/(1-gam))

        difs = 1
        while difs > tol:
            # Array of total surplus when paying startup cost (p.277) for each pi
            S0new = np.maximum(pigrid*yg+(1-pigrid)*yb-b-localkh\
                               +beta*(1-lam)*((1-pkh)*(alph*pigrid*S0old[-1]+(1-alph)*S0old)\
                                              +pkh*(alph*pigrid*S1old[-1]+(1-alph)*S1old))\
                               -beta*pe*eta*np.dot(H_pi_dist,S0old),0)
            # Array of total surplus when not paying startup cost (p.277) for each pi
            S1new = np.maximum(pigrid*yg+(1-pigrid)*yb-b\
                               +beta*(1-lam)*(alph*pigrid*S1old[-1]+(1-alph)*S1old)\
                               -beta*pe*eta*np.dot(H_pi_dist,S0old),0)
            # Absolute difference between successive iterates
            difs = np.max([np.max(np.abs(S1old-S1new)), np.max(np.abs(S0old-S0new))])
            # Updating iteration
            S1old = S1new
            S0old = S0new

        peold = pe
        Ju = (-localkr+beta*pf*(1-eta)*np.dot(H_pi_dist,S0new))/(1-beta)
        difpe = Ju
        pe = peold*(1+difpe*xx)  # increase worker's finding rate if Ju>0
    
    #labor market tightness
    theta = pe/pf
    #wage of new match
    w0 = b+eta*(pigrid*yg+(1-pigrid)*yb-localkh-b+localkr*theta)
    #wage of old match
    w1 = b+eta*(pigrid*yg+(1-pigrid)*yb-b+localkr*theta)

    #threshold probability
    pibar = pigrid[np.sum(S0new == 0)]

    ####################################################

    # Calculate statistics of interest, given the equilibrium pe, pf, and pibar from above

    probmatch = np.dot(H_pi_dist,pigrid>pibar)  # fraction of meetings with high enough pi to form/continue match

    probg = (H_pi_dist/np.sum(H_pi_dist[pigrid>pibar]))@(pigrid*(pigrid>pibar))  # probability that matches are actually good (conditional on having been formed)

    trans = np.zeros((3, 3))  # three states: unemployed, unknown type, known good matches

    trans = np.array([[1-pe*probmatch, lam+(1-lam)*alph*(1-probg),lam],[pe*probmatch,(1-lam)*(1-alph),0],[0, (1-lam)*alph*probg,1-lam]])

    # steady state
    mtr = np.eye(3) - trans
    mtr[2,:] = 1
    invmtr = np.linalg.inv(mtr)
    erg = invmtr[:,2]
    u = erg[0]  # unemployment
    e0 = erg[1]  # matches of unknown type
    e1 = erg[2]  # matches known to be good


    # Calculate the job tenure distribution
    # --Allow mass of one unit to enter and the trace the realized tenure
    # distribution of that cohort
    # --This is equivalent to looking at the point-in-time distribution if one
    # unit is allowed to enter each period. The whole distribution would simply
    # scale up or down with the size of the mass of entrants.

    tendist = np.zeros((3, 2000))
    tendist[:, 0] = [0, 1, 0]
    trans1 = trans.copy()
    trans1[:, 0] = 0  # don't allow any exits from unemployment after the first period, so this just tracks the initial mass of entrants
    for i in range(1, 2000):
        tendist[:, i] = np.dot(trans1, tendist[:, i-1])

    # fqseps: All separations among jobs newly created in the quarter=(start in first week of quarter and separate within 12
    # periods)+(start in second week of quarter and separate within 11
    # periods)+(start in third week of quarter and separate within 10)+...etc
            
    fqseps = 13*np.sum(tendist[1:3,0]) - np.sum(tendist[1:3,13]) - np.sum(tendist[1:3,12]) -\
            np.sum(tendist[1:3,11]) - np.sum(tendist[1:3,10]) - np.sum(tendist[1:3,9]) -\
            np.sum(tendist[1:3,8]) - np.sum(tendist[1:3,7]) - np.sum(tendist[1:3,6]) -\
            np.sum(tendist[1:3,5]) - np.sum(tendist[1:3,4]) - np.sum(tendist[1:3,3]) -\
            np.sum(tendist[1:3,2]) - np.sum(tendist[1:3,1]);
    
    # empspells: (Employment among all tenure categories at start of
    # quarter)+(new starts in 2nd week)+(new starts in third week)+...+(new
    # starts in 13th week)
    
    empspells = np.sum(np.sum(tendist[1:3,])) + 12*np.sum(tendist[1:3,0])

    sqrate = fqseps/empspells  # incidence rate of one-quarter spells (q_1 in paper)

    hires = 13*np.sum(tendist[1:3,0])

    # First-quarter hazard rate: among those who enter a job in a given quarter, what fraction have separated by the start of the next quarter?
    oneqhazrate = fqseps/hires

    # Note: we could have used either tendist or scaledtendist in the above calculations for sqrate and fqhazrate.
    # Whether they are scaled or not doesn't matter much so long as the numerators and denominators are scaled equivalently.

    # Two-quarter hazard rate
    twoqseps = np.sum(np.sum(tendist[1:3,1:14])) - np.sum(np.sum(tendist[1:3,14:27]))
    twoqhazrate = twoqseps/np.sum(np.sum(tendist[1:3,1:14]))

    # Three or more quarters separation rate
    threeplusseps = np.sum(np.sum(tendist[1:3,14:])) - np.sum(np.sum(tendist[1:3,27:]))
    threeplushazrate = threeplusseps / np.sum(np.sum(tendist[1:3,14:]))

    # Quarterly hires rate (# hires in the quarter divided by total employment spells)
    hiresrate = hires/empspells

    # Monthly job-finding rate
    jfr = pe*probmatch + (1-pe*probmatch)*pe*probmatch + (1-pe*probmatch)**2*pe*probmatch + (1-pe*probmatch)**3*pe*probmatch  # weekly frequency: "month"==4 weeks

    # Mean vacancy duration in weeks
    meanvacdur = 1/(pf*probmatch)

    # Print output
    if show == 1:
        fid = open('results.txt', 'w')
        fid.write('Unemployment rate: \t\t\t\t %g\n' % u)
        fid.write('Monthly job-finding rate:\t\t %g\n' % jfr)
        fid.write('Ave. vacancy duration (weeks):\t %g\n' % meanvacdur)
        fid.write('Prob(good|match):\t\t\t\t %g\n' % probg)
        fid.write('1 - H(pi^n):\t\t\t\t\t\t %g\n' % probmatch)
        fid.write('Worker meeting rate:\t\t\t\t %g\n' % pe)
        fid.write('First quarter hazard rate:\t\t %g\n' % oneqhazrate)
        fid.write('Second quarter hazard rate:\t\t %g\n' % twoqhazrate)
        fid.write('3+ quarter hazard rate:\t\t\t %g\n' % threeplushazrate)
        fid.write('Hires rate:\t\t\t\t\t\t %g\n' % hiresrate)
        fid.close()
        #with open('results.txt', 'r') as file:
        #    print(file.read())

    # Print results to be cut-and-pasted into tables in LaTeX
    fid = open('results2.txt', 'w')
    fid.write('& %.3f & %.3f & %.3f & %.3f & %.3f & %.2f & %.3f' % (oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u))
    fid.close()
    #with open('results2.txt', 'r') as file:
    #    print(file.read())

    Z = [(np.log(targs[0])-np.log(probg))**2,(np.log(targs[1])-np.log(jfr))**2,(np.log(targs[2])-np.log(meanvacdur))**2]

    return Z

###############################################################################

# Grid for pi
# pi from 0 to 1 discretized with 1500 equidistant points
pigrdpts = 1500
pigrdwidth = 1/pigrdpts
pilo = 0
pihi = 1

# Values of pigrid represent the "midpoint" of a particular gridpoint,
# and values pigriddiv represent the "dividing point" between two
# successive gridpoints.
pigriddiv = np.arange(pilo, pihi+pigrdwidth, pigrdwidth)  # Dividing points between gridpoint ranges
pigrid = np.arange(pilo+pigrdwidth/2, pihi-pigrdwidth/2+pigrdwidth, pigrdwidth)  # Midpoints of gridpoints

yb = 0.4  # Productivity of bad match
yg = 1  # Productivity of good match
beta = 0.96**(1/52)  # 4 percent annual discount rate (period is a week)
b = 0.4  # Flow value of leisure
gam = 0.5  # Elasticity of matches with respect to unemployment
eta = 0.5  # Workers' bargaining power
pkh = 1/52  # Hazard rate for ending of start-up costs

#######################################################################

# Baseline results with pi0=0.40

# Exercises that hold pi0 and kh fixed
pi0 = 0.4  # Prior that y=y_g
kh = 0.11  # Training/start-up costs

# 1999 calibration
alph = 0.146
lam = 0.0085

startvals = np.array([0.809, 0.232, 0.813]) 

# Targeted values for probg, jfr, and meanvacdur (1999)

# probability of good match taken from shortjobs_reduced_form
# job finding rate calculated from BLS data using approach by Shimer (2012)
# mean vacancy duration assumed to be 25/7 (as opposed to 22.7/7 in the original study)
targets = np.array([0.419, 0.486, 25/7])  

Params1999 = fsolve(lambda x: solvemodel(x,targets,1,0), startvals, xtol=1e-12)
fval = solvemodel(Params1999,targets,1,0)

# Reveal the calibrated 1999 values of sigeps, A, and kr
print("Calibrated values of sigeps, A, and kr:")
print(Params1999)
print('\n')

print('Calculations for Table 4')

# Table 4: The model's 1999 moments are calculated and displayed here

# 1999 values of model moments
print('1999 values of model moments')
stats1999 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print(stats1999)
print('\n')

# 2017 calibration
alph = 0.124
lam = 0.0068
startvals = np.array([0.555, 0.197, 0.951]) 
targets = np.array([0.463, 0.364, 28.1/7])  # Targeted values for probg, jfr, and meanvacdur (2017)

print('Calculations for Table 5')

# Solve for parameters
Params2017a = fsolve(lambda x: solvemodel(x,targets,1,0), startvals, xtol=1e-12)
fval = solvemodel(Params2017a,targets,1,0)
                  
# Reveal the calibrated 2017 values of sigeps, A, and kr
print('Calibrated values of sigeps, A, and kr:')
print(Params2017a)
print('\n')

# Show the 2017 model moments
print('2017 values of model moments:')
stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print(stats2017)
print('\n')

print('Calculations for Table 6')

# Table 6 results: The following code shows (changes in) the statistics of interest for the decompositions that change parameters one at a time to their 2017 values

alph = 0.146  # Set alph back to its 1999 value
lam = 0.0085  # Set lam back to its 1999 value

# Change kr
solvemodel([Params1999[0],Params1999[1],Params2017a[2]],targets,1,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in kr:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change A
solvemodel([Params1999[0],Params2017a[1],Params1999[2]],targets,1,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in A:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change lam
lam = 0.0068
solvemodel(Params1999,targets,1,0)
lam = 0.0085  # Restore to 1999 value

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in lam:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change alph
alph = 0.124
lam = 0.0085
solvemodel(Params1999,targets,1,0)
alph = 0.146  # Restore to 1999 value

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in alph:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change sigeps
solvemodel([Params2017a[0],Params1999[1],Params1999[2]],targets,1,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in sigeps:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

########################################################################

# Exercises that hold sigeps and kh fixed

# Note that there is no need here to re-do anything for the 1999 calibration. These exercises are just re-doing the 2017 calibration under the assumption that now pi0, rather than sigeps, is allowed to vary.

sigeps = 0.8088  # Fix this at the 1999 calibrated value
kh = 0.11

# 2017 calibration
alph = 0.124
lam = 0.0068
startvals = np.array([0.452, 0.169, 0.951]) 
targets = np.array([0.463, 0.364, 28.1/7])  # Targeted values for probg, jfr, and meanvacdur (2017)

# Solve for parameters, and show the 2017 model moments
# note: the third argument here is 2, indicating that the three free parameters are pi0, A, and kr
Params2017b = fsolve(lambda x: solvemodel(x,targets,2,0), startvals, xtol=1e-12)
fval = solvemodel(Params2017b,targets,2,0)

print('Calculations for Table 7')

# Reveal the calibrated 2017 values of pi0, A, and kr
print('Calibrated values of pi0, A, and kr:')
print(Params2017b)
print('\n')

print('2017 values of model moments:')
stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print(stats2017)
print('\n')

print('Table 7 results:')

alph = 0.146  # Set alph back to its 1999 value
lam = 0.0085  # Set lam back to its 1999 value

# Note: Here the first parameter is pi0, so need to set it to its 1999 value of 0.4

# Change kr
solvemodel([0.4,Params1999[1],Params2017b[2]],targets,2,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in kr:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change A
solvemodel([0.4,Params2017b[1],Params1999[2]],targets,2,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in A:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change lam
lam = 0.0068
solvemodel([0.4,Params1999[1],Params1999[2]],targets,2,0)
lam = 0.0085  # Restore to 1999 value

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in lam:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change alph
alph = 0.124
lam = 0.0085
solvemodel([0.4,Params1999[1],Params1999[2]],targets,2,0)
alph = 0.146  # Restore to 1999 value

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in alph:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change pi0
solvemodel([Params2017b[0],Params1999[1],Params1999[2]],targets,2,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in pi0:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

########################################################################

# Exercises that hold pi0 and kr fixed

pi0 = 0.4
kr = 0.8141  # Fix this at the 1999 calibrated value

# 2017 calibration
alph = 0.124
lam = 0.0068
startvals = np.array([0.650, 0.202, 0.345]) 
targets = np.array([0.463, 0.364, 28.1/7])

# note: the third argument here is 3, indicating that the three free parameters are pi0, A, and kh
Params2017c = fsolve(lambda x: solvemodel(x,targets,3,0), startvals, xtol=1e-12)
fval = solvemodel(Params2017c,targets,3,0)

print('Calculations for Table 8')

# Reveal the calibrated 2017 values of sigeps, A, and kh
print('Calibrated values of sigeps, A, and kh:')
print(Params2017c)
print('\n')

print('2017 values of model moments:')
stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print(stats2017)
print('\n')

print('Table 8 results:')

alph = 0.146  # Set alph back to its 1999 value
lam = 0.0085  # Set lam back to its 1999 value

# Note: Here the third parameter is kh, so need to set it to its 1999 value of 0.11 (except for the exercise where it is being changed)

# Change kh
solvemodel([Params1999[0],Params1999[1],Params2017c[2]],targets,3,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in kh:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change A
solvemodel([Params1999[0],Params2017c[1],0.11],targets,3,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in A:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change lam
lam = 0.0068
solvemodel([Params1999[0],Params1999[1],0.11],targets,3,0,)
lam = 0.0085  # Restore to 1999 value

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in lam:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change alph
alph = 0.124
lam = 0.0085
solvemodel([Params1999[0],Params1999[1],0.11],targets,3,0)
alph = 0.146  # Restore to 1999 value

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in alph:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change sigeps
solvemodel([Params2017c[0],Params1999[1],0.11],targets,3,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in sigeps:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

########################################################################

# Appendix. Re-do the exercises, but with a baseline value of pi0=0.38 (version of model in which sigeps, A, and kr can change)

print('Calculations for the table in the Appendix')

pi0 = 0.38  # Prior that y=y_g
kh = 0.11  # Training/start-up costs

# 1999 calibration
alph = 0.146
lam = 0.0085

startvals = np.array([0.691, 0.249, 0.813]) 
targets = np.array([0.419, 0.486, 25/7])  # Targeted values for probg, jfr, and meanvacdur (1999)

Params1999b = fsolve(lambda x: solvemodel(x,targets,1,0), startvals, xtol=1e-12)
fval = solvemodel(Params1999b,targets,1,0)

# Reveal the calibrated 1999 values of sigeps, A, and kr
print('Calibrated values of sigeps, A, and kr:')
print(Params1999b)
print('\n')

# 1999 values of model moments
print('1999 values of model moments')
stats1999 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print(stats1999)
print('\n')

# 2017 calibration
alph = 0.124
lam = 0.0068
startvals = np.array([0.515, 0.210, 0.951]) 
targets = np.array([0.463, 0.364, 28.1/7])  # Targeted values for probg, jfr, and meanvacdur (2017)

# Solve for parameters, and show the 2017 model moments
Params2017d = fsolve(lambda x: solvemodel(x,targets,1,0), startvals, xtol=1e-12)
fval = solvemodel(Params2017d,targets,1,0)

# Reveal the calibrated 2017 values of sigeps, A, and kr
print('Calibrated values of sigeps, A, and kr:')
print(Params2017d)
print('\n')

print('2017 values of model moments:')
stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print(stats2017)
print('\n')

# Panel B: The following code shows (changes in) the statistics of interest for the decompositions that change parameters one at a time to their 2017 values

alph = 0.146  # Set alph back to its 1999 value
lam = 0.0085  # Set lam back to its 1999 value

# Change kr
solvemodel([Params1999b[0],Params1999b[1],Params2017d[2]],targets,1,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in kr:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change A
solvemodel([Params1999b[0],Params2017d[1],Params1999b[2]],targets,1,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in A:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change lam
lam = 0.0068
solvemodel(Params1999b,targets,1,0)
lam = 0.0085  # Restore to 1999 value

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in lam:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change alph
alph = 0.124
lam = 0.0085
solvemodel(Params1999b,targets,1,0)
alph = 0.146  # Restore to 1999 value

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in alph:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

# Change sigeps
solvemodel([Params2017d[0],Params1999b[1],Params1999b[2]],targets,1,0)

stats2017 = [oneqhazrate, twoqhazrate, threeplushazrate, hiresrate, jfr, meanvacdur, u]
print('Changes due to change in sigeps:')
x = np.array(stats2017) - np.array(stats1999)
print([round(elem,4) for elem in x])
print('\n')

########################################################################

# Calculate mean wages at different tenure

# Before running the code that calculates average wages, we first need to solve a version of the model,
# so as to determine the equilibrium values of the variables needed to calculate those averages wages, such as probmatch, pe, pf, etc.

print('Calculations for section 5.5')

# Calculations in the third paragraph of section 5.5

print('Third paragraph')

pi0 = 0.4  # Reset to value from baseline parameterization.

# 1999
alph = 0.146
lam = 0.0085
solvemodel(Params1999,targets,1,0)
calculate_mean_wages()
q1wages1999 = mean_wages_first_qtr
y5wages1999 = mean_wages_fifth_year

# 2017
alph = 0.124
lam = 0.0068
solvemodel(Params2017a,targets,1,0)
calculate_mean_wages()
q1wages2017 = mean_wages_first_qtr
y5wages2017 = mean_wages_fifth_year

print('Percentage change in first-quarter mean wages')
print(q1wages2017/q1wages1999-1)
print('\n')

print('Percentage change in first-quarter/fifth-year ratio')
print((q1wages2017/y5wages2017)/(q1wages1999/y5wages1999)-1)
print('\n')

# Calculations for the fourth paragraph of section 5.5

print('Fourth paragraph')

# 2017 value for sigeps, 1999 for all others
alph = 0.146
lam = 0.0085
solvemodel([Params2017a[0],Params1999[1],Params1999[2]],targets,1,0)
calculate_mean_wages()
q1wages2017 = mean_wages_first_qtr
y5wages2017 = mean_wages_fifth_year

print('Percentage change in first-quarter mean wages')
print(q1wages2017/q1wages1999-1)
print('\n')

print('Percentage change in first-quarter/fifth-year ratio')
print((q1wages2017/y5wages2017)/(q1wages1999/y5wages1999)-1)
print('\n')

# Calculations for the fifth paragraph of section 5.5
solvemodel(Params2017c,targets,3,0)
calculate_mean_wages()

print('First-quarter wages in 1999')
print(q1wages1999)
print('\n')

print('First-quarter wages in 2017')
q1wages2017 = mean_wages_first_qtr
print(q1wages2017)
print('\n')

print('Percentage change in first-quarter mean wages')
print(round(q1wages2017,3)/round(q1wages1999,3)-1)
print('\n')

print('Fifth-year wages in 1999')
print(round(y5wages1999,3))
print('\n')

print('Fifth-year wages in 2017')
y5wages2017 = mean_wages_fifth_year
print(round(y5wages2017,3))
print('\n')

print('Percentage change in fifth-year mean wages')
print(round(y5wages2017,3)/round(y5wages1999,3)-1)
print('\n')

