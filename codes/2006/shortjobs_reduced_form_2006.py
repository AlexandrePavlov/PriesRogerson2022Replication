#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import numpy as np
from scipy.optimize import fmin

def moments(x,targs=[0,0,0]):
    import numpy as np
    
    #probability of good match
    pigood=x[0]
    #probability of learning match type
    alpha=x[1]
    #exogenous separation probability
    lam=x[2]
    
    T=1999
    
    empg = np.zeros(T)
    empb = np.zeros(T)
    
    #probability of being employed as a good match
    empg[0] = pigood
    #probability of being employed as a bad match
    empb[0] = 1-pigood
    
    for j in range(1,T):
        #probability of being employed as a good match after j periods
        empg[j] = empg[j-1]*(1-lam)
        #probability of being employed as a bad match after j periods
        empb[j] = empb[j-1]*(1-alpha)*(1-lam)
    
    #total probability of employment after T periods
    emp = empg+empb
    
    #hazard rate for one-quarter spells
    #using formula in p.280
    delta1 = 1-np.sum(emp[1:14])/13
    #hazard rate for two-quarter spells
    #using formula in p.280
    delta2 = 1-np.sum(emp[14:27])/np.sum(emp[1:14])
    #hazard rate for spells of over two quarters
    #using formula in p.280
    delta3 = 1-np.sum(emp[27:])/np.sum(emp[14:])
    #hires rate
    #using formula in p.279
    hirerate = 13/(np.sum(emp)+12)

    #if no second argument is passed then targs=[0,0,0]
    if targs == [0,0,0]:
        return delta1,delta2,delta3,hirerate
    else:
        #sum of squared log-difference between target and model hazard rates to be minimized
        Z=(np.log(targs[0])-np.log(delta1))**2+(np.log(targs[1])-np.log(delta2))**2+\
            (np.log(targs[2])-np.log(delta3))**2
        return Z
    
############################################################################################

opts = {'xtol': 1e-16, 'disp': False}

startvals = np.array([0.5, 0.5, 0.5])  # starting values for pigood, alph, and lam

print('**********Table 1***********')

# 2006
targets = [0.355, 0.351, 0.103]

X = fmin(lambda x: moments(x,targets),startvals,**opts)
fval = moments(X,targets)

pigood2006 = X[0]
alph2006 = X[1]
lam2006 = X[2]

print('pigood2006 : ', str(round(pigood2006,4)))
print('alph2006 : ', str(round(alph2006,4)))
print('lam2006 : ', str(round(lam2006,4)))
print('\n')

[delta1,delta2,delta3,hirerate] = moments([pigood2006, alph2006, lam2006])

print('2006 values of delta1, delta2, delta3, and hirerate:')
print([round(delta1,3), round(delta2,3), round(delta3,3), round(hirerate,3)])

print('\n')
print('*********Table 2************')

# 2017
targets = [0.322, 0.331, 0.095]

X = fmin(lambda x: moments(x,targets),startvals,**opts)

pigood2017 = X[0]
alph2017 = X[1]
lam2017 = X[2]

print('pigood2017 : ', str(round(pigood2017,4)))
print('alph2017 : ', str(round(alph2017,4)))
print('lam2017 : ', str(round(lam2017,4)))
print('\n')

[delta1,delta2,delta3,hirerate] = moments([pigood2017, alph2017, lam2017])

print('Row 2: changes in 2017 values of delta1, delta2, delta3, and hirerate, relative to 2006 model value')
print([round(delta1-0.355,3), round(delta2-0.351,3), round(delta3-0.103,3), round(hirerate-0.181,3)])

[delta1,delta2,delta3,hirerate] = moments([pigood2017, alph2006, lam2006])

print('Row 3: changes in values of delta1, delta2, delta3, and hirerate, when only pigood changes')
print([round(delta1-0.355,3), round(delta2-0.351,3), round(delta3-0.103,3), round(hirerate-0.181,3)])

[delta1,delta2,delta3,hirerate] = moments([pigood2006, alph2017, lam2006])

print('Row 4: changes in values of delta1, delta2, delta3, and hirerate, when only alph changes')
print([round(delta1-0.355,3), round(delta2-0.351,3), round(delta3-0.103,3), round(hirerate-0.181,3)])

[delta1,delta2,delta3,hirerate] = moments([pigood2006, alph2006, lam2017])

print('Row 5: changes in values of delta1, delta2, delta3, and hirerate, when only lam changes')
print([round(delta1-0.355,3), round(delta2-0.351,3), round(delta3-0.103,3), round(hirerate-0.181,3)])

