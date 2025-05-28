import numpy as np
from numba import njit

@njit
def gen_suicide_sim(t, dt, stress, rem_params, rng):
    
    # define values
    mu, sigma, S0, f1 = stress
    A0, K2, b2, a2, d2, e2, g2, U0, c3, b3, T0, d4, c41, c42, O0, e5, c51, c52, E0, K6, f6, b6, c6, I0, K7, g7, b7, c7 = rem_params

    model_sim = np.zeros((t, 12))
    
    model_sim[0,5] = S0 # stressor
    model_sim[0,6] = A0 # aversive
    model_sim[0,7] = U0 # urge
    model_sim[0,8] = T0 # sui_thoughts
    model_sim[0,9] = O0 # other_escape
    model_sim[0,10] = E0 # ext_change
    model_sim[0,11] = I0 # int_change
            
    for i in range(t - 1):
            
        ## time
        model_sim[i,1] = i + 1
        
        ## stressor and effect of long-term change
        model_sim[i + 1, 5] = model_sim[i, 5]*np.exp((mu - ((sigma**2)/2))*dt + sigma*rng.normal(0, np.sqrt(dt)) - f1*model_sim[i, 5])
        if model_sim[i + 1, 5] < 0:
            model_sim[i + 1, 5] = 0
        
        ## aversive internal state including negative effect of suicidal thoughts
        model_sim[i + 1, 6] = model_sim[i, 6] + dt*(b2*model_sim[i, 6]*(K2 - model_sim[i, 6]) + a2*model_sim[i, 5] - d2*model_sim[i, 8] - e2*model_sim[i, 9] - g2*model_sim[i, 11])
        if model_sim[i + 1, 6] < 0:
            model_sim[i + 1, 6] = 0
            
        ## urge to escape
        model_sim[i + 1, 7] = model_sim[i, 7] + dt*(-c3*model_sim[i, 7] + b3*model_sim[i, 6]) # + 5*dt*new_effect OR really weak new_effect
        if model_sim[i + 1, 7] < 0:
            model_sim[i + 1, 7] = 0
            
        ## suicidal thoughts
        model_sim[i + 1, 8] = model_sim[i, 8] + dt*(-d4*model_sim[i, 8] + (1 / (1 + np.exp(-c41 * (model_sim[i, 7] - c42)))))
        if model_sim[i + 1, 8] < 0:
            model_sim[i + 1, 8] = 0
            
        ## other escape behaviors 
        model_sim[i + 1, 9] = model_sim[i, 9] + dt*(-e5*model_sim[i, 9] + (1 / (1 + np.exp(-c51 * (model_sim[i, 7] - c52)))))
        if model_sim[i + 1, 9] < 0:
            model_sim[i + 1, 9] = 0
            
        ## external-focused change
        model_sim[i + 1, 10] = model_sim[i, 10] + dt*(f6*model_sim[i, 10]*(K6 - model_sim[i, 10]) + b6*model_sim[i, 6] - c6*model_sim[i, 7])
        if model_sim[i + 1, 10] < 0:
            model_sim[i + 1, 10] = 0
            
        ## internal-focused change
        model_sim[i + 1, 11] = model_sim[i, 11] + dt*(g7*model_sim[i, 11]*(K7 - model_sim[i, 11]) + b7*model_sim[i, 6] - c7*model_sim[i, 7])
        if model_sim[i + 1, 11] < 0:
            model_sim[i + 1, 11] = 0  
    
    return(model_sim)