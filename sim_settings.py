import numpy as np

def change_params(param, index):
    all_params = {
        "d4": np.linspace(0.1, 5, 84).tolist(),
        "K2": np.linspace(0.1, 0.3, 84).tolist(),
        "e5": np.linspace(1, 5, 84).tolist(),
        "c42": np.linspace(0.1, 0.5, 84).tolist(),
        "c52": np.linspace(0.1, 0.5, 84).tolist(),
        "c3": np.linspace(1, 5, 84).tolist()
    }
    return {param: all_params[param][index]}

def default_params(d4 = 1, K2 = 0.2, e5 = 3, c42 = 0.25, c52 = 0.2, c3 = 3, key = "default", val = 0):
    # neg affect params
    A0, K2, b2, a2, d2, e2, g2 = 0.3, K2, 4, 2, 1.5, 1, 0.5

    # urge to escape params
    U0, b3, c3 = 0.25, 1.5, c3

    # suicidal thoughts params
    T0, d4, c41, c42 = 0, d4, 100, c42

    # other escape params
    O0, e5, c51, c52 = 0.2, 3, 50, c52

    # external-focused changed params
    E0, K6, f6, b6, c6 = 0.05, 0.1, 0.5, 0.41, 0.82

    # internal-focused change params
    I0, K7, g7, b7, c7 =  0.1, 0.05, 0.5, 0.65, 1.3

    # mutable parameters
    if key != "default":
        if key == "d4":
            d4 = val
        elif key == "K2":
            K2 = val
        elif key == "e5":
            e5 = val
        elif key == "c42":
            c42 = val
        elif key == "c52":
            c52 = val
        elif key == "c3":
            c3 = val

    return np.array([A0, K2, b2, a2, d2, e2, g2,
            U0, c3, b3, 
            T0, d4, c41, c42, 
            O0, e5, c51, c52, 
            E0, K6, f6, b6, c6, 
            I0, K7, g7, b7, c7])  

def set_stressor(level):
    
    if level == "low":
        sigma = 0.05
        S0, mu, f1 = 0.2, ((sigma**2) / 2), 0.0001
        
    if level == "high":
        sigma = 0.12
        S0, mu, f1 = 0.2, ((sigma**2) / 2), 0.0001
        
    return np.array([mu, sigma, S0, f1])

