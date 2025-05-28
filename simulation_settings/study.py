import sim_settings as set
import sim_model as sim
import numpy as np
import pandas as pd

# variables required to be pre-specified
rng = np.random.default_rng(504)
var_pars = ["d4", "K2", "e5", "c42", "c52", "c3"]
stress_lvl = ["low", "high"]
data = [[]]
t, dt = 40320, 0.01
pp_nr = 0
# main simulation loop
for level in stress_lvl:
    for par in var_pars:
        for i in range(84):
            # generate suicide data
            rng = np.random.default_rng()
            modifiers = set.change_params(par, i)
            model_output = sim.gen_suicide_sim(t, dt, stress = set.set_stressor(level),
                                rem_params = set.default_params(key = list(modifiers.keys())[0], 
                                                   val = modifiers.get(par)), rng = rng)
            # indices for correct appending
            #start_index = len(data[pp_nr]) * pp_nr
            #end_index = (len(model_output) * len(data))

            # filling in simulation values
            model_output[0:40320, 0] = pp_nr
            model_output[0:40320, 2] = stress_lvl.index(level)
            model_output[0:40320, 3] = var_pars.index(par)
            model_output[0:40320, 4] = modifiers.get(par)
            pp_nr = pp_nr + 1

            # collect data
            data.append(model_output)
        
        

# name columns
data.remove(data[0])
data = pd.DataFrame(np.concatenate(data))
data.columns = ["pp_nr", "time", "stress_lvl", "par_changed", "par_value", "stressor", "av_state", 
                         "urge_escape", "sui_thoughts", "other_escape",
                         "ext_change", "int_change"]
data.to_parquet("sim_data.parquet", index = False, compression = "snappy", 
                partition_cols = ["pp_nr"])

