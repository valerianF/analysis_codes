"""
Composing with acoustic niches: A laboratory soundscape evaluation of sound installation reactivity

Valérian Fraisse

The present code is the code used to implement acoustic reactivity for the stimuli used
in the laboratory study described in the journal article 'Composing with acoustic niches: A laboratory soundscape evaluation 
of sound installation reactivity' by Fraisse V., Schütz N., Vincent C., Wanderley M., Guastavino C., and Misdariis N., 
submitted to npj Acoustics. 

The code is not functional per se as it relies on measurement data that is not included in the supplementary materials. 
However, it can be used as a template for similar reactivity implementations.
"""

import numpy as np
from reathon.nodes import Track, Item, Source, Project
import os

"""
Functions
"""

def rms_energy(
        array, 
        sr=48000,
        win=1
        ):
    """ This function computes the RMS energy and peak level of an audio signal, in dBFS. """
    dn = int(win*sr)
    N = len(array)
    nLim = N // dn
    
    rms = np.zeros(nLim)
    peak = np.zeros(nLim)
    time = []

    for i in range(0, nLim):
        rms[i] = np.sqrt(np.mean(np.abs(array[int(i*dn) : int((i+1)*dn)])**2))
        peak[i] = np.max(array[int(i*dn) : int((i+1)*dn)])
        time.append(((i+1)*dn)/sr)

    rms = 20*np.log10(rms/max(rms))
    peak = 20*np.log10(peak/np.max(peak))
    
    return time, rms, peak

def create_env_string(third_laeq_bands, factor, pos=True, name=''):
    """ This function creates a stringchain to be fed into Reaper as an FX chain for the 
    JS "12-Band EQ Niches Acoustiques" plugin. """
    
    str_out = '\nBYPASS 1 0 0\n<JS "12-Band EQ Niches Acoustiques" ' + name + '\n0 0 '
    laeq_mean = 10*np.log10(np.mean(np.power(np.full(len(third_laeq_bands), 10), third_laeq_bands/10)))

    for j in range(1, 13):
        band = np.array([i for i in third_laeq_bands[2*j-2:2*j+1]])

        # Octave band LAeqs: sum of three surrounding 1/3 octave bands, 
        # centered on each octave starting from 80Hz to 12.5kHz

        laeq_band = 10*np.log10(np.sum(np.power(np.full(len(band), 10), band/10)))

        if pos:
            str_out += str((laeq_band-laeq_mean)/factor) + " "
        else:
            str_out += str(-(laeq_band-laeq_mean)/factor) + " "

    str_out += "22000 0\n>"
    
    return str_out
    

def reaper_addedsound_track(time, rms_inds, eq_gains, source, shape=2, name=''):
    """ This function creates a Reaper track with the appropriate settings for an added sound track, 
    including envelope settings ("VOLENV2") and adds the given source as an item on that track."""
    
    str_suivi = "\n"

    for ind in range(0, len(rms_inds)):
        str_suivi += "PT " + str(time[ind]) + " " + str(rms_inds[ind]) + " " + str(shape) + "\n"
        
    track = Track()

    track.props = [
        ["NAME", name],
        ["NCHAN", 1],
        ["HWOUT", "30 0 1 0 0 0 1024 -1:U -1"],
        ["MAINSEND", "0 0"],
        ["VOLPAN", "1 0 -1 -1 1"],
        # VOLENV2: enveloppe (post fx) settings
        # field 1, float, fade in curve type
        # field 2, float, gain value (linear)
        # field 3, int, curve type (2: Slow start/end, piecewise cubic)
        ["<VOLENV2", str_suivi + " \n>"],
        ["<FXCHAIN\n", eq_gains + " \n>"],
    ]

    if source is not None:
        item = Item(source)

        item.props = [
            ["LENGTH", 380],
            ["POSITION", 0]
        ]

        track.add(item)
    
    return track

def reaper_em32_track(source, name=''):
    """This function creates a Reaper track with the appropriate settings 
    for an em32 track, and adds the given source as an item on that track. 
    It is used to import the em32 recordings of the scenarios 
    within the Reaper project for the listening session"""

    track = Track()

    track.props = [
        ["NAME", name.split('.wav')[0]],
        ["NCHAN", 32],
        ["MAINSEND", "0 0"],
        ["HWOUT", "34 0 1 0 0 0 16384 -1:U -1"]
    ]

    item = Item(source)

    item.props = [
        ["LENGTH", 380],
        ["POSITION", 0]
    ]

    track.add(item)
    
    return track

"""
Computing  envelope gains for temporal reactivity
"""
delta_t = 2 # window size δ_t
temp_factors = [1, 2, 3] # Intensity coefficients n_t
rms_dbfs_tracked = []
rms_dbfs_inv = []

# Computing RMS dBFS values
time, rms_dbfs, peak_dbfs = rms_energy(
    measurement_recording, # import array containing recording of scenario into the listening room
    win=delta_t
    )
min_dbfs = -min(rms_dbfs)

# Computing envelope gains to be fed into Reaper (equations 1 and 2)
for n_t in temp_factors:
    rms_dbfs_tracked.append(np.power(np.full(len(rms_dbfs), 10), n_t*rms_dbfs/20))
    rms_dbfs_inv.append(np.power(np.full(len(rms_dbfs), 10), (-rms_dbfs*n_t-n_t*min_dbfs)/20))

"""
Computing EQ filter gains for spectral reactivity
"""
# import 1/3 A-weighted octave values from 
# 63 Hz to 20 kHz measured in the listening room for each scenario
db_a_s = measurement_thirdoctave

strs_filter_tracked = ""
strs_filter_inv = ""

spect_factors = [6, 4, 3, 2] # Intensity coefficients n_f

# Computing spectral envelope gains in the form of stringchains to be fed into Reaper
for n_f in spect_factors:
    strs_filter_tracked += create_env_string(db_a_s, n_f, pos=True, name="tracked_nf")
    strs_filter_inv += create_env_string(db_a_s, n_f, pos=False, name="inv_nf")

"""
Create Reaper project with reactive enveloppes
"""
project = Project() # Create a Reaper project within which to import the tracks for the listening session

# Import em32 track
project.add(reaper_em32_track(Source(file = "path_to_em32_track")))

# Import audio file within the Reaper project
source = Source(file = os.path.join("path_to_audio_files", x))
        

# Add each added sound track for tracked enveloppe
for i in range(0, len(temp_factors)):
    project.add(reaper_addedsound_track(time,
                                        rms_dbfs_tracked[i], 
                                        strs_filter_tracked, 
                                        source, 
                                        name='tracked_nt' + str(temp_factors[i]),
                                        onset=0))

# Add each added sound track for inverse enveloppe  
for i in range(0, len(temp_factors)):
    project.add(reaper_addedsound_track(time,
                                        rms_dbfs_inv[i], 
                                        strs_filter_inv, 
                                        source, 
                                        name='inv_nt' + str(temp_factors[i]),
                                        onset=0))

project.write("session_react.rpp")