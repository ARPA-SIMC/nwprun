#!/usr/bin/env python3
# encoding: utf-8

import sys, os, shutil
import h5py 
import math
import argparse

def get_args():
    parser = argparse.ArgumentParser()

    parser.add_argument( '-f', '--file', dest="filename", required=True,
                         help="File da analizzare" )
    parser.add_argument( '-t', '--tolerance', dest="tolerance", required=False,
                         help="Tolleranza sull'angolo di elevazione", default=0.2 )
    parser.add_argument( '-o', '--output', dest="dir_quaratine", required=False,
                         help="Cartella per quarantena", default="")
    
    args = parser.parse_args()
    return args

if __name__ == '__main__':
    
    args = get_args()
    filename = args.filename
    tolerance = args.tolerance
    dir_quaratine = args.dir_quaratine

# Estraggo la lista di elevazioni dal file odim
with h5py.File(filename, 'r') as f:
    datasets = [k for k in f.keys() if "dataset" in k]
    print(datasets)
    elev_array = [f["dataset{}/where".format(x)].attrs["elangle"] for x in range(1, len(datasets) + 1)]
    print(elev_array)

# Confronto con i possibili default
defaults = [
    [ 0.5, 1.4, 2.3, 3.2, 4.2, 5.0, 7.0, 9.5, 13.0, 18.0 ], 
    [ 0.5, 1.4, 2.3, 3.2, 4.2, 5.0 ], 
    [ -0.1, 0.5, 1.2, 2.0, 3.0, 4.4, 5.8, 7.4, 10.0, 15.0 ],
    [ -0.3, 0.7, 2.1, 4.0, 6.4, 9.7, 15.0 ],
    [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 7.0, 9.0, 11.0, 13.5, 16.0 ],
    [ -0.2, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 7.0, 9.0, 11.0, 13.5, 16.0 ], 
    [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 7.0, 9.1, 11.0, 13.5, 16.0 ],
    [ -0.1, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 7.1, 9.1, 11.0, 13.5, 16.0 ],
    [ -0.1, 0.6, 1.5, 2.5, 3.5, 4.6, 5.6, 7.1, 9.1, 11.0, 13.5, 16.0 ],
    [ 0.8, 1.4, 2.4, 3.4, 4.4, 5.9, 7.9, 9.9, 14.9 ],
    [ 1.0, 1.4, 2.4, 3.4, 4.4, 5.9, 7.9, 9.9, 14.9 ],
    [ 0.0, 0.7, 1.5, 2.5, 3.6, 5.0, 6.5, 8.4, 10.6, 13.3, 16.3, 20.0 ],
    [ 0.5, 1.5, 2.5, 3.5 ],
    [ 0.0, 1.0, 2.0, 3.0, 4.0, 8.0, 14.0 ],
    [ 0.5, 1.4, 2.3, 3.2, 4.1, 5.0, 25.0 ],
    [ 0.5, 1.4, 2.3, 3.2, 4.1, 5.0, 8.0, 11.0, 18.0, 25.0 ],
    [ 0.5, 1.4, 2.3, 3.2, 4.1, 5.0, 8.0, 11.0, 18.0 ]
    ]

usabile = False

for i in defaults:
    if len(i) != len(elev_array):
        continue
    else:
        if all((math.isclose(n1, n2, abs_tol = tolerance) for n1, n2 in zip(elev_array, i))):
            usabile = True
            print("Default presente, esco")
            sys.exit()
        else:
            print("Default assente, continuo la verifica")

if usabile == False:
    if dir_quaratine != "": 
        print("Sposto il file in quarantena")
        try:
            shutil.move(filename, dir_quaratine)
        except shutil.Error:
            os.remove(filename)
    else:
        print("Elimino il file")
        os.remove(filename)


