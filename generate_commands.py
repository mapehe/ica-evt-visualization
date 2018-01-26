import os, re
import numpy as np
import argparse

parser = argparse.ArgumentParser(description='Generate input for the plot.r script.')
parser.add_argument("--n", required=True, help="The number of settings to plot.", type=int)
parser.add_argument("--random", action="store_true", help="Pick n random settings.")

args = parser.parse_args()

if args.random:
    s = os.listdir("data")
    s = filter(lambda(x): re.compile(r"none").search(x) is None, s)
    print ",".join(np.random.choice(map(lambda(x): "data/"+x, s), args.n))
