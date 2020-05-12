import sys
import subprocess
from time import time
import os
from shutil import copyfile

fnr = 0

args = sys.argv

bin_name = "aoc2015"
mod_cpy_dirs = [
    "src/",
]

if len(args) >= 2:
    optimize = args[1] == "-O"
else:
    optimize = False


def compile_file(f):
    global fnr
    command = ["gfortran", "-cpp", "-c"]
    if optimize:
        command.append("-O3")
    command.append("-o")
    command.append(str(fnr) + ".o")
    command.append(f)
    subprocess.call(command)
    fnr += 1


def compile_dir(p):
    with open(p + "/fbld.txt") as file:
        for l in file:
            l = l.strip()
            if os.path.isdir(os.path.join(p, l)):
                compile_dir(os.path.join(p, l))
            else:
                f = os.path.join(p, l) + ".f90"
                compile_file(f)


def delete_all_ext(dir, ext):
    for f in os.listdir(dir):
        (root, ext2) = os.path.splitext(f)
        if ext == ext2:
            os.remove(os.path.join(dir, f))


def copy_all_ext(d2, ext):
    for f in os.listdir("."):
        (root, ext2) = os.path.splitext(f)
        if ext == ext2:
            copyfile(f, os.path.join(d2, f))


for d in mod_cpy_dirs:
    delete_all_ext(d, ".mod")

delete_all_ext(".", ".mod")

compile_dir(".")

objfiles = [f for f in os.listdir(".") if os.path.splitext(f)[1] == ".o"]
command = ["gfortran"]
command.extend(objfiles)
command.extend(["-o", bin_name])

subprocess.call(command)

delete_all_ext(".", ".o")

for d in mod_cpy_dirs:
    copy_all_ext(d, ".mod")

command = ["./" + bin_name]

if optimize:
    command.extend(args[2:])
else:
    command.extend(args[1:])

# Running the binary
print("Running...")
t = time()
subprocess.call(command)
t = time() - t
print("Took", round(t, 4), "seconds")
