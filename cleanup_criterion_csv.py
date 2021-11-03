import sys

infile = sys.argv[1]
outfile = sys.argv[2]

with open(infile, "r") as ifile:
    with open(outfile, "w") as of:
        lines = ifile.readlines()
        header = "n,k,pruned" + lines[0][lines[0].index(","):]
        print(header, end='', file=of)

        for line in lines[1:]:
            if line.startswith("Name"):
                continue
            (verif, values) = line.split("\",")
            nkp = verif.split("/")[1]
            print(nkp + "," + values, end='', file=of)

print("Done")

