import sys, requests
from os import path


if len(sys.argv) < 3:
    print("usage: python get_inputs.py [year #] [day #]")
    quit()

year = sys.argv[1]
day = sys.argv[2]
filename = 'inputs/day' + day + '.txt'
url = 'https://adventofcode.com/' + year + '/day/' + day + '/input'

if path.exists(filename):
    with open(filename, 'r') as f:
        print(f.read())
        quit()


r = requests.get(url, cookies={'session': '53616c7465645f5f801faf75e48194409166259363239cdbe97d70659769065b08c0653067e2e0cdaf06192ab225e069'})

with open(filename, 'w') as f:
    f.write(r.content)
    print(r.content)
    
