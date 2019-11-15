import re, sys, os, subprocess

def lineAndColumn(index):
    "This finds line index and column of a word by index"
    lineIndex = len(newLines) + 1
    columnIndex = index + 1
    for i in range(len(newLines)): 
        if(newLines[i] > index): 
            lineIndex = i + 1
            break

    if(lineIndex != 1): 
        columnIndex = index - newLines[lineIndex-2]
    return (lineIndex, columnIndex)

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

string = ""

file_in = 'input.il'
file_out = 'output.cl'

if(len(sys.argv) >= 2):
    file_in = sys.argv[1]

if(len(sys.argv) >= 3):
    file_out = sys.argv[2]

if(os.path.isfile(file_in)):
    print('\nTranspiling {}\"{}\"{} to {}\"{}\"{}\n'.format(bcolors.HEADER, file_in,bcolors.ENDC, bcolors.HEADER, file_out, bcolors.ENDC))
else:
    print("{}\nNo such file: {}\n{}".format(bcolors.FAIL, file_in, bcolors.ENDC))
    exit(-1)

with open(file_in, 'r') as file:
    string = file.read()

validWords = [(m.group(), m.start(), m.end()) for m in re.finditer("[a-zA-z0-9+-/=<>^!@#$%^&]+\\(", string)]
ignoredWords = [(m.group(), m.start()) for m in re.finditer("[a-zA-z0-9+-/=<>^!@#$%^&]+[ ]\\(", string)]
newLines = [m.start() for m in re.finditer("\n", string)]
result = []
i = 0
for (word, start, end) in validWords:
    #print(word)
    word = '(' + word[:-1]

    string = string[:start+i] + word + ' ' + string[end+i:]
    i += 1

with open(file_out, 'w') as file:
    file.write(string)

#print('\n\n'+string)

print("{}Ignored words:{}".format(bcolors.WARNING, bcolors.OKGREEN))
for (word, start) in ignoredWords:
    indexes = lineAndColumn(start)
    print("line: {: <6} column: {: <3} word: \"{}\"".format(indexes[0], indexes[1], word))

print("\n{}Starting the {} with clisp. \nOutput: {}".format(bcolors.OKBLUE, file_out, bcolors.OKGREEN))

subprocess.run(["clisp", file_out])

print(bcolors.ENDC)
