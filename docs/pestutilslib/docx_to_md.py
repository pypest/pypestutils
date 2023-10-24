import sys
import os

def processFile(inFile, outFile):
    mdFile = open(inFile, 'r')
    toc = []
    levels = [0,0,0,0,0 ]
    newFile = open(outFile, 'w')
    tempFile = []
    tocLoc = 0
    partOfToc = False
    
    for line in mdFile:
        if partOfToc and line != '\n':
            continue
        else:
            partOfToc = False
        if 'Table of Contents' in line:
            tocLoc = len(tempFile) + 1
            partOfToc = True
            line += "\n"
        elif line[0] == '#':
            secId = buildToc(line, toc, levels)
            line = addSectionTag(cleanLine(line), secId) + '\n'
        tempFile.append(line)


    for line in toc:
        tempFile.insert(tocLoc, line)
        tocLoc += 1

    newFile.write("\n")
    for line in tempFile:
        newFile.write(line)

    mdFile.close()
    newFile.close()

def addSectionTag(line, secId):
    startIndex = line.find(' ')
    line = line[:startIndex + 1] + '<a id=\'' + secId + '\' />' + line[startIndex + 1:]
    return line

def buildToc(line, toc, levels):
    line = cleanLine(line)
    secId = 's'
    if line[:4] == '####':
        
        #raise UserWarning('Header levels greater than 3 not supported')
        levels[4] += 1
        secId += str(levels[1]) + '-' + str(levels[2]) + '-' + str(levels[3]) + '-' + str(levels[4])
        toc.append('            - [' + line[5:] + '](#' + secId + ')\n')
    elif line[:3] == '###':
        levels[3] += 1
        secId += str(levels[1]) + '-' + str(levels[2]) + '-' + str(levels[3])
        toc.append('        - [' + line[4:] + '](#' + secId + ')\n')
    elif line[:2] == '##':
        levels[2] += 1
        levels[3] = 0
        secId += str(levels[1]) + '-' + str(levels[2])
        toc.append('    - [' + line[3:] + '](#' + secId + ')\n')
    elif line[:1] == '#':
        levels[1] += 1
        levels[3] = levels[2] = 0
        secId += str(levels[1])
        toc.append('- [' + line[2:] + '](#' + secId + ')\n')
    return secId

def cleanLine(text):
    text = stripNewline(text)
    text = removeAnchors(text)
    return text

def stripNewline(text):
    return text.replace('\n', '')

def removeAnchors(text):
    while ('<' in text and '>' in text):
        leftTag = text.index('<')
        rightTag = text.index('>')
        text = text[0:leftTag] + text[rightTag + 1:]
    return text

def clean(docx_file,inFile,outFile,run_pandoc=True):
    if run_pandoc:
        os.system("pandoc -t gfm --wrap=none --extract-media . -o file.md {0} --mathjax".format(docx_file))
    num_str = [str(i) for i in range(1,11)]
    lines = open(inFile,'r').readlines()

    #notoc_lines = []
    i = 0
    # while i < len(lines):
    #     line = lines[i]
    #     if "table of contents" in line.lower():
    #         while True:
    #             i += 1
    #             line = lines[i]
    #             if line.lower().strip().startswith("introduction") or line.lower().strip().startswith("# introduction") :
    #                 break
    #             #print(line)
    #     notoc_lines.append(line)
    #     i += 1
    # lines = notoc_lines
    # notoc_lines = []
    i = 0
    while i < len(lines):
        #if lines[i].strip().startswith("----"):
        #    lines[i-1] = "## " + lines[i-1]
        #    lines[i] = "\n"
        if "<img" in lines[i]:
            lines[i] = lines[i].replace("#","")
        if "<p>" in lines[i]:
            lines[i] = lines[i].replace("<p>","").replace("</p>","<br>").replace("#","~")
        if "blockquote" in lines[i]:
            lines[i] = lines[i].replace("<blockquote>","").replace("</blockquote>","")
        if lines[i].strip().startswith("$") and not "bmatrix" in lines[i].lower():
            label = lines[i].split()[-1]
            eq_str = lines[i].replace("$$","$").split('$')[1]
            eq_str = r"{0}".format(eq_str).replace("\\\\","\\").replace(" \\ "," ").replace("\\_","_")
            math_str_pre = "<img src=\"https://latex.codecogs.com/svg.latex?\Large&space;{0}".format(eq_str)
            math_str_post = "\" title=\"\Large {0}\" />  {1}  <br>".format(eq_str,label)
            lines[i] = math_str_pre + " " + math_str_post
        if lines[i].strip().startswith("<table>"): # and "pcf" in lines[i].lower():
            lines[i] = "<div style=\"text-align: left\">" + lines[i] + "</div>"
            if "comment" in lines[i].lower():
                lines[i] = lines[i].replace("~","#")
        elif lines[i].strip().startswith("[") and "]" in lines[i]:
            raw = lines[i].strip().split(']')
            rraw = " ".join(raw[0].split()[:-1])+"]"
            new_line = rraw + "".join(raw[1:]) + "\n"
            print(lines[i],new_line)
            lines[i] = new_line
        # elif "bmatrix" in lines[i].lower():
        #     eq_str = lines[i]
        #     lines.pop(i)
        #     while True:
        #         i += 1
        #         eq_str += lines[i]
        #         lines.pop(i)
        #         if "bmatrix" in lines[i].lower():
        #             break
        #     eq_str = r"{0}".format(eq_str).replace("\\\\", "\\").replace(" \\ ", " ").replace("\\_", "_")
        #     math_str_pre = "<img src=\"https://latex.codecogs.com/svg.latex?\Large&space;{0}".format(eq_str)
        #     math_str_post = "\" title=\"\Large {0}\" />".format(eq_str)
        #     lines[i] = math_str_pre + " " + math_str_post

        i += 1

    #lines[0] = "# Table of Contents\n"
    with open(outFile,'w') as f:

        for line in lines:
            #if line.lower().strip().startswith("[introduction"):
            #    f.write("\n# Table of Contents\n\n")
            f.write(line)


if __name__ == "__main__":

    clean("new_function_documentation.docx","file.md","fortran_library_documentation.md",True)
    




