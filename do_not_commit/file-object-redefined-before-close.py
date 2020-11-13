def test1():
    # ruleid:file-object-redefined-before-close
    fin = open("file1.txt", 'r')
    data = fin.read()
    fin = open("file2.txt", 'r')
    data2 = fin.read()
    fin.close()

def test2():
    #ok
    fin = open("file1.txt", 'r')
    data = fin.read()
    fin.close()
    
    fin = open("file2.txt", 'r')
    data2 = fin.read()
    fin.close()