fout = open("example.txt", 'w')
print("stuff")
# ok
fout.write("I'm writable!")
fout.close()


fout = open("example.txt", 'r')
print("stuff")
# ruleid:writing-to-file-in-read-mode
fout.write("whoops, I'm not writable!")
fout.close()


with open("example.txt", 'rb') as fout:
    # ruleid:writing-to-file-in-read-mode
    fout.write("whoops, me neither!")