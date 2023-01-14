
import subprocess.open
import subprocess.{open => sub_open}

def foo = {
    //ERROR: match
    result = subprocess.open("ls")

    //ERROR: match
    result = sub_open("ls")

}