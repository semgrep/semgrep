def with_stmt_test():
    x = 0
    with open("some_file.txt") as file1:
        data = file1.read()

    with open("some_file.txt"):
        x += 1

    with open("some_file1.txt") as file1, open("some_file2.txt") as file2:
        data1 = file1.read()
        data2 = file2.read()

    with open("some_file1.txt"), open("some_file2.txt") as file2:
        data2 = file2.read()

    with open("some_file1.txt") as file1, open("some_file2.txt"):
        data1 = file1.read()

    with open("some_file1.txt"), open("some_file2.txt"):
        x += 1
