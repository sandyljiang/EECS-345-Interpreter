'''for i in range(1, 52):
    with open("test_cases/test{}".format(i), "r") as f:
        lines = f.readlines()

        with open("test_cases/test{}".format(i), "w") as out:
            out.write("function main() {\n")

        for line in lines:
            with open("test_cases/test{}".format(i), "a") as out:
                out.write("    {}".format(line))

        with open("test_cases/test{}".format(i), "a") as out:
            out.write("\n}")'''

# update for part 4
for i in range(1,2):
    with open("test_cases/test{}".format(i), "r") as f:
        lines = f.readlines()

        with open("test_cases/test{}".format(i), "w") as out:
            out.write("class A {\n")

        for line in lines:
            with open("test_cases/test{}".format(i), "a") as out:
                out.write("    {}".format(line))

        with open("test_cases/test{}".format(i), "a") as out:
            out.write("\n}")
