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
for i in range(57,80):
    with open("test_cases/test{}".format(i), "r") as f:
        lines = f.readlines()

        with open("test_cases/test{}".format(i), "w") as out:
            pass
            '''out.write("class A {\n")
            out.write("    static function main() {\n")'''

        for line in lines:
            with open("test_cases/test{}".format(i), "a") as out:
                if "return yote" in line:
                    out.write("        return yote();\n")
                else:
                    out.write("{}".format(line))

        '''with open("test_cases/test{}".format(i), "a") as out:
            out.write("\n        return yote()")
            out.write("\n    }")
            out.write("\n}")'''
