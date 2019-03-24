with open("p3tests.txt", "r") as f:
    lines = f.readlines()
    case = 51

    for line in lines:
        if (line.startswith("Test")):
            case = case + 1
        else:
            with open("test_cases/test{}".format(case), "a") as out:
                if (line != ''):
                    out.write("{}".format(line))
