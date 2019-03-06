try:
    print("hello\n")
    try:
        print("inner try\n")
        x = 1/0
    finally:
        print("inner finally\n")
except:
    print("catch")
finally:
    print("finally out\n")