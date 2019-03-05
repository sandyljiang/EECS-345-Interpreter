#include <stdio.h>

int func() {
    int x = 10;
    try {
        break;
    }
    finally {
        return x;
    }
}

int main() {
    printf("%i", func());
}