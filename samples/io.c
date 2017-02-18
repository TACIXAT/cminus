#include <stdio.h>
#include <stdint.h>

int32_t read() {
    int val = 0;
    printf("READ: ");
    scanf("%d", &val);
    return val;
}

void write(int32_t val) {
    printf("WRITE: %d\n", val);
}
