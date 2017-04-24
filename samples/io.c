#include <stdio.h>
#include <stdint.h>

int32_t input() {
    int val = 0;
    printf("READ: ");
    scanf("%d", &val);
    return val;
}

int32_t output(int32_t val) {
    printf("WRITE: %d\n", val);
    return 0;
}
