/// This program prints out the first 10 fibonacci sequences

function fibonacci(i) {
    if (i <= 1) {
        return i;
    } else {
        return fibonacci(i - 1) + fibonacci(i - 2);
    }
}

function main() {
    let i = 0;
    let b = 10;
    while (i < b) {
        print fibonacci(i);
        i += 1;
    }
}