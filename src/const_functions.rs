pub const fn clog2(n: usize) -> usize {
    let mut c = 0;
    let mut v = 1;
    while v <= n {
        c += 1;
        v <<= 1;
    }
    c
}
