pub const fn clog2(n: usize) -> usize {
    if n == 0 {
        return 1;
    }

    let mut c = 0;
    let mut v = 1;
    while v <= n {
        c += 1;
        v <<= 1;
    }
    c
}

pub const fn clog2_len(len: usize) -> usize {
    if len == 0 {
        0
    } else {
        clog2(len - 1)
    }
}

pub const fn min(n: usize, m: usize) -> usize {
    [n, m][(n > m) as usize]
}

pub const fn max(n: usize, m: usize) -> usize {
    [n, m][(n < m) as usize]
}

pub const fn mask(n: u128) -> u128 {
    assert!(n <= 128);
    if n == 128 {
        u128::MAX
    } else {
        (1 << n) - 1
    }
}

pub const fn max_val(n: u128) -> u128 {
    mask(n)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clog2() {
        assert_eq!(clog2(0), 1);
        assert_eq!(clog2(1), 1);
        assert_eq!(clog2(2), 2);
        assert_eq!(clog2(3), 2);
        assert_eq!(clog2(4), 3);
        assert_eq!(clog2(7), 3);
        assert_eq!(clog2(8), 4);
    }

    #[test]
    fn test_min() {
        assert_eq!(min(1, 1), 1);
        assert_eq!(min(1, 3), 1);
        assert_eq!(min(3, 1), 1);
    }

    #[test]
    fn test_max() {
        assert_eq!(max(1, 1), 1);
        assert_eq!(max(1, 3), 3);
        assert_eq!(max(3, 1), 3);
    }
}
