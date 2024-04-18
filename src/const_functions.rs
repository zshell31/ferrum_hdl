pub use fhdl_const_func::*;

pub const fn idx_range_len(n: usize, m: usize) -> usize {
    assert!(m > 0);
    assert!(m <= n);
    n + 1 - m
}

pub const fn assert_in_range(n: usize, start: usize, len: usize) -> usize {
    assert!(start + len <= n);
    1
}

pub const fn assert_extend(n: usize, m: usize) -> usize {
    assert!(n < m);
    1
}

pub const fn bit_width(start: usize, end: usize) -> usize {
    let (start, end) = if start <= end {
        (start, end)
    } else {
        (end, start)
    };
    end - start + 1
}

pub const fn bit_width_off(start: usize, offset: usize, dir: bool) -> usize {
    assert!(offset > 0);
    if dir {
        offset
    } else {
        assert!(start + 1 >= offset);
        offset
    }
}

pub const fn bit_start_off(start: usize, offset: usize, dir: bool) -> usize {
    assert!(offset > 0);
    if dir {
        start
    } else {
        assert!(start + 1 >= offset);
        start + 1 - offset
    }
}
