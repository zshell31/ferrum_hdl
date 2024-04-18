use fhdl_macros::blackbox;

pub trait Unbundle {
    type Unbundled;

    #[blackbox(Unbundle)]
    fn unbundle(self) -> Self::Unbundled;
}

pub trait Bundle {
    type Bundled: Unbundle<Unbundled = Self>;

    #[blackbox(Bundle)]
    fn bundle(self) -> Self::Bundled;
}
