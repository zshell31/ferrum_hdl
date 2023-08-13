use strum::EnumString;

#[derive(Debug, Clone, Copy, EnumString, PartialEq, Eq)]
pub enum BlackboxKind {
    Bit,
    Clock,
    Register,
    RegisterFn,
}

// trait IntoSynType<const KIND: BlackboxKind> {
//     type SynType: Synthesizable;
// }

// impl IntoSynType<{ BlackboxKind::Bit }> for () {
//     type SynType = Bit;
// }

// impl IntoSynType<{ BlackboxKind::Clock }> for () {
//     type SynType = ClockTy;
// }

// impl IntoSynType<{ BlackboxKind::Register }> for () {
//     type SynType = RegisterTy;
// }

// type SynType<const K: BlackboxKind> = <() as IntoSynType<{ K }>>::SynType;

// impl BlackboxKind {
//     pub fn synthesize_lit(self, lit: &Lit) -> Option<Expression> {
//         match self {
//             Self::Bit => SynType::<{ Self::Bit }>::synthesize_lit(lit),
//             // Self::Clock => SynType::<{ Self::Clock }>::synthesize_lit(lit),
//             // Self::Register => SynType::<{ Self::Register }>::synthesize_lit(lit),
//             Self::Clock => None,
//             Self::Register => None,
//         }
//     }

//     pub fn synthesize_param(self, kind: ParameterKind, ident: Ident) -> Parameter {
//         let width = match self {
//             Self::Bit => SynType::<{ Self::Bit }>::width(),
//             // Self::Clock => Some(SynType::<{ Self::Clock }>::width()),
//             // Self::Register => Some(SynType::<{ Self::Register }>::width()),
//             Self::Clock => 1,
//             Self::Register => 1,
//         };

//         Parameter { ident, kind, width }
//     }
// }
