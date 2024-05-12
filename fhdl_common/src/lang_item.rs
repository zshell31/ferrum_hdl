use strum::{Display, EnumString};

#[derive(Display, Debug, Clone, Copy, EnumString, PartialEq, Eq, Hash)]
pub enum LangItem {
    Module,
    ModLogic,
    Domain,
    DomFreq,
    DomRstKind,
    DomRstPol,
}
