fn main() -> Dyn<ExitValue> = {
    if cond() {
        ():into()
    } else {
        1:into()
    }
}.

fn cond() -> Bool = todo().