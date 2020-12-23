fn box_up<T>(t: T) -> Dyn<Iterator<Item = <T as Iterator>::Item>> where T: Iterator = {
    t:into()
}.