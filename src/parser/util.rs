pub fn append<T>(mut t: Vec<T>, e: T) -> Vec<T> {
    t.push(e);
    t
}

pub fn append_maybe<T>(mut t: Vec<T>, e: Option<T>) -> Vec<T> {
    if let Some(e) = e {
        t.push(e);
    }
    t
}
