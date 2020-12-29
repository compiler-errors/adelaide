fn main() = {
    let x: Vector<_> = (0..10):flat_map(|i| repeated(i, i + 1)):collect().
    println("\(x)").
}.