fn main() = {
    let answer = (0...):zip(10...):map(|(a, b)| a * b):limit(20):sum().
    let dyn_str: Dyn<Into<String>> = answer:into().
    println("The answer is \(answer))").
}.