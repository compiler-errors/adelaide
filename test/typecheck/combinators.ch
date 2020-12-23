fn main() = {
    let answer = (0...):zip(10...):map(|(a, b)| a * b):limit(20):sum().
    println("The answer is \(answer))").
}.