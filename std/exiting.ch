extern fn exit(i: Int).

trait ExitValue {
    fn finally(self).
}

impl ExitValue for () {
    fn finally(self) = {
        // Do nothing!
    }.
}

impl ExitValue for Int {
    fn finally(self) = {
        println("Program exited with code \(self)").
    }.
}

impl<T> ExitValue for T where T: Poll, <T as Poll>::Result: ExitValue {
    fn finally(self) = {
        self:wait():finally().
    }.
}