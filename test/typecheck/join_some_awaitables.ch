async fn foo() -> Int = {
    Vector::new_from([async { 1 }, async{ 2 }, async{ 3 }])
        :join()
        :await
        :iterator()
        :map(|x| x + x)
        :limit(2)
        :sum()
}.