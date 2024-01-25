type Foo =
    | Foo1 of int
    | Foo2 of string

let f (x: System.Nullable<Foo>) = x
