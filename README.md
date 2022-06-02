# And interpreted rust lang

- This language should contains a entry function called main
- This function should support types
- This function should support function calls
- This function should support conditions and loops

- Very basic example

```
fn main() {
    var x i32 = 0;
    var y i32 = 0;

    print(x + y);
}
```

- Calling functions

```
fn main() {
    println(sum(1, 2));
}

fn sum(x, y i32) i32 {
    return x + y;
}
```

- Conditions and Loops

```
fn main() {
    for {
        println("infinty");
    }

    for var i i32 = 0; i < 10; i ++ {
        println(i);
    }

    if 1 == 1 {
        println("something");
    } else if 2 != 2 {
        println("otherwise")
    }
}
```

#### BNF

```
expression      =>  equality
equality        =>  comparision ( ("!=" | "==") comparision )*
comparision     =>  term ( (">" | "<" | ">=" | "<=") term )*
term            =>  factor ( ("-" | "+") factor )*
factor          =>  unary ( ("*" | "/") unary )*
unary           =>  ("!" | "-") unary | primary

numbers         =>  0 -> 9
identifiers     =>  A -> Z | a -> z
primary         =>  identifiers | numbers | true | false | "(" + expression + ")"

call            =>  "fn" identifiers "(" + arguments? + ")"
arguments       =>  expression ( "," expression )*
```