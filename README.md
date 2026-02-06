an actual programming language for ai2 since dragging blocks is a pile of dogshit

-----

# The language

### Basic reference:

### Literals
- boolean

    ```
    true
    false
    ```

- string

    ```
    "this is a string"
    "escape \" for double quotes"
    'escape \' for single quotes'
    ```

- number

    ```
    12
    -1
    0xFF
    0b10011010
    067
    ```

- color

    ```
    #FFFFFF
    #(0xFF, 255, 0b11111111)
    ```

- array

    ```
    [1, "2", [3]]
    ```

- dictionary

    ```
    {"hello": "world", 12: 34}
    ```


### Variables

```
global global_var = "skibidi";
/*
 * to use global_var, always prefix it with "global "
 * eg. replace(global global_var, "skibidi", "gyatdula")
 */

let width = 1,
    height = 2 in {
    $Label:MyLabel.Width = width;
    $Label:MyLabel.Height = height;
};

$Label:MyLabel.Text = let text = "hello" in {
    replace(text, "lo", "p me") // <--- no semicolon, this is the return value
};
```

### Control flow
- `if` statement

    ```
    if cond1 {
        /* cond1 is true */
    } else if cond2 {
        /* cond2 is true */
    } else {
        /* otherwise, ... */
    }
    ```

- `while` loop

    ```
    while cond {
        /* runs while cond is true */
    }
    ```

- `for` loop
    - range-for loop

        ```
        for i from 1 to 5 by 2 {
            /* i goes from 1 to 5, incrementing by 2 every step */
        }
        ```

    - iterative loop

        ```
        for i in my_list {
            /* iterate over members of my_list */
        }
        ```

    - dictionary-based iterative loop

        ```
        for key, value in my_dict {
            /* iterate over keys and values of my_dict */
        }
        ```

### Event handler

```
when $Button:MyButton.Click {
    /* clicked */
}
```

### Functions

```
/* "func"tions must return a value */
func foo() {
    69 // <--- return value, no semicolon
}

/* "proc"edures don't return a value */
proc bar(baz) {
    do_stuff(baz + 1);
}
```
