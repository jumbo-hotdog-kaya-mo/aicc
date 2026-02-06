an actual programming language for ai2 since dragging blocks is a pile of dogshit

-----

# The language

## `aicc` command

### Usage

```
./aicc [-p] srcfile outfile
```

Compiles source `srcfile` into xml or if `-p` is specified, output into a bunch of pngs.

### Notes

**You most likely want to use `-p` to use outputs for app inventor, it will become the default in a future version**
  
If `-p` is specified, `outfile` becomes a prefix for generated output pngs (`outfile0.png`, `outfile1.png`, ...)

## Basic reference:

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
    #(red, green, blue, alpha)
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

## Builtin functions

### Generic

- `append(obj1, obj2)`
- `contains(haystack, needle)`
- `copy(obj)`
- `find(haystack, needle)`
- `len(obj)`
- `remove(obj, prop)`
- `reverse(obj)`

### Numerical

- `abs(num)`
- `arccos(num)`
- `arcsin(num)`
- `arctan(num)`
- `atan2(y, x)`
- `ceil(num)`
- `cos(num)`
- `exp(num)`
- `floor(num)`
- `log(num)`
- `round(num)`
- `sin(num)`
- `sqrt(num)`
- `tan(num)`

### Strings

- `contains_all(text, substrings)`
- `lexcmp(text1, [eq | ne | lt | gt], text2)`
- `replace(text, search, replacement)`
- `replace_multi(text, mappings, [max_munch | ordered])`
- `segment(text, start, length)`
- `split(text, separator)`
- `split_once(text, separator)`
- `to_lower(text)`
- `to_upper(text)`
- `trim(text)`

### Arrays

- `filter(arr, var, func)`
- `join(arr, sep)`
- `lookup_pairs(arr, key, default)`
- `map(arr, var, func)`
- `max(arr)`
- `min(arr)`
- `reduce(arr, init, acc, cur, func)`
- `slice(arr, index1, index2)`
- `sort(arr)` | `sort(arr, item, func)` | `sort(arr, item1, item2, func)`

### Dictionaries

- `keys(dict)`
- `path_all()`

> NOTE: when using `path_get` or `path_walk`, ensure array indices are 1-indexed

- `path_get(dict, keys, default)`
- `path_walk(dict, keys)`
- `to_dict(arr)`
- `to_pairs(dict)`
- `values(dict)`
