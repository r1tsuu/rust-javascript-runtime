### RSX - Rust Javascript Execution Engine

Very simple JS Engine with basic expressions support:

1. Binary expressions like `1+(3+5)*3*(3+10)`
2. Variables and changing variables:
```js
let x = 10;
let b = 30 + 1;
b = b + 5;
x + b // evaluates to JSNumber { value: 46 }
```
3. Strings
```js
let x = "Hello World"
x //  evaluates to JSString  { value: "Hello World" }
```
4. Block scoping
```js
// its own scope
{
  let x = 1;
}
```
5. Functions
```js
function x(a, b) { 
  return a + b;
}

x(2, 3); // evaluates to 5
```

6. Objects
```js
let obj = {a: 3, b: "Hello World"}; // evaluates to JSObject { value: HashMap /w "a" - 3 as JSNumber and "b" - "Hello World" as JSString }
```