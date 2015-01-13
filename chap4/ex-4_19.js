/*
 * (let ((a 1))
 *   (define (f x)
 *     (define b (+ a x))
 *     (define a 5)
 *     (+ a b))
 * (f 10))
 * 
 * */

(function() {
  var a = 1;
  function f(x) {
    var b = a + x, // a is hosted in this scope. So a === undefined
        a = 5;

    return a + b; // NaN + 5
  }
  console.log(f(10)); // NaN
})();