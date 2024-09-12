(module
  (type $t (func (param i32) (result i32)))
  (func $f1 (param i32) (result i32) local.get 0)
  (func $f2 (param i32) (result i32) local.get 0)
  (table $t 1 1 (ref null $t))
  (elem $t (i32.const 0) (ref null $t)
        (item (ref.func $f1))
        (item (ref.func $f2))))
