(module
  (func (export "f") (param i32) (result i32)
    (block $c (result i32)
      (block $b (result i32)
        (block $a (result i32)
          local.get 0
          i32.const 1
          i32.add
          local.get 0
          br_table $a $b $c)
        i32.const 5
        i32.mul)
      i32.const 10
      i32.add
      return)
    i32.const 20
    i32.add)
  (func (export "g") (param i32) (result i32)
    (local i32)
    local.get 0
    (loop $l (param i32) (result i32)
      local.tee 1
      i32.const 1
      i32.sub
      local.get 1
      br_if $l
      drop
      local.get 1)))
