(module
  (func $a (param i32) (param i32) (result i32)
  (local $diff i32)
  (local.set $diff (i32.const 1))
  (if (i32.eqz (local.get 0)) (return (local.get 1)))
  (return_call $a (i32.sub (local.get 0) (local.get $diff)) (i32.add (local.get 1) (local.get $diff)))
        ))
