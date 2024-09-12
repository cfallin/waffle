(module
  (type $t (func (param i32 i32) (result i32)))

  (table $tab 10 10 (ref null $t))
  (table $tab2 10 10 (ref null $t))

  (elem (table $tab2) (i32.const 0) (ref null $t) (ref.func $f))

  (func $callit (param i32 i32 i32) (result i32)
        (call_ref $t (local.get 1)
                     (local.get 2)
                     (table.get $tab (local.get 0))))

  (func $setit (param i32 (ref null $t))
        (table.set $tab (local.get 0) (local.get 1)))

  (func $getf (result (ref null $t))
        (ref.func $f))

  (func $f (param i32 i32) (result i32)
        local.get 0))
