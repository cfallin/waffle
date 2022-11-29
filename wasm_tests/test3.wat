(module
  (func (param i32) (result i32)
        (local $i i32)
        (local.set $i (local.get 0))

        (local.get $i)
        (block $loop-break (param i32) (result i32)
            (loop $l (param i32) (result i32)
                  (if (i32.eq (local.get $i) (i32.const 100))
                    (br $loop-break
                      (local.get $i)))
                  (local.set $i (i32.add (i32.const 10) (local.get $i)))
                  (br $l (local.get $i))))))
