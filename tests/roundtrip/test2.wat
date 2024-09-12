(module
  (func (param i32) (result i32)
        local.get 0
        if (result i32)
          i32.const 1
          local.get 0
          i32.add
        else
          i32.const 2
          local.get 0
          i32.add
        end))
