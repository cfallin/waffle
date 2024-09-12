(module
  (memory 1 1)
  (func (export "pack") (param i64 i64) (result v128)
        v128.const i64x2 0 0
        local.get 0
        i64x2.replace_lane 0
        local.get 1
        i64x2.replace_lane 1
        return)
  (func (export "unpack") (param v128) (result i64 i64)
        local.get 0
        i64x2.extract_lane 0
        local.get 0
        i64x2.extract_lane 1
        return)
  (func (export "load") (param i32) (result v128)
        local.get 0
        v128.load)
  (func (export "store") (param i32 v128)
        local.get 0
        local.get 1
        v128.store))
