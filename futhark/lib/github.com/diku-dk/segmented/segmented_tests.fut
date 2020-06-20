-- | ignore

import "segmented"

-- ==
-- entry: test_segmented_scan
-- input { [true,false,false,true,false,false,true,false,false,false] [1,2,3,4,5,6,7,8,9,10] }
-- output { [1,3,6,4,9,15,7,15,24,34] }
-- input { [true] [1] }
-- output { [1] }
-- input { empty([0]bool) empty([0]i32) }
-- output { empty([0]i32) }

entry test_segmented_scan (flags: []bool) (as: []i32) =
  segmented_scan (+) 0 flags as

-- ==
-- entry: test_segmented_reduce
-- input { [true,false,false,true,false,false,true,false,false,false] [1,2,3,4,5,6,7,8,9,10] }
-- output { [6,15,34] }
-- input { [true] [1] }
-- output { [1] }

entry test_segmented_reduce (flags: []bool) (as: []i32) =
  segmented_reduce (+) 0 flags as

-- ==
-- entry: test_replicated_iota
-- input { [2,3,1] } output { [0,0,1,1,1,2] }
-- input { [3] } output { [0,0,0] }
-- input { [2,0,1] } output { [0,0,2] }
-- input { empty([0]i32) } output { empty([0]i32) }
-- input { [0] } output { empty([0]i32) }
-- input { [0,0] } output { empty([0]i32) }

entry test_replicated_iota (repl:[]i32) : []i32 =
  replicated_iota repl

-- ==
-- entry: test_segmented_iota
-- input { [false,false,false,true,false,false,false] }
-- output { [0,1,2,0,1,2,3] }
-- input { [false] } output { [0] }
-- input { [true] } output { [0] }
-- input { empty([0]bool) } output { empty([0]i32) }

entry test_segmented_iota (flags:[]bool) : []i32 =
  segmented_iota flags

-- ==
-- entry: test_expand
-- input { [2,3,1] }
-- output { [0,2,0,3,6,0] }

entry test_expand (arr:[]i32) : []i32 =
  expand (\ x -> x) (\x i -> x*i) arr

-- ==
-- entry: test_expand_reduce
-- input { [2,0,3,1] }
-- output { [2,9,0] }

entry test_expand_reduce (arr:[]i32) : []i32 =
  expand_reduce (\ x -> x) (\x i -> x*i) (+) 0 arr

-- ==
-- entry: test_expand_outer_reduce
-- input { [2,0,3,1] }
-- output { [2,0,9,0] }

entry test_expand_outer_reduce (arr:[]i32) : []i32 =
  expand_outer_reduce (\ x -> x) (\x i -> x*i) (+) 0 arr
