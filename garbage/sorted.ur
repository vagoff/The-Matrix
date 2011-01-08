datatype dir = Ascending of id | Descending of id
datatype sort = Unsorted | SortByOne of dir | SortByTwo of (dir,dir)
datatype state
    = Direct of (vec lang_id * vec feature_id)
    | Transposed of (vec feature_id * vec lang_id)
