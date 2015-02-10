def isSorted[A](as: Array[A], ordered: (A,A) => Boolean, indx: Int = 0): Boolean = {
    if (indx < as.length - 1) 
        ordered(as(indx), as(indx + 1)) &&  isSorted(as, ordered, indx + 1) 
    else true
}
