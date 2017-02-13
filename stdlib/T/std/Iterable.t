package T::std

import T::std::Iterator

trait Iterable<T> {

    /**
    * Returns an iterator over this iterable instance.
    */
    Def Iterator(): Iterator<T>

}