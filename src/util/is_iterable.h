#ifndef IS_ITERABLE_H
#define IS_ITERABLE_H

#include<iterator>
#include<type_traits>

/*
   Defines an is_iterable() macro which (in theory) you can use to determine
   if a given type is iterable.

   usage:
   if constexpr(is_iterable(some_type)) {
        ... 
   }

   I don't know why there's no standard/built in way to do this.  Maybe there
   is one.  Whatever.  this works well enough.  Moving on.
 */

template <typename T, typename = int>
struct _is_iterable
    : std::false_type
{};

// uhh let's say it's iterable if it has a "begin" method
// (or, sloppily, anything called "begin")
// or actually apparently... gah I hate c++.
// just try to make something work at all.  std::begin.
// whatevs.  shipit.
// checking std::begin allows this to work with arrays.
template <typename T>
struct _is_iterable <T, decltype(std::begin(std::declval<T&>()), 0)>
    : std::true_type
{};

#define is_iterable(type) (_is_iterable<type>::value)

#endif // IS_ITERABLE_H
