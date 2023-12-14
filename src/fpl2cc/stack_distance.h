#ifndef STACK_DISTANCE_H
#define STACK_DISTANCE_H

/*

Class for 

I've made this to simplify dealing with melding related
distances.

The general arithmetic rules for these are:

  - Distances are always positive.  If you subtract
    2 non-indeterminate distances, you will get a 
    positive distance.
  - Distances may be "indeterminate".  Operations
    involving one or more indeterminate distances
    result in an indeterminate distance.

 */

class stack_distance {
    int dist;

public:
    static const int INDETERMINATE = -1;

    stack_distance() : dist(0) { }
    stack_distance(int d) : dist(d) { }

    static stack_distance indeterminate() {
        return stack_distance(INDETERMINATE);
    }

    bool is_indeterminate() const {
        return dist < 0;
    }

    int to_int() const { return dist; }


    // resets the distance to 0 (and makes this freely  assignable)
    void reset() {
        dist = 0;
    }

    stack_distance &operator=(const stack_distance& b) {
        // 0 length is used to mean not-yet-assigned,
        // so we can always assign to or from 0 without
        // conflict
        if(dist == 0) {
            dist = b.dist;
        } else if(b.dist == 0) {
            // ... if b is not-yet-assigned, though,
            // this is a no-op.
        } else if(b.dist != dist) {
            // disagreeing distances means indeterminate:
            dist = INDETERMINATE;
        } else {
            dist = b.dist;
        }
        return *this;
    }

    stack_distance &operator+=(const stack_distance &b) {
        // we -can- increment assign without conflict though
        if(b.is_indeterminate()) {
            dist = INDETERMINATE;
        } else if(!is_indeterminate()) {
            dist += b.dist;
        }
        return *this;
    }

    // negating an already indeterminate distance
    // yields an indeterminate distance, so this
    // is always indeterminate:
    stack_distance operator-() const { return indeterminate(); }

    stack_distance operator+(const stack_distance &b) const {
        if(b.is_indeterminate()) return indeterminate();
        if(is_indeterminate())   return indeterminate();
        return stack_distance(dist + b.dist);
    }

    stack_distance operator-(const stack_distance &b) const {
        if(b.is_indeterminate()) return indeterminate();
        if(is_indeterminate())   return indeterminate();
        // distances are always 0 or positive:
        if(dist >=  b.dist)
            return stack_distance(dist - b.dist);
        else
            return stack_distance(b.dist - dist);
    }

    stack_distance operator*(const int &b) const {
        if(is_indeterminate()) return indeterminate();
        // similar to subtraction, we always get a positive
        // value here:
        if(b >= 0)
            return stack_distance(dist * b);
        else
            return stack_distance(dist * -b);
    }

    stack_distance operator/(const int &b) const {
        if(is_indeterminate()) return indeterminate();
        if(b <= 0)             return indeterminate();
        return stack_distance(dist / b);
    }

    stack_distance operator%(const int &b) const {
        if(is_indeterminate()) return indeterminate();
        if(b <= 0)             return indeterminate();
        return stack_distance(dist % b);
    }

    // next 2 are pre[in/de]crement:
    stack_distance &operator++() {
        if(!is_indeterminate()) dist++;
        return *this;
    }

    // predecrement.
    // as with subtraction, this never results in a negative
    // distance - 0 is the floor.
    stack_distance &operator--() {
        if(!is_indeterminate()) dist--;
        if(dist < 0) dist = 0;
        return *this;
    }

    // postincrement:
    stack_distance operator++(int) {
        int old = dist;
        if(!is_indeterminate()) dist++;
        return old;
    }

    // postdecrement:  once again, 0 is the shortest possible distance.
    stack_distance operator--(int) {
        int old = dist;
        if(!is_indeterminate()) dist--;
        if(dist < 0) dist = 0;
        return old;
    }

    std::string to_str() const {
        if(is_indeterminate())
            return "␀";

        return stringformat("{}", dist);
    }
};


#endif // STACK_DISTANCE_H
