//
// Created by marka on 25.10.2019.
//

#ifndef MOLECULES_VECTOR2D_H
#define MOLECULES_VECTOR2D_H


#include <math.h>

class Vector2D {
public:
    long double x, y;
    Vector2D();
    Vector2D(long double x, long double y);

    Vector2D operator+(const Vector2D& other);

    Vector2D operator*(long double scalar);

    Vector2D operator-(const Vector2D& other);

    long double get_norm();
};


#endif //MOLECULES_VECTOR2D_H
