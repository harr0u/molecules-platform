//
// Created by marka on 25.10.2019.
//

#include "vector2d.h"

Vector2D::Vector2D(){
    x = 0.0;
    y = 0.0;
}

Vector2D::Vector2D(long double x, long double y) {
    this->x = x;
    this->y = y;
};

Vector2D Vector2D::operator+(const Vector2D& other) {
    Vector2D newVector(x + other.x, y + other.y);
    return newVector;
};

Vector2D Vector2D::operator*(long double scalar) {
    Vector2D newVector(x * scalar, y * scalar);
    return newVector;
};

Vector2D Vector2D::operator-(const Vector2D& other) {
    Vector2D newVector(x - other.x, y - other.y);
    return newVector;
};

long double Vector2D::get_norm() {
    return sqrtf(x*x + y*y);
}
