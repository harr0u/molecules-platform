import numpy as np
import numbers


class Vector2D:
    def __init__(self, x: float = 0.0, y: float = 0.0, coordinates: np.ndarray=None):
        if (coordinates is None):
            self.coordinates = np.array([x, y])
        else:
            self.coordinates = coordinates

    def __str__(self):
        return f'({self.x}, {self.y})'

    def __repr__(self):
        return str(self)

    def __add__(self, other):
        return Vector2D(coordinates=np.add(self.coordinates, other.coordinates))

    def __sub__(self, other):
        return Vector2D(coordinates=np.subtract(self.coordinates, other.coordinates))

    def __mul__(self, scalar):
        return Vector2D(coordinates=np.multiply(self.coordinates, scalar))

    def __abs__(self):
        return np.linalg.norm(self.coordinates)

    def __eq__(self, other):
        return np.array_equal(self.coordinates, other.coordinates)

    @property
    def x(self):
        return self.coordinates[0]

    @x.setter
    def x(self, x):
        if (isinstance(x, numbers.Number)):
            self.coordinates[0] = x

    @property
    def y(self):
        return self.coordinates[1]

    @y.setter
    def y(self, y):
        if (isinstance(y, numbers.Number)):
            self.coordinates[1] = y


