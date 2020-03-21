import numpy as np
from vector import Vector2D

class Particle:
    last_particle_id = 0

    def __init__(self, center: Vector2D, radius: float, velocity: Vector2D = None, color=None):
        Particle.last_particle_id += 1
        self.id = Particle.last_particle_id

        self.radius = radius
        self.center = center

        if (velocity is None):
            velocity = Vector2D()
        self.velocity = velocity
        self.color = color

        self.acceleration = Vector2D()
        self.potential = 0.0
        self.mass = 1

    def translate(self, delta_radius: Vector2D):
        self.center = self.center + delta_radius

    def __str__(self):
        return ';'.join(
            [str(x) for x in [
                self.id,
                self.center.x,
                self.center.y,
                self.velocity.x,
                self.velocity.y,
                self.acceleration.x,
                self.acceleration.y,
                self.potential
            ]]
        )

    def string2particle(str):
        args = str.split(';')
        return Particle(
            #int(args[0]),
            center=Vector2D(
                float(args[1]),
                float(args[2]),
            ),
            radius=0.5,
            velocity=Vector2D(
                float(args[3]),
                float(args[4]),
            )
            # float(args[5]),
            # float(args[6]),
            # float(args[7]),
        )