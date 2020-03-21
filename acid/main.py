from board import Board
from particle import Particle
from vector import Vector2D
from random import random
from particles_factory import make_particles, load_particles_from_file
import calculator
import sys


if __name__ == "__main__":
    particles_count = 100
    density = 0.81
    cut_off_distance = 1000
    width, height = calculator.get_width_height(particles_count, density)
    print(width)
    if ('old_state' in sys.argv):
        particles = load_particles_from_file(width, height, cut_off_distance, 'last_particles_state.csv')
    else:
        particles = make_particles(width, height, particles_count, cut_off_distance, velocity_mul=16)

    parameteres = {
        'width': width,
        'height': width,
    }

    board = Board(**parameteres, ticks_per_second=120, cut_off_distance=cut_off_distance, delta_time=0.001)
    board.start(particles)
