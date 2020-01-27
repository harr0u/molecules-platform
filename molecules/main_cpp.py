from particle import Particle
from vector import Vector2D
from pygame_painter import PygamePainter
from plot_logger import PlotLogger
import pygame
import sys


if __name__ == "__main__" and "simple" not in sys.argv:
    filename = f'/Users/marka/Documents/molecules/molecules_cpp/data_detailed.csv'
    frames_per_iteration = 30

    with open(filename, 'r') as file:
        data = list([s.strip() for s in file.readlines()])
        particles_side_count, density, delta_time, width = tuple(float(c) for c in data[0].split())
        particles_number = int(particles_side_count) ** 2

        frames = []
        for i in range(0, (len(data) - 1) // particles_number, frames_per_iteration):
            raw_particles = data[i * particles_number + 1 : (i+1) * particles_number + 1]
            particles = []
            for raw_particle in raw_particles:
                frame_id, particle_id, cx, cy, p, kin = tuple(float(c) for c in raw_particle.split())
                particle = Particle(Vector2D(cx, cy), 0.5, None)
                particle.potential = p
                particle.kinetic_energy = kin

                if particle_id == 100:
                    particle.color = (255, 55, 55)
                elif particle_id == 437:
                    particle.color = (55, 255, 55)
                elif particle_id == 321:
                    particle.color = (55, 55, 255)
                particles.append(particle)

            frames.append(particles)

        screen_width = 800
        screen = pygame.display.set_mode((screen_width, screen_width))

        scale = screen_width / width
        painter = PygamePainter(screen, scale)
        logger = PlotLogger()

        clock = pygame.time.Clock()

        condition = True
        for particles in frames:
            if not condition:
                break
            clock.tick(120)
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    condition = False

            potential = 0.0
            kinetic_energy = 0.0
            for particle in particles:
                potential += particle.potential
                kinetic_energy += particle.kinetic_energy

            logger.kinetic_energy.append(kinetic_energy / particles_number)
            logger.potential_energy.append(potential / particles_number)
            logger.total_energy.append((kinetic_energy + potential) / particles_number)
            painter.draw_particles_(particles)

            pygame.display.update()

        logger.dispose()


if __name__ == '__main__' and 'simple' in sys.argv:
    logger = PlotLogger()
    filename = f'/Users/marka/Documents/molecules/molecules_cpp/data.csv'
    with open(filename, 'r') as f:
        data = list([s.strip().split() for s in f.readlines()])

        logger.kinetic_energy = [float(line[0]) for line in data]
        logger.potential_energy = [float(line[1]) for line in data]
        logger.total_energy = [float(line[2]) for line in data]
        logger.dispose()
