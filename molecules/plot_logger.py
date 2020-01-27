import matplotlib.pyplot as plt
import numpy as np
from particles_factory import ParticlesCells
import calculator


class PlotLogger:
    def __init__(self):
        self.kinetic_energy = []
        self.potential_energy = []
        self.total_energy = []

        self.center_of_mass_velocities = []

    def log_particles(self, particles: ParticlesCells):
        number = len(list(particles.iterate_throw_particles()))
        kinetic_energy = sum([abs(p.velocity)**2 / 2 for p in particles.iterate_throw_particles()]) / number
        potential_energy = sum([p.potential for p in particles.iterate_throw_particles()]) / number

        self.kinetic_energy.append(kinetic_energy)
        self.potential_energy.append(potential_energy / 2)
        self.total_energy.append(kinetic_energy + potential_energy / 2)

        self.center_of_mass_velocities.append(np.linalg.norm(calculator.calculate_center_of_mass_velocity(particles)))

    def dispose(self):
        colors = ['r', 'b', 'g']

        if self.total_energy:
            plt.subplot(1, 2, 1)

        keys = [
            'kinetic_energy',
            'potential_energy',
            'total_energy'
        ]
        for i, key in enumerate(keys):
            values = np.array(getattr(self, key))
            if len(values) == 0:
                continue
            plt.plot([0, len(values)], [np.mean(values), np.mean(values)], 'k--', linewidth=1.0)
            plt.plot([i for i, _ in enumerate(values)], values, colors[i], label=key)

        plt.legend(loc='best', shadow=True, fontsize='small')

        # Total Energy
        if self.total_energy:
            values = np.array(self.total_energy)
            mean_energy = np.mean(values)

            plt.subplot(1, 2, 2)
            plt.plot([0, len(values)], [mean_energy, mean_energy], 'k--', linewidth=1.0)
            plt.plot([i for i, _ in enumerate(values)], values, colors[2], label='Total energy',)

            relative_deviation = sum([abs(v - mean_energy) for v in values]) / len(values) / abs(mean_energy)
            print(f'Energy Relative Deviation ~> {relative_deviation * 100}')

        plt.show()
