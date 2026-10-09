
"""
Validation script for riv_art
"""
import numpy as np
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac2d']
        self.walltime = '17:00:00'

    def _pre(self):
        """
        Defining the studies
        """

        # riv_art scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_riv_art.cas')


        # riv_art parallel mode
        cas = TelemacCas('t2d_riv_art.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_riv_art_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_riv_art.slf',
                            eps=[1.E-7, 1.E-8, 1.E-7, 3.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-10, 1.E-10, 1.E-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_riv_art.slf',
                            eps=[2.E-2, 9.E-3, 2.E-4, 2.E-4, 1.E-8, 2.E-8, 2.E-8, 1.E-10, 1.E-10, 1.E-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[2.E-2, 9.E-3, 2.E-4, 2.E-4, 1.E-8, 2.E-8, 2.E-8, 1.E-10, 1.E-10, 1.E-10])


    def _post(self):
        """
        Post-treatment processes
        """
        import matplotlib.pyplot as plt
        import matplotlib.dates as mdates

        from data_manip.computation.datetimes import compute_datetimes
        from postel.plot2d import plot2d_triangle_mesh
        from postel.plot1d import plot1d

        res_file = self.get_study_file('vnv_1:T2DRES')
        bnd_file = self.get_study_file('vnv_1:T2DCLI')
        res_seq = TelemacFile(res_file, bnd_file=bnd_file)

        # plotting mesh
        fig, ax = plt.subplots(1, 1, figsize=(8, 2))
        ax.set_aspect('equal')
        plot2d_triangle_mesh(ax, res_seq.tri,
                             x_label='X (m)', y_label='Y (m)',
                             color='k', linewidth=0.1)
        ax.set_title('2D mesh (%d triangles, %d nodes)' \
                     % (res_seq.nelem2, res_seq.npoin2))
        ax.plot(17, 4.5, 'ro', label='HAP probe')
        plt.legend()
        print(" "*8+"~> img/Mesh.pdf")
        plt.savefig('img/Mesh.pdf')
        plt.close('all')

        # Getting concentration versus experiment
        datetimes = res_seq.times
        points = [[17.0, 4.5]]
        data = res_seq.get_timeseries_on_points('VARIABLE 23', points)
        data_exp1 = np.loadtxt('doc/kero_exp1.txt', skiprows=0)
        data_exp2 = np.loadtxt('doc/kero_exp2.txt', skiprows=0)
        fig, ax = plt.subplots(figsize=(10, 5))

        # for each plot adding a history plot with a label node_(node_number)
        for i, _ in enumerate(points):
            plot1d(ax, datetimes, data[i, :],
                   x_label='time (s)',
                   y_label='concentration (kg/m$^3$)',
                   plot_label='simulation')
        ax.plot(data_exp1[:, 0], data_exp1[:, 1]*10**(-9), label='experiment 1')
        ax.plot(data_exp2[:, 0], data_exp2[:, 1]*10**(-9), label='experiment 2')
        # Displaying legend
        ax.legend()
        print(" "*8+"~> img/conc_over_time.pdf")
        plt.savefig('img/conc_over_time.pdf')
        plt.close('all')
