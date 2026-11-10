
"""
Validation script for estimation
"""
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

    def _pre(self):
        """
        Defining the studies
        """
        # estimation scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_estimation_basic.cas')

        # estimation scalar mode
        cas = TelemacCas('t2d_estimation_basic.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_estimation_basic_par.cas',
                       cas=cas)
        del cas

        # estimation scalar mode
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_estimation.cas')


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_estimation.slf',
                            eps=[1e-2, 1e-2, 1e-2, 1e-2, 1e-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        vnv_3_t2dres = self.get_study_file('vnv_3:T2DRES')
        res_vnv_3_t2dres = TelemacFile(vnv_3_t2dres)
        vnv_3_t2dgeo = self.get_study_file('vnv_3:T2DGEO')
        res_vnv_3_t2dgeo = TelemacFile(vnv_3_t2dgeo)

        # Plotting FREE SURFACE over polyline over records range(0, ntimestep)
        vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_vnv_3_t2dres,
                poly=[[0, 50], [500, 50]],
                record=[i for i in range(0, res_vnv_3_t2dres.ntimestep)],
                fig_size=(12, 5),
                fig_name='img/FreeSurfaceProfiles')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_3_t2dgeo,
                   plot_mesh=True,
                   fig_size=(20, 5),
                   fig_name='img/Mesh')

        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_3_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(20, 5),
                   fig_name='img/FreeSurfacetf')

        # Plotting FREE SURFACE at 5
        vnv_plot2d('FREE SURFACE',
                   res_vnv_3_t2dres,
                   record=5,
                   filled_contours=True,
                   fig_size=(20, 5),
                   fig_name='img/FreeSurfacet5')

        # Closing files
        res_vnv_3_t2dres.close()
        res_vnv_3_t2dgeo.close()
