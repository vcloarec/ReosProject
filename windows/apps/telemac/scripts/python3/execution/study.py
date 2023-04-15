"""
File containing the Study class that is used to run a steering file
"""
from os import path, mkdir, sep, chdir, getcwd
from time import localtime, strftime

from config import CFGS

from execution.telemac_cas import TelemacCas
from execution.process import process_lit, process_config, \
                              process_executable, check_para_tilling, \
                              process_ecr, process_artnim
from execution.get import get_mpi_cmd, get_glogeo, get_partel_cmd, \
                          get_hpc_cmd, get_hpc_depend, get_gretel_cmd, \
                          get_partitionner
from execution.run import run_partition, run_code, run_recollection
#from execution.mascaret_cas import MascaretCas
#from execution.run_mascaret import create_mascaret_files
from utils.files import put_file_content, get_file_content, zipsortie, \
                        remove_directories
from utils.exceptions import TelemacException
#from postel.parser_output import get_latest_output_files


class StudyException(TelemacException):
    """
    Exception from Study class
    """
    def __init__(self, study, message):
        """
        init function

        @param study (Study) Study object for which there was and error
        @param message (String) The error message
        """
        string = "\n{} Study of {}:\n".format(
            study.code_name,
            study.steering_file)
        super().__init__(string+message)


class Study():
    """
    Define a study it can them be split, compiled, run, merge
    """

    def __init__(self, steering_file, code_name, working_dir, mascaret_cas=False):
        """
        Init function

        @param steering_file (string) Name of the steering file to run
        @param code_name (string) Name of the module used
        @param working_dir (string) If not empty will be the name of the
                                         working directory
        """
        if not path.exists(steering_file):
            raise TelemacException(
                "Could not find your steering file :\n{}".format(steering_file))
        self.steering_file = steering_file
        self.case_dir = path.dirname(path.realpath(self.steering_file))
        self.working_dir = ''
        self.code_name = code_name
        self.sortie_file = ''
        self.exe_name = ''
        self.run_cmd = ''
        self.mpi_cmd = ''
        self.par_cmd = ''
        self.cpl_cases = {}

        # Getting configuration information
        self.cfgname = CFGS.cfgname
        self.cfg = CFGS.configs[CFGS.cfgname]

        # Special behaviour for mascaret that does not need the treatment below
        if self.code_name == 'mascaret':
            # Mascaret
            self.working_dir = self.case_dir
            self.bin_path = path.join(self.cfg['root'], 'builds',
                                      self.cfgname, 'bin')
            ext = self.cfg['SYSTEM']['sfx_exe']
            self.dico_file = path.join(
                                   self.cfg['MODULES'][self.code_name]['path'],
                                   self.code_name+'.dico')
            self.exe_name = path.join(self.bin_path, self.code_name+ext)
            self.run_cmd = self.exe_name
            # mascaret only runs in seuqential
            self.ncsize = 1
            pwd = getcwd()
            chdir(self.case_dir)

            if mascaret_cas:
                if not path.exists(self.dico_file):
                    raise StudyException(self,
                            'Could not find the dictionary file: {}'
                            .format(self.dico_file))

                self.cas = MascaretCas(self.steering_file,
                                       dico_file=self.dico_file)

            else:
                self.cas = MascaretCas(self.steering_file,
                                           check_files=False)

            # Mascaret : conversion at the execution of the .cas file into
            #            .xcas file
            if self.cas.file_type == 'cas':
                self.cas.xcas_filename = \
                    path.join(path.dirname(self.cas.cas_filename),
                    self.cas.cas_filename.split(sep)[-1].split('.')[0]+'.xcas')
                cas_filename = self.cas.cas_filename
                self.cas.write_xcas_file()
                self.steering_file = self.cas.xcas_filename
                self.cas = MascaretCas(self.steering_file,
                                           dico_file=self.dico_file,
                                           check_files=False)
                self.cas.cas_filename = cas_filename

            chdir(pwd)
            return

        # Searching for the dictionary associated with the steering case
        self.dico_file = path.join(self.cfg['MODULES'][self.code_name]['path'],
                                   self.code_name+'.dico')
        if not path.exists(self.dico_file):
            raise StudyException(self,
                                 'Could not find the dictionary file: {}'
                                 .format(self.dico_file))

        # ~~> processing steegin file
        self.cas = TelemacCas(self.steering_file, self.dico_file)

        # parsing informations for coupled modules steering files
        cplages = self.cas.get('COUPLING WITH', '').split(',')

        self.ncnode = 1
        self.nctile = 1
        self.ncsize = self.cas.get('PARALLEL PROCESSORS', default=1)

        self.lang = self.cas.lang


        # /!\ having done the loop this way it will not check for DELWAQ
        cpl_codes = []
        for cplage in cplages:
            for mod in self.cfg['MODULES']:
                if mod in cplage.lower():
                    cpl_codes.append(mod)

        for code in cpl_codes:
            # ~~~~ Extract the CAS File name ~~~~~~~~~~~~~~~~~~~~~~~
            cas_name_cpl = self.cas.get(code.upper()+' STEERING FILE')
            cas_name_cpl = path.join(self.case_dir, cas_name_cpl)

            if not path.isfile(cas_name_cpl):
                raise StudyException(self,
                                     'Missing coupling steering file for '
                                     + code + ': ' +
                                     cas_name_cpl)

            # ~~ Read the coupled CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~
            dico_file_plage = path.join(self.cfg['MODULES'][code]['path'],
                                        code+'.dico')
            cas_plage = TelemacCas(cas_name_cpl, dico_file_plage)

            self.cpl_cases[code] = cas_plage

        # ~~> structural assumptions
        self.bin_path = path.join(self.cfg['root'], 'builds',
                                  self.cfgname, 'bin')
        self.obj_path = self.cfg['MODULES'][self.code_name]['path'].replace(
            path.join(self.cfg['root'], 'sources'),
            path.join(self.cfg['root'], 'builds', self.cfgname, 'obj'))
        self.lib_path = path.join(self.cfg['root'], 'builds',
                                  self.cfgname, 'lib')

        self.set_working_dir(working_dir)
        self.set_exe()

    def set_working_dir(self, working_dir_name=''):
        """
        Set the working directory for the study by default:
        steering_cas_YYYY-MM-HHh-MMm-SSs/

        @param working_dir_name (string) If not empty will be the name of the
                                         working directory
        """
        # ~~> default temporary directory name
        # /!\ includes date/time in the name
        tmp_dir = self.case_dir+sep +\
            path.basename(self.steering_file) + '_' + \
            strftime("%Y-%m-%d-%Hh%Mmin%Ss", localtime())
        wdir = tmp_dir
        self.working_dir = wdir
        self.sortie_file = wdir
        # ~~> user defined directory name
        if working_dir_name != '':
            wdir = path.join(self.case_dir, working_dir_name)
            self.working_dir = wdir

    def create_working_dir(self):
        """
        Creates the working_dir for the study
        """
        # ~~> dealing with the temporary directory
        if not path.exists(self.working_dir):
            mkdir(self.working_dir)

    def copy_files(self, dir_path, verbose=False, copy_cas_file=True):
        """
        Will copy all input files into a directory

        @param dir_path (str) Directory in which to copy the files
        @param verbose (bool) If True print info for each copy
        @param copy_cas_file (bool) If True copies the steerings files as well
        """
        if not path.exists(dir_path):
            raise StudyException(self, "Copy dir does not exists:\n"+dir_path)

        self.cas.copy_cas_files(dir_path, verbose=verbose,
                                copy_cas_file=copy_cas_file)

        for cpl_cas in self.cpl_cases.values():
            cpl_cas.copy_cas_files(dir_path, verbose=verbose,
                                   copy_cas_file=True)

    def set_ncsize(self, ncsize, ncnode, nctile):
        """
        Overwrite the number of parallel processor in the steering file

        @param ncsize (int) The total number of cores
        @param nctile (int): number of cores per node given by user
        @param ncnode (int): number of nodes given by user
        """
        if self.code_name == 'mascaret':
            self.nctile = self.ncnode = self.ncsize
            return

        self.nctile, self.ncnode, ncsize = \
            check_para_tilling(nctile, ncnode,
                               ncsize, 1, self.ncsize)
        if self.cfg['MPI'] != {}:
            ncsize = max(1, ncsize)
        elif ncsize > 1:
            raise StudyException(
                self,
                '\nParallel inconsistency: '
                '\n     +> you may be using an inappropriate configuration: '
                + self.cfgname +
                '\n     +> or may be wishing for scalar mode while setting to '
                + str(ncsize)+' processors')
        if self.cfg['MPI'] == {}:
            ncsize = 0
        # ~~ Forces keyword if parallel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # /!\ in case of multiple CAS files, you have to have the same ncsize
        self.cas.set('PARALLEL PROCESSORS', ncsize)
        # Adding in coupled cas file as well
        for code in self.cpl_cases:
            self.cpl_cases[code].set('PARALLEL PROCESSORS', ncsize)
        self.ncsize = ncsize

    def fill_working_dir(self, use_link=False):
        """
        Filling temporary folder copying files

        @param use_link (boolean) If True making link instead of copy
        """
        # >>> Placing yourself where the CAS File is
        chdir(self.case_dir)
        if self.code_name == 'mascaret':
            # Creating files missing for execution
            create_mascaret_files(self.cfg, path.basename(self.steering_file))
            return
        # >>> Copy INPUT files into wdir
        process_lit(
            self.cas,
            self.case_dir,
            self.ncsize,
            self.working_dir,
            use_link)
        # Adding section name to CAS file information as the coupled
        # module might have sections and zones as well

        for cas_cpl in self.cpl_cases.values():
            process_lit(
                cas_cpl,
                self.case_dir,
                self.ncsize,
                self.working_dir,
                use_link)
        # >>> Placing yourself into the wdir
        chdir(self.working_dir)
        # >>> Creating LNG file
        process_config(self.lang)

    def build_mpi_cmd(self, hosts):
        """
        Set the mpi command string

        @param hosts (string) Name of the host on which to run the mpi command
        """
        # ~~> MPI host file provided through the command line
        if hosts != '':
            if 'HOSTS' in self.cfg['MPI']:
                self.cfg['MPI']['HOSTS'] = hosts.replace(':', ' ')
            else:
                self.cfg['MPI'].update({'HOSTS': hosts.replace(':', ' ')})
        # ~~> MPI Command line and options ( except <exename> )
        # /!\ cfg['MPI'] is also modified
        mpicmd = get_mpi_cmd(self.cfg['MPI'])\
            .replace('<root>', self.cfg['root'])
        # mpi_exec supports: -n <ncsize> -wdir <wdir> <exename>
        mpicmd = mpicmd.replace('<ncsize>', str(self.ncsize))
        # >>> Parallel execution configuration
        mpi = mpicmd
        # ~~> filling in the blanks
        mpi = mpi.replace('<wdir>', self.working_dir)
        self.mpi_cmd = mpi

    def set_exe(self):
        """
        Set the name of the executable of the study
        """
        ext = self.cfg['SYSTEM']['sfx_exe']
        user_fortran = self.cas.get("FORTRAN FILE")
        # If we have a user_fortran
        if user_fortran != '':
            use_file = 'out_user_fortran'+ext
            exe_name = path.join(self.working_dir, use_file)
        else:
            exe_file = path.join(self.bin_path, self.code_name+ext)
            use_file = 'out_' + path.basename(exe_file)
            exe_name = path.join(self.working_dir, use_file)

        self.exe_name = exe_name
        self.run_cmd = self.exe_name

    def compile_exe(self):
        """
        Compile the executable
        """
        if self.code_name == 'mascaret':
            # Nothing to do
            return
        # >>> Placing yourself in the temporary folder
        chdir(self.working_dir)

        self.exe_name = process_executable(
            self.working_dir,
            self.bin_path, self.lib_path,
            self.obj_path, self.cfg['SYSTEM'],
            self.cfg['TRACE'], self.code_name)

    def generate_mpi_files(self):
        """
        Generate the PARAL and HOSTFILE files need by telemac-mascaret
        """

        if self.cfg['MPI'] != {}:
            # ~~> MPI host file ( may be re-written by the HPC INFILE script )
            hostfile = self.cfg['MPI']['HOSTFILE']
            hosts = []
            n = 0
            while n < self.ncsize:
                for i in self.cfg['MPI']['HOSTS'].split():
                    hosts.append(i)
                    n += 1
                    if n == self.ncsize:
                        break
            chdir(self.working_dir)
            # ~~> Creating the HOST file
            put_file_content(hostfile, hosts)
            # ~~> Creating the PARA file
            put_file_content('PARAL',
                             [str(self.ncsize),
                              str(len(self.working_dir+sep)),
                              self.working_dir+sep, ''])

    def partionning(self, use_link):
        """
        Running partionning of the input files
        """
        # No partitionning to do

        if self.ncsize <= 1:
            return
        chdir(self.working_dir)
        # ~~> Path
        bin_path = path.join(self.cfg['root'], 'builds', self.cfgname, 'bin')
        parcmd = get_partel_cmd('"' +bin_path+ '"', self.cfg, self.mpi_cmd)
        # >>> Add running command
        self.par_cmd = parcmd

        # ~~> Run PARTEL for the base files
        # Global GEO file
        g_geo, g_fmt_geo, g_conlim = get_glogeo(self.cas)
        # Setting section, zone, weirs file (needed by partel) but only
        # available in telemac2d
        if self.code_name == 'telemac2d':
            # section file
            if 'SECTIONS INPUT FILE' in self.cas.in_files:
                submit = self.cas.in_files['SECTIONS INPUT FILE']
                section_name = submit.split(';')[1]
            else:
                section_name = ''
            # Zone file
            if 'ZONES FILE' in self.cas.in_files:
                submit = self.cas.in_files['ZONES FILE']
                zone_name = submit.split(';')[1]
            else:
                zone_name = ''
            # Weirs are only passed to partel if type of weirs == 2
            if 'WEIRS DATA FILE' in self.cas.in_files and \
               self.cas.values.get('TYPE OF WEIRS', 0) == 2:
                submit = self.cas.in_files['WEIRS DATA FILE']
                weir_name = submit.split(';')[1]
            else:
                weir_name = ''
        else:
            section_name = ''
            zone_name = ''
            weir_name = ''
        # Identify the partitioner to use for Partel
        i_part = get_partitionner(self.cas.get('PARTITIONING TOOL'))
        # Are we gonna concatenate the output of partel or not ?
        concat = self.cas.get('CONCATENATE PARTEL OUTPUT')
        s_concat = 'YES' if concat else 'NO'

        # ~~> Run partitioning/duplication for all input files
        run_partition(parcmd, self.cas, g_geo, g_fmt_geo, g_conlim,
                      self.ncsize, section_name, zone_name, weir_name,
                      use_link, i_part, s_concat)

        # Same actions for coupled steering files
        for cas_cpl in self.cpl_cases.values():
            g_geo, g_fmt_geo, g_conlim = get_glogeo(cas_cpl)
            run_partition(parcmd, cas_cpl, g_geo, g_fmt_geo,
                          g_conlim,
                          self.ncsize, '', '',
                          '', use_link, i_part, s_concat)

    def set_sortie(self, sortie_file, merge):
        """
        Defining name of 'sortie' files
        """
        if not sortie_file:
            self.sortie_file = None
        else:
            if merge:
                # try re-using existing/latest sortie file with same root name
                output_dir = path.join(self.working_dir,
                                       path.basename(self.steering_file))
                sortie_file = get_latest_output_files(output_dir)[0]
                self.sortie_file = path.basename(sortie_file)
            else:
                # define the filename (basename) of the sortie file
                self.sortie_file = path.basename(self.sortie_file)+'.sortie'

    def run_local(self):
        """
        Local run of the study (sequential or parallel)
        """
        chdir(self.working_dir)
        print('\n\nIn {}:\n{}\n\n'.format(self.working_dir, self.run_cmd))
        # ~~> here you go run
        run_code(self.run_cmd, self.sortie_file)

    def run_hpc_exe(self, options, job_id=''):
        """
        Run only the execution of telemac-mascaret executable in job scheduler

        @return job_id (integer) Id of the job that was launched
        """
        # /!\ This is being done in parallel when multiple cas_files
        # if not hpcpass:
        chdir(self.working_dir)
        # ~~> HPC Command line launching runcode
        hpccmd = get_hpc_cmd(self.cfg['HPC']).replace('<root>',
                                                      self.cfg['root'])
        hpccmd = hpccmd.replace('<wdir>', self.working_dir)
        if 'id_log' in options:
            hpccmd = hpccmd.replace('<id_log>', options.id_log)
        else:
            hpccmd = hpccmd.replace('<id_log>', 'id.log')

        hpccmd = hpccmd.replace('<project>', options.project)

        # ~~> HPC dependency between jobs
        hpcjob = get_hpc_depend(self.cfg['HPC'])
        if hpcjob != '' and job_id != '':
            hpccmd = hpccmd + ' ' + hpcjob.replace('<jobid>', job_id)
        # ~~> HPC queueing script
        stdin_file = self.cfg['HPC']['STDIN'][0]    # only one key for now
        stdin = self.cfg['HPC']['STDIN'][1]
        stdin, sortie = self.fill_hpc_stdin(stdin, options)
        # working from working dir
        stdin = stdin.replace('<wdir>', self.working_dir)
        # ~~> Recreate the <mpi_exec> (option --hpc)
        stdin = stdin.replace('<exename>', self.run_cmd)
        # /!\ serial mode
        stdin = stdin.replace('<mpi_cmdexec>', self.run_cmd)

        # ~~> Write to HPC_STDIN
        chdir(self.working_dir)
        put_file_content(stdin_file, stdin.split('\n'))

        # ~~> here you go run
        run_code(hpccmd, sortie)

        job_id = get_file_content(sortie)[0].strip()
        print('... Your simulation ('+self.steering_file +
              ') has been launched through the queue.\n')
        print('   +> You need to wait for completion before re-collecting'
              'files using the option --merge\n')

        return job_id

    def fill_hpc_stdin(self, stdin, options):
        """
        Replacing tags in file with the one given in options

        @param stdin (string) The content of HPC_STDIN
        @param options (Values) Options of the script runcode.py

        @return (stdin, sortie) The update content of HPC_STDIN and sortie
        """
        if self.cfg['MPI'] != {}:
            stdin = stdin.replace('<hosts>', self.cfg['MPI']['HOSTS'])
        stdin = stdin.replace('<root>', self.cfg['root'])
        stdin = stdin.replace('<configName>', self.cfgname)
        stdin = stdin.replace('<ncsize>', str(self.ncsize))
        stdin = stdin.replace('<nctile>', str(self.nctile))
        stdin = stdin.replace('<ncnode>', str(self.ncnode))
        stdin = stdin.replace('<email>', options.email)
        stdin = stdin.replace('<jobname>', options.jobname[:40])
        time = strftime("%Y-%m-%d-%Hh%Mmin%Ss", localtime())
        stdin = stdin.replace('<time>', time)
        stdin = stdin.replace('<queue>', options.hpc_queue)
        stdin = stdin.replace('<walltime>', options.walltime)
        stdin = stdin.replace('<codename>', self.code_name)
        stdin = stdin.replace('\n ', '\n')
        stdin = stdin.replace('<wdir>', self.case_dir)

        sortie = 'hpc-job.sortie'
        if options.sortie_file:
            sortie = self.sortie_file
        stdin = stdin.replace('<sortiefile>', sortie)

        return stdin, sortie

    def run_hpc_full(self, options, job_id=''):
        """
        Rerun whole script in jobscheduler

        @return job_id (integer) Id of the job that was launched
        """
        chdir(self.working_dir)
        # ~~> HPC Command line launching runcode
        hpccmd = get_hpc_cmd(self.cfg['HPC']).replace('<root>',
                                                      self.cfg['root'])
        hpccmd = hpccmd.replace('<wdir>', self.working_dir)
        if 'id_log' in options:
            hpccmd = hpccmd.replace('<id_log>', options.id_log)
        else:
            hpccmd = hpccmd.replace('<id_log>', 'id.log')

        hpccmd = hpccmd.replace('<project>', options.project)

        # ~~> HPC dependency between jobs
        hpcjob = get_hpc_depend(self.cfg['HPC'])
        if hpcjob != '' and job_id != '':
            hpccmd = hpccmd + ' ' + hpcjob.replace('<jobid>', job_id)

        # ~~> HPC queueing script
        stdin_file = self.cfg['HPC']['STDIN'][0]    # only one key for now

        stdin = self.cfg['HPC']['STDIN'][1]
        stdin = stdin.replace('<exename>', self.steering_file)
        # Replacing tags by options values
        stdin, sortie = self.fill_hpc_stdin(stdin, options)

        # Building runcode.py command
        runcmd = 'runcode.py ' + self.code_name + ' --mpi '
        if options.config_name != '':
            runcmd = runcmd + ' -c ' + options.config_name
        if options.config_file != '':
            runcmd = runcmd + ' -f ' + options.config_file
        if options.root_dir != '':
            runcmd = runcmd + ' -r ' + options.root_dir
        runcmd = runcmd + ' -s '
        if not options.tmpdirectory:
            runcmd = runcmd + ' -t '
        runcmd = runcmd + ' -w ' + self.working_dir
        runcmd = runcmd + ' --nctile ' + str(self.nctile)
        runcmd = runcmd + ' --ncnode ' + str(self.ncnode)
        runcmd = runcmd + ' --ncsize ' + str(self.ncsize)
        if options.split:
            runcmd = runcmd + ' --split '
        if options.compileonly:
            runcmd = runcmd + ' -x '
        if options.merge:
            runcmd = runcmd + ' --merge '
        if options.run:
            runcmd = runcmd + ' --run '
        if options.gretel_method > 1:
            runcmd = runcmd + ' --gretel-method ' + str(options.gretel_method)
        runcmd = runcmd + ' ' + self.steering_file
        stdin = stdin.replace('<py_runcode>', runcmd)

        # ~~> Write to HPC_STDIN
        chdir(self.working_dir)
        put_file_content(stdin_file, stdin.split('\n'))

        # ~~> here you go run
        run_code(hpccmd, sortie)

        job_id = get_file_content(sortie)[0].strip()
        print('... Your simulation ('+self.steering_file +
              ') has been launched through the queue.\n')
        print('    +> You need to wait for completion '
              'before checking on results.\n')

        return job_id

    def run(self, options):
        """
        Running the study

        @param options (Values) options of runcode.py
        """
        # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        # ~~ Running the Executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # You need to do this if ...
        #     - options.split is out already
        #     - options.compileonly is out already
        #     - if options.run, obvisouly this is the main run
        #        of the executable
        # Inputs ...
        #     - runcmd if options.hpc
        #     - cas_files[name]['run'] and cas_files[name]['sortie'] otherwise
        # update mpi command if necessary
        if self.cfg['MPI'] != {} or options.mpi:
            self.run_cmd = self.mpi_cmd.replace('<exename>', self.exe_name)
        if self.cfg['HPC'] == {} or options.mpi:
            self.run_local()

        # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        # ~~ Handling the HPC before running ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # You need to do this if ...
        #     - if options.run, obvisouly this is the main executable to run
        # Inputs ...
        #     - ncsize, nctilem ncnode, wdir, casdir, options, code_name
        #     - cfg['HPC']['STDIN'] and cfg['MPI']['HOSTS']
        #     - cas_files.values()[0]['sortie'] and
        #       cas_files.values()[0]['exe']
        #     - cas_files[name]['run']
        # Outputs ...
        #     > runcmd and put_file_content(stdinfile,)
        elif 'STDIN' not in self.cfg['HPC']:
            raise StudyException(
                self,
                '\nI would need the key hpc_stdin in you '
                'configuration so I can launch your simulation '
                'on the HPC queue.')
        elif 'EXCODE' in self.cfg['HPC']:
            self.run_hpc_exe(options)
        elif 'PYCODE' in self.cfg['HPC']:
            self.run_hpc_full(options)

    def merge(self, method):
        """
        Run gretel on file that need it

        @param method (int) the method to be used for merging data"
        """
        # No merging to do
        if self.ncsize <= 1:
            return
        # ~~> Path
        bin_path = path.join(self.cfg['root'], 'builds', self.cfgname, 'bin')
        execmd = get_gretel_cmd('"' + bin_path + '"', self.cfg)\
            .replace('<root>', self.cfg['root'])
        # ~~> Run GRETEL
        chdir(self.working_dir)
        # Global GEO file
        cas = self.cas
        g_geo, g_fmt_geo, g_bnd = get_glogeo(cas)
        run_recollection(
            execmd, cas, g_geo, g_fmt_geo, g_bnd,
            self.ncsize, method)

        # Running it for coupled steering files
        for cas_cpl in self.cpl_cases.values():
            g_geo, g_fmt_geo, g_bnd = get_glogeo(cas_cpl)
            run_recollection(
                execmd, cas_cpl, g_geo, g_fmt_geo, g_bnd,
                self.ncsize, method)

    def gather(self, sortie_file, nozip):
        """
        Gather back output files

        @param sortie_file (boolean) If True copying log from working_dir
        @param nozip (boolean) If False log files are zipped together
        """
        if self.code_name == 'mascaret':
            # Nothing to do
            return
        sortiefiles = []
        chdir(self.working_dir)
        # ~~> copying all primary result files
        cas = self.cas
        files = process_ecr(cas,
                            self.case_dir,
                            self.sortie_file,
                            self.ncsize)
        if sortie_file:
            sortiefiles.extend(files)
        # ~~> copying all coupled result files
        for cas_cpl in self.cpl_cases.values():
            files = process_ecr(cas_cpl,
                                self.case_dir, None,
                                self.ncsize)
            if sortie_file:
                sortiefiles.extend(files)
        # ~~> zipping sortie files if necessary
        if not nozip and self.ncsize > 1 and sortie_file:
            zipsortie(sortiefiles[0])

        # ~~> post-processing the ARTEMIS animation file if necessary
        if self.code_name == 'artemis':
            value = self.cas.get('FREE SURFACE FILE')
            if value.strip() != '':
                files = process_artnim(cas, self.case_dir)

    def delete_working_dir(self):
        """
        Delete the working dir
        """
        if self.code_name == 'mascaret':
            # No working dir
            return
        chdir(self.case_dir)
        remove_directories(self.working_dir)
