"""
Contains function to execute one or more steering files
"""
from os import path, sep
from execution.study import Study
from utils.exceptions import TelemacException


def run_local_cas(my_study, options):
    """
    Run a single steering file

    @param my_study (Study) Structure of the study
    @param options (Values) Options of runcode
    """
    full_run = not (options.split or options.merge or
                    options.compileonly or options.run)

    print('\n... checking parallelisation')
    my_study.set_ncsize(options.ncsize, options.ncnode, options.nctile)
    if my_study.cfg['MPI'] != {}:
        my_study.build_mpi_cmd(options.hosts)
    my_study.set_sortie(options.sortie_file, options.merge)

    if options.split or full_run:
        # split part
        print('\n... handling temporary directories')
        my_study.create_working_dir()
        my_study.fill_working_dir(use_link=options.use_link)
        my_study.generate_mpi_files()
        my_study.partionning(options.use_link)
        if options.split:
            print('\n\n'+'~'*72+'\n')
            print('... Your simulation is almost ready for launch. '
                  'You need to compile your executable with the option '
                  '-x (--compileonly)\n')
            return

    if options.compileonly or full_run:
        # compile part
        print('\n... checking the executable')
        my_study.compile_exe()
        if options.compileonly:
            print('\n\n'+'~'*72+'\n')
            print(
                '... Your simulation is ready for launch and you can now :\n')
            print('     +> re-run without option -x (--compileonly) '
                  'or with option --run\n')
            if my_study.cfg['MPI'] == {}:
                print('     +> or run the following command within '
                      'each local subdirectory:')
                work_dir = my_study.working_dir
                exe_name = path.basename(my_study.exe_name)
                print('         -> in <{}> run with EXE:'
                      '\n                      {}'
                      .format(work_dir+sep, exe_name))
            else:
                print('     +> or run with MPI: ')
                print('                      '+my_study.run_cmd)
            return

    # run part
    if options.run or full_run:
        print('\n\nRunning your simulation(s) :\n'+'~'*72+'\n')
        my_study.run(options)
        if options.run:
            print('\n\n'+'~'*72+'\n')
            print('... Your simulation has been completed but you need to '
                  're-collect files using the option --merge\n')
            return

    # merge part
    if options.merge or full_run:
        print('... merging separated result files\n')
        my_study.merge(options.gretel_method)
        print('... handling result files\n')
        my_study.gather(options.sortie_file, options.nozip)

        if options.tmpdirectory:
            print('... deleting working dir\n')
            my_study.delete_working_dir()

    del my_study


def run_hpc_cas(my_study, options):
    """
    Running a case throught the hpc job scheduler (hpc_cluster)

    @param my_study (Study) Structure of the study
    @param options (Values) Options of runcode
    """

    # Just relaunch the whole script in job scheduler
    if 'PYCODE' in my_study.cfg['HPC']:
        print('\n... checking parallelisation')
        my_study.set_ncsize(options.ncsize, options.ncnode, options.nctile)
        my_study.build_mpi_cmd(options.hosts)
        my_study.set_sortie(options.sortie_file, options.merge)

        my_study.create_working_dir()

        my_study.run(options)

    # Just launch the execution of homere in job scheduler
    elif 'EXCODE' in my_study.cfg['HPC']:
        full_run = not (options.split or options.merge or
                        options.compileonly or options.run)
        print('\n... checking parallelisation')
        my_study.set_ncsize(options.ncsize, options.ncnode, options.nctile)
        my_study.build_mpi_cmd(options.hosts)
        my_study.set_sortie(options.sortie_file, options.merge)

        if options.split or full_run:
            my_study.create_working_dir()
            my_study.fill_working_dir(use_link=options.use_link)
            my_study.generate_mpi_files()
            my_study.partionning(options.use_link)

        if options.compileonly or full_run:
            print('\n... checking the executable')
            my_study.compile_exe()

        if options.run or full_run:
            my_study.run(options)

        if options.merge:
            print('... merging separated result files\n')
            my_study.merge(options.gretel_method)
            print('... handling result files\n')
            my_study.gather(options.sortie_file, options.nozip)

            if options.tmpdirectory:
                print('... deleting working dir\n')
                my_study.delete_working_dir()

    else:
        raise TelemacException(
             "HPC job with undefined configuration missing" +
             "hpc_runcode or hpc_pytel in your configuration file")

    del my_study


def run_study(steering_file, code_name, options):
    """
    Run a TELEMAC-MASCARET steering file

    @param steering_file (string) Name of the steering file to run
    @param code_name (string) Name of the module used
    @param options (Values) options of runcode.py
    """
    print('\n... processing the steering file')

    if code_name == "mascaret":
        my_study = Study(
            steering_file,
            code_name,
            options.w_dir,
            mascaret_cas=options.mascaret_cas)

    else:
        my_study = Study(
            steering_file,
            code_name,
            options.w_dir)

    if my_study.cfg['HPC'] == {} or options.mpi:
        run_local_cas(my_study, options)
    else:
        run_hpc_cas(my_study, options)

    return my_study.sortie_file
