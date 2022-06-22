r""" @brief collection of processing methods for runcode.py
"""

import shutil
from os import path, chdir, listdir, getcwd, mkdir, remove
#from data_manip.computation.amp2wave import amp2wave
from utils.files import symlink_file, get_file_content,\
                        put_file_content,\
                        is_newer
from utils.messages import Messages
from utils.exceptions import TelemacException
from config import CFGS


def check_para_tilling(in_tile, in_node, in_size, ncruns, cas_ncsize):
    """
    @brief Check the consistency between number of core
           / processors and domains.

    The logic is as follows:
    > First, nctile is the one parameter we cannot modify, unless
         ncnode and ncsize are provided.
    > If ncruns > 1, then ncsize and nctile will not be adjusted,
         but:
         - if ncnode is given by the user, there will be no
              adjustment, even if the resource allocated ncnode * nctile
              might be too much or too few.
         - if ncnode is not provided, the ncnode will be adjusted to
              best fit ncsize * ncruns with a constant nctile.
    > If ncruns = 1, then normal adjustment will be done, with:
         - if ncnode is given by the user then ...
              + if ncsize is given by the user, there will be a
                    re-adjustment of nctile to accomodate
              + if nctile is given by the user, there will be a
                    re-adjustment of ncsize to accomodate

    @param in_tile (int) number of cores per node given by user
    @param in_node (int) number of nodes given by user
    @param in_size (int) total number of cores given by the script options

    @param ncruns (int) allows a number of CAS files to be placed in the same
         queue and run in parallel as a single batch
    @param cas_ncsize (int) Number of cores asked for in the steering file

    @return nctile (int) number of processors per node
    @return ncnode (int) actual number of nodes

    @note: ncsize = 0 is also supported.
    """
    # ~~ Default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ncnode = 1
    if in_node != 0:
        ncnode = max(1, in_node)
    # By default taking the input ncsize
    if in_size != -1:
        ncsize = in_size
    else:
        ncsize = cas_ncsize

    # ~~ Special case of nctile ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    nctile = 1
    if in_tile != 0:
        nctile = max(1, in_tile)
    elif ncnode > 1:
        if ncsize > 1:
            nctile = ncsize // ncnode
        elif ncruns > 1:
            nctile = ncruns // ncnode

    # ~~ Special case of batching ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if in_node == 0:
        # ~~> temporary measure before doing
        # each run in parallel of one another
        ncnode = max(1, ncsize) // nctile
        if ncnode * nctile < max(1, ncsize):
            ncnode = ncnode + 1
        # ~~> valid for runs in parallel of one another
        # ncnode = int( max( 1,ncsize ) * ncruns / nctile )
        # if ncnode * nctile < max( 1,ncsize ) * ncruns: ncnode = ncnode + 1

    if ncruns == 1:
        # ~~ Standard cases ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # If the command line options.nctile and options.ncnode are fixed
        if in_tile != 0 and in_node != 0 and ncsize == 0:
            ncsize = ncnode * nctile
    # If options.ncsize is set, it will have priority over the others
        elif in_size != 0:
            # ncnode is an integer of nctile and ncsize is re-ajusted
            if in_tile != 0:
                ncnode = max(1, ncsize) // nctile
                while ncnode * nctile < max(1, ncsize):
                    ncnode = ncnode + 1
            # nctile is an integer of ncnode and ncsize is re-ajusted
            if in_node != 0:
                nctile = max(1, ncsize) // ncnode
                while ncnode * nctile < max(1, ncsize):
                    nctile = nctile + 1
            # local processor with 1 node and many cores
            if in_tile == 0 and in_node == 0:
                ncnode = 1
                nctile = max(1, ncsize)

    return nctile, ncnode, ncsize


def hide_root(string):
    """
    Replace the path to sources of telemac by <root>

    @param string (string) The string in which to hide root

    @return (string) The updated string
    """

    return string.replace(CFGS.get_root(), "<root>")


def process_lit(cas, cas_dir, ncsize, tmp_dir, use_link):
    """
    @brief Process all the input files except FORTRAN files.

    Copy the input files in the temporary folder if necessary,
    store the ascii file names that have to be split by partel.

    @param cas (TelemacCas): Structure of the CAS file
    @param cas_dir (TelemacCas): Steering fiel directory
    @param ncsize (int): total number of processors
    @param tmp_dir (string): complete name of the temporary directory where
        the simulation results will be written
    @param use_link (boolean): only create a link in the temporary folder
        or copy the input files

    """

    # ~~> exception report
    #  xcpt will accumulate all input troubles before reporting a more
    #     comprehensive list of possible errors
    xcpt = []
    #
    #
    # ~~> loop over all (key,value) pairs of the CAS file
    #    and process those that are related to input file names
    #
    #    /!\ the FORTRAN file and its associated exe file are not included here
    #
    # Copying steering file and dictionary
    tmp_cas_name = cas.dico.data['STEERING FILE']['SUBMIT'].split(';')[1]
    print('         copying: ' + path.basename(cas.file_name) +
          ' -> '+path.join(hide_root(tmp_dir), tmp_cas_name))
    shutil.copyfile(path.join(cas_dir, cas.file_name),
                    path.join(tmp_dir, tmp_cas_name))
    tmp_dico_name = cas.dico.data['DICTIONARY']['SUBMIT'].split(';')[1]
    print('         copying: ' + path.basename(cas.dico.file_name) +
          ' -> '+path.join(hide_root(tmp_dir), tmp_dico_name))
    shutil.copyfile(cas.dico.file_name,
                    path.join(tmp_dir, tmp_dico_name))

    for key in cas.in_files:
        submit = cas.in_files[key].split(';')
        file_type = submit[5]
        if file_type[0:7] == 'FORTRAN':
            tmp_fortran_dir = path.join(tmp_dir, "user_fortran")
            if not path.exists(tmp_fortran_dir):
                mkdir(tmp_fortran_dir)
            ori_file = cas.values[key]
            # If it is a file copying it
            if path.isfile(ori_file):
                new_name = path.join(tmp_fortran_dir, path.basename(ori_file))
                shutil.copyfile(ori_file, new_name)
            # Otherwise it is a folder
            else:
                for f in listdir(ori_file):
                    # only copying file that en
                    if path.isfile(path.join(ori_file, f)):
                        if f[0] == '.' or f[-1] == '~':
                            continue
                        shutil.copyfile(path.join(ori_file, f),
                                        path.join(tmp_fortran_dir, f))
            continue
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ~~> checking presence of input files was done by TelemacCas
        file_name = cas.values[key]
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ~~> checking validity of default files
        tmp_file_name = path.join(tmp_dir, submit[1])
        #  tmp_file_name: is the default input name as set in the temporary
        #  directory
        #  if tmp_file_name does not exists then file_name is copied as
        #  tmp_file_name again
        #  if file_name is newer than tmp_file_name then file_name is copied as
        #  tmp_file_name again
        if path.exists(tmp_file_name):
            if not is_newer(tmp_file_name, file_name) == 1:
                # ~~> further check are necessary depending on file type
                if file_type[0:7] == 'SELAFIN' or \
                        file_type[0:5] == 'PARAL':
                    # ~~> check if all files are there
                    #    > while file_name is one file, tmp_file_name could
                    #    have been split already into multiple parallel files
                    found = True
                    for npsize in range(ncsize):
                        pll = tmp_file_name+'{0:05d}-{1:05d}'.format(ncsize-1,
                                                                     npsize)
                        if not path.isfile(pll):
                            found = False
                        elif not is_newer(pll, file_name) == 1:
                            found = False
                    if found:
                        #    > no partioning is required and no re-copying
                        #      either
                        cas.in_files[key] = \
                                cas.in_files[key].replace('SELAFIN', 'DONE')\
                                                 .replace('PARAL', 'DONE')
                        continue
                elif submit[0:3] == 'CAS':
                    #    > force the copying of the CAS file for some reason
                    print('     re-copying: ' + tmp_file_name)
                    put_file_content(tmp_file_name, cas.steering_file)
                    continue
                else:
                    #    > you have passed all checks
                    #      you can ignore that file
                    print('        ignoring: ' + path.basename(file_name) +
                          ' ' + hide_root(tmp_file_name))
                    continue
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ~~> files are otherwise copied (or linked)
        if use_link:
            print('         linking: ' + path.basename(file_name) +
                  ' -> '+hide_root(tmp_file_name))
            symlink_file(path.join(getcwd(), file_name), tmp_file_name)
        else:
            print('         copying: ' + path.basename(file_name) +
                  ' -> '+hide_root(tmp_file_name))
            shutil.copyfile(path.join(getcwd(), file_name), tmp_file_name)

    if xcpt != []:
        raise TelemacException(xcpt)  # raise full report
    return


def process_ecr(cas, cas_dir, sortiefile, ncsize):
    """
    @brief copies output files from the temporary folder to the CAS folder
        at the end of the simulation

    @param cas (TelemacCas): Steering file structure
    @param cas_dir (string): name of the CAS directory
    @param sortiefile (string): ascii listing of the simulation
    @param ncsize (int): total number of processors

    @return sortiefiles (list): list of output files for the CAS
    """

    xcpt = []                         # try all files for full report
    # ~~ copy output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for key in cas.out_files:
        submit = cas.out_files[key].split(';')
        tmp_file_name = submit[1]
        file_name = cas.values[key]
        if submit[5] == 'MULTI':    # POSTEL3D
            npsize = 1
            # Lookin for horizontal files POSHOR_000
            while 1:                              # HORIZONTAL SECTION FILES
                real_file_name = path.join(
                    cas_dir,
                    file_name + '_{0:03d}'.format(npsize))
                if path.isfile(real_file_name):
                    base, ext = path.splitext(real_file_name)
                    i = 0
                    # this would be an infinite loop only if you have an
                    # inifite number of files
                    while 1:
                        i = i + 1
                        if not path.isfile(base+'_old'+str(i)+ext):
                            break
                    shutil.move(real_file_name, base+'_old'+str(i)+ext)
                tmp2_file_name = tmp_file_name +\
                    '_{0:03d}'.format(npsize)
                # Temporary file not found exiting
                if not path.isfile(tmp2_file_name):
                    break
                shutil.move(tmp2_file_name, real_file_name)
                print('        moving: ' + path.basename(real_file_name))
                npsize = npsize + 1
            npsize = 1
            # Lookin for vertival files POSVER_000-000
            done = False
            while 1:                              # VERTICAL SECTION FILES
                nptime = 1
                while 1:
                    real_file_name = path.join(\
                        cas_dir,
                        file_name +
                        '_{0:03d}'.format(npsize) +
                        '-{0:03d}'.format(nptime))
                    if path.isfile(real_file_name):
                        base, ext = path.splitext(real_file_name)
                        i = 0
                        # this would be an infinite loop only if you have an
                        # inifite number of files
                        while 1:
                            i = i + 1
                            if not path.isfile(base+'_old'+str(i)+ext):
                                break
                        shutil.move(real_file_name, base+'_old'+str(i)+ext)
                    tmp2_file_name = tmp_file_name\
                        + '_{0:03d}'.format(npsize)\
                        + '-{0:03d}'.format(nptime)
                    # Temporary file not found exiting and increasing first
                    # npsize
                    if not path.isfile(tmp2_file_name):
                        # If not temporary file for first nptime we are done
                        if nptime == 1:
                            done = True
                        break
                    shutil.move(tmp2_file_name, real_file_name)
                    print('        moving: ' + path.basename(real_file_name))
                    nptime = nptime + 1
                if done:
                    break
                npsize = npsize + 1
        # MAIN MODULE
        elif submit[5] == 'PARAL' and ncsize > 1:
            npsize = 0
            c_base, c_ext = path.splitext(file_name)
            while 1:
                file_name = path.join(cas_dir,
                                      c_base
                                      + '{0:05d}-{1:05d}'
                                      .format(ncsize-1, npsize)
                                      + c_ext)
                if path.isfile(file_name):
                    base, ext = path.splitext(file_name)
                    i = 0
                    # this would be an infinite loop only if you have an
                    # inifite number of files
                    while 1:
                        i = i + 1
                        if not path.isfile(base+'_old'+str(i)+ext):
                            break
                    shutil.move(file_name, base+'_old'+str(i)+ext)
                tmp_file_name_par = tmp_file_name +\
                    '{0:05d}-{1:05d}'.format(ncsize-1, npsize)
                if not path.isfile(tmp_file_name_par):
                    break
                shutil.move(tmp_file_name_par, file_name)
                # shutil.copy2(tmp_file_name,file_name)
                print('        moving: ' + path.basename(file_name))
                npsize = npsize + 1
        elif submit[5] == 'MULTI2':
            for itmp_file_name in listdir('.'):
                if itmp_file_name.count(tmp_file_name) == 1:
                    base, ext = path.splitext(file_name)
                    new_tmp_file_name = \
                        itmp_file_name.lower()\
                        .replace(tmp_file_name.lower(), base)
                    new_file_name = path.join(cas_dir, new_tmp_file_name) + ext
                    if path.isfile(new_file_name):
                        base, ext = path.splitext(new_file_name)
                        i = 0
                        # this would be an infinite loop only if you have an
                        # inifite number of files
                        while 1:
                            i = i + 1
                            if not path.isfile(base+'_old'+str(i)+ext):
                                break
                        shutil.move(new_file_name, base+'_old'+str(i)+ext)
                    shutil.move(itmp_file_name, new_file_name)
                    print('        moving: ' + path.basename(new_file_name))
        else:
            file_name = path.join(cas_dir, file_name)
            if path.isfile(file_name):
                base, ext = path.splitext(file_name)
                i = 0
                # this would be an infinite loop only if you have an
                # inifite number of files
                while 1:
                    i = i + 1
                    if not path.isfile(base+'_old'+str(i)+ext):
                        break
                shutil.move(file_name, base+'_old'+str(i)+ext)
            if not path.isfile(tmp_file_name):
                xcpt.append({'name': 'process_ecr',
                             'msg': 'did not create outfile: ' +
                                    path.basename(file_name)+
                                    ' ('+tmp_file_name+')'})
                continue
            shutil.move(tmp_file_name, file_name)
            print('        moving: ' + path.basename(file_name))

    # ~~~ copy the sortie file(s) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sortiefiles = []
    if sortiefile is not None:
        crun = path.basename(sortiefile)
        cref = path.join(cas_dir, sortiefile)
        if not path.isfile(crun):
            xcpt.append({'name': 'process_ecr',
                         'msg': 'did not create listing file: ' +
                                path.basename(cref)+' ('+crun+')'})
            raise TelemacException(xcpt)  # raise full report
        shutil.copy(crun, cref)
        print('      copying: ' + path.basename(cref))
        sortiefiles.append(cref)

        # ~~~> If in parallel, also copy the slave log files
        # called PEnnnnn_xxxxx.log
        #    for slave x of n but for the last one called the sortie file
        if ncsize > 1:
            for i in range(ncsize-1):
                slavefile = 'PE{0:05d}-{1:05d}.LOG'.format(ncsize-1, i+1)
                base, ext = path.splitext(sortiefile)
                slogfile = base+'_p'+'{0:05d}'.format(i+1)+ext
                crun = slavefile
                cref = path.join(cas_dir, slogfile)
                if not path.isfile(crun):
                    xcpt.append({'name': 'process_ecr',
                                 'msg': 'could not find the listing file: '
                                        + crun})
                    raise TelemacException(xcpt)  # raise full report
                shutil.copy(crun, cref)
                print('      copying: ' + path.basename(cref))
                sortiefiles.append(cref)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if xcpt != []:
        raise TelemacException(xcpt)  # raise full report
    return sortiefiles


def process_artnim(cas, cas_dir):
    """
    @brief Processes the instantaneous free-surface from Artemis
        results and outputs it in a file

    process_artnim calculates the instantaneaous free-surface value from
        Artemis results (FICHIER DES PHASES ET AMPLITUDES) so as to output
        animations of the free-surface evolution. It is done by runcode.py
        so that Artemis always outputs this file. It was not done in the
        Fortran because the amount of data needed for the calculation of the
        instantaneous free-surface values is huge: for each mesh node,
        it requires the phase and amplitude values for all directions and
        period values. Storing this in a Fortran array would be too costly
        so it was decided to store it in a file instead, and read it from
        python for the processing.

    @param cas (string): name of the CAS file
    @param cas_dir (string): name of the CAS directory

    @return file_name_wfs (string): file containing the time-series of
        free-surface values for Artemis
    """

    # ~~> Output to the prost process
    value = cas.get('FREE SURFACE FILE')
    print('     +> '+value)

    file_name_wfs = path.join(cas_dir, value)
    if path.isfile(file_name_wfs):
        base, ext = path.splitext(file_name_wfs)
        i = 0
        # this would be an infinite loop only if you have an
        # inifite number of files
        while 1:
            i = i + 1
            if not path.isfile(base+'_old'+str(i)+ext):
                break
        shutil.move(file_name_wfs, base+'_old'+str(i)+ext)

    # ~~> Input to the prost process
    value = cas.get('AMPLITUDE AND PHASE FILE')
    if value != '':
        file_name_amp = path.join(cas_dir, value)
    if not path.isfile(file_name_amp):
        raise TelemacException(
            'Could not find the file of amplitudes'
            'and phases.:\n'+file_name_amp)

    # ~~> Parameters
    tfrom = 2006.07
    tfrom = cas.get('FIRST TIME IN THE FREE SURFACE FILE')
    tstep = 0.34
    tstep = cas.get('TIME STEP')
    tstop = 2108.75
    ntimestep = cas.get('NUMBER OF TIME STEPS')
    tstop = tfrom + ntimestep*tstep

    # ~~> Processing the new file
    amp2wave(file_name_amp, file_name_wfs,
             start_time=tfrom, time_step=tstep, end_time=tstop)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return file_name_wfs


def process_config(lang):
    """
    @brief Process the CONFIG file

    @param lang (int) Language to write in file (1:French, 2:English)

    @return True when file is correctly filled

    """

    # ~~ create CONFIG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    put_file_content('CONFIG', ['2' if lang == 'en' else '1', '6', ''])
    return True


def process_executable(working_dir, pbin, plib, pobj, system,
                       trace, code_name):
    """
    @brief Process the excecutable including:
        - checking the presence and the validity of the current executable
        - if necessary, copying the FORTRAN files in the temporary directory
        - if necessary, compiling the FORTRAN into the executable
        The background of this function sits where the CAS file is

    @param working_dir (string)  Working directory
    @param pbin (string) Location of the default executables
    @param plib (string) location of the associated libs (and cmdx files)
    @param pobj (string) location of the associated objs (and cmdo files)
    @param system (dict) Configuration info
    @param trace TODO
    @param code_name (string) Name of the module

    @return exe_fort: name of the executable whether copied or compiled and a

    @note even in case of coupling, the principal executable remains
    @note possible fortran files may or may not be associated with the
        principal code and may be files or directories
    @note If the executable exist, and that the user fortran files have not
        changed and that the system has not been recompiled,
        then the executable remains valid
    @note The name of the executable is taken to be based on the name defined
        by the user, i.e. the name of the PRINCI whether it is a file or a
        directory.
    @note
        - exe_file: The name of the default executable as well as the system
          preference for that file extension
        - ori_file: The user define name of the executable,
          based on the name of
          the FORTRAN FILE whether a file or a directory.
        - use_name,obj_name,f90_name,obj_cmd,exe_cmd
    """
    # ~~ exception error, if any
    mes = Messages(size=10)
    # ~~ saving current location
    curdir = getcwd()
    # ~~ default executable (no user defined fortran file(s)
    exe_file = path.join(pbin, code_name+system['sfx_exe'])
    if not path.exists(exe_file):
        raise TelemacException(
            '\nNot able to find your default executable: ' +
            exe_file + '\n' +
            '\n ... you have to compile this module at least: ' +
            code_name)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~> All user fortran have been copied in user_fortran
    #
    ori_fort = []
    tmp_fortran_dir = path.join(working_dir, "user_fortran")
    if path.exists(tmp_fortran_dir):
        # Sort them to force order of compilation
        for f in sorted(listdir(tmp_fortran_dir)):
            # If we found a compiled file removing it
            if f.endswith(system['sfx_obj']):
                remove(path.join(tmp_fortran_dir, f))
            else:
                ori_fort.append(path.join(tmp_fortran_dir, f))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~> Getting name of executable in working dir
    #
    # ~~ case of user fortran
    if ori_fort != []:
        # wir_fort working sub-directory,
        # locally contain all user fortran files
        use_file = 'out_user_fortran'+system['sfx_exe']
    #
    # ~~ without user fortran
    else:
        # ~~ default executable
        use_file = 'out_' + code_name + system['sfx_exe']
    exe_fort = path.join(working_dir, use_file)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # copy the default executable
    if ori_fort == []:
        shutil.copy2(exe_file, exe_fort)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else:
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ~~> Compiling fortran file(s)
        # ~~ default command line for compilation of obects
        cmdo_file = path.join(pobj, code_name+'.cmdo')
        if not path.exists(cmdo_file):
            raise TelemacException(
                '\nNot able to find your OBJECT command line: ' +
                cmdo_file + '\n' +
                '\n ... you have to compile this module at least: ' +
                code_name)
        obj_cmd = get_file_content(cmdo_file)[0]
        # ~~ make the keys portable (no full path)
        for k in trace:
            obj_cmd = obj_cmd.replace('['+k+']', path.normpath(trace[k]))
        # ~~ into local compilation
        chdir(tmp_fortran_dir)
        print('  > compiling objs')
        # ~~ compilation one file at a time
        objs = []
        # Reordonning user fortran to compile module first
        # Are considered modules files beginning the letter m
        user_files = []
        tmp = []
        for f90 in ori_fort:
            # /!\ TODO avoid using hard-coded explicit assumptions
            #             use get_principal_wrap_names(f90)[0], or better,
            # scan the local user tree
            if f90.lower()[0] == "m":
                user_files.append(f90)
            else:
                tmp.append(f90)
        user_files.extend(tmp)
        # Looping on ordered fortran files
        for f90 in user_files:
            obj_name = path.splitext(f90)[0]+system['sfx_obj']
            print('         compiling: '+path.basename(f90), end='')
            tail, code = mes.run_cmd(obj_cmd.replace('<f95name>', f90)\
                                            .replace('<objname>', obj_name),
                                     False)
            if code != 0:
                raise TelemacException(
                    'Could not compile your FORTRAN (runcode=' +
                    str(code)+').\n        '+tail)
            print(' ... completed')
            objs.append(obj_name)
        # ~~ default command line for linkage into an executable
        cmdx_file = path.join(plib, code_name+'.cmdx')
        if not path.exists(cmdx_file):
            raise TelemacException(
                '\nNot able to find your EXECUTE command line: '
                + cmdx_file + '\n'
                + '\n ... you have to compile this module '
                'at least: ' + code_name)
        exe_cmd = get_file_content(cmdx_file)[0]
        # ~~ make the keys portable (no full path)
        for k in trace:
            exe_cmd = exe_cmd.replace('['+k+']', path.normpath(trace[k]))
        exe_cmd = exe_cmd.replace('<objs>', ' '.join(objs))\
            .replace('<exename>', '"'+exe_fort+'"')
        tail, code = mes.run_cmd(exe_cmd, False)
        if code != 0:
            raise TelemacException(
                'Could not link your executable (runcode=' +
                str(code)+').\n        '+tail)
        print('         created: '+path.basename(exe_fort))

        # ~~ out of local compilation
        chdir(curdir)

    return exe_fort


def print_twice(pipe, ofile, last_line):

    """
    @brief Prints the listing data to both stdout and to the listing file

    @param pipe (string): the path of the pipe
    @param ofile (string): name of the output file
    @param last_line (string): name of the *.sortie listing file
    """

    # Utility subroutine to print listing data both to stdout
    # and to the listing file, accessed via the ofile handle
    lastlineempty = False        # JPG addition here as opposed to argument
    last_dat = b''
    for line in iter(pipe.readline, b''):
        dat = line.rstrip()
        # This IF statement just avoid printing a lot of blank lines
        # at the end of the run, before Python realises that the process
        # has stopped.
        if dat == b'':
            if not lastlineempty:
                print(dat.decode('utf-8'))
                if ofile is not None:
                    # Write to sortiefile (if requested)
                    ofile.write(dat.decode('utf-8')+'\n')
                # Set to avoid printing multiple consecutive newlines
                lastlineempty = True
        else:
            lastlineempty = False
            print(dat.decode('utf-8'))
            if ofile is not None:
                # Write to sortiefile (if requested)
                ofile.write(dat.decode('utf-8')+'\n')
            last_dat = dat

    last_line.append(last_dat)
