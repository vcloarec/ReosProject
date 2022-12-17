r""" @brief collection of get methods for runcode.py
"""

from os import path


def get_gretel_cmd(pbin, cfg):
    """
    @brief Returns the command to execute GRETEL and its arguments

    @param pbin (string): path to partel executable from root
    @param cfg (string):  configuration file information

    @return execmd (string): the text string that will be executed
     on the system
    """
    # ~~> Temporary variable to keep pbin unchanged
    par_dir = pbin
    if cfg['PARTEL'] != {}:
        if 'PATH' in cfg['PARTEL']:
            par_dir = cfg['PARTEL']['PATH'].replace('<root>', cfg['root'])\
                                                    .replace('<config>', pbin)
    # ~~> GRETEL Executable
    execmd = path.join(par_dir, 'gretel'+cfg['SYSTEM']['sfx_exe'])

    return execmd


def get_partel_cmd(pbin, cfg, mpi_cmd):
    """
    @brief Returns the command to execute PARTEL and its arguments

    @param pbin (string): path to to partel executable from root
    @param cfg (string): configuration file information
    @param mpi_cmd (string): Command to execute in parallel

    @return execmd (string): the text string that will be executed
        on the system
    """
    # ~~> Temporary variable to keep pbin unchanged
    par_dir = pbin
    if cfg['PARTEL'] != {}:
        if 'PATH' in cfg['PARTEL']:
            par_dir = cfg['PARTEL']['PATH'].replace('<root>', cfg['root'])\
                                                    .replace('<config>', pbin)
    # ~~> Default call to PARTEL
    execmd = path.join(pbin, 'partel'+cfg['SYSTEM']['sfx_exe'] +
                             ' < <partel.par> >> <partel.log>')
    # ~~> User defined call to PARTEL
    if cfg['PARTEL'] != {}:
        if 'EXEC' in cfg['PARTEL']:
            execmd = cfg['PARTEL']['EXEC']
    # ~~> Replacement of known keys
    # <mpi_cmdexec> and <exename> should be known by now
    if cfg['MPI'] != {}:
        execmd = execmd.replace('<mpi_cmdexec>', mpi_cmd)\
                            .replace('<exename>', '')
    # <root> and <config> are part of the arguments
    execmd = execmd.replace('<root>', cfg['root']).replace('<config>', par_dir)

    return execmd


def get_ncsize(cas):
    """
    @brief Get the number of processors for the case

    @param cas (string): name of the CAS file

    @return ncsize (int): number of processors for the CAS
    """

    # ~~ check keyword ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ncsize = cas.get('PARALLEL PROCESSORS')

    return ncsize


def get_mpi_cmd(cfg_mpi):
    """
    @brief Gets the MPI command to be executed from the config file

    @param cfg_mpi (string): MPI EXEC command in the config file

    @return mpi_cmd (string): MPI command to be executed
    """
    # ~~> Executable
    mpi_cmd = cfg_mpi['EXEC']
    # ~~> host file
    hostfile = ''
    if 'HOSTFILE' in cfg_mpi:
        hostfile = cfg_mpi['HOSTFILE']
    mpi_cmd = mpi_cmd.replace('<hostfile>', hostfile)
    # ~~> stdin file
    infile = ''
    if 'INFILE' in cfg_mpi:
        infile = cfg_mpi['INFILE']
    mpi_cmd = mpi_cmd.replace('<mpi_infile>', infile)

    return mpi_cmd


def get_hpc_cmd(cfg_hpc):
    """
    @brief Gets the HPC command to be executed from the config file

    @param cfg_hpc (string): HPC configuration in the CONFIG file,
           either from EXCODE or PYCODE

    @return hpc_cmd (string): HPC command to be executed
    """
    # ~~> Executable
    if 'EXCODE' in cfg_hpc:
        hpc_cmd = cfg_hpc['EXCODE']
    elif 'PYCODE' in cfg_hpc:
        hpc_cmd = cfg_hpc['PYCODE']
    # ~~> script
    if 'STDIN' in cfg_hpc:
        hpc_stdin = cfg_hpc['STDIN'][0]
        hpc_cmd = hpc_cmd.replace('<hpc_stdin>', hpc_stdin)

    return hpc_cmd


def get_hpc_depend(cfg_hpc):
    """
    @brief Gets the HPC jobs dependency (for example, only launch a job
        when the previous one has successfully terminated, see
        systel.cis-hydra.cfg)

    @param cfg_hpc (string): HPC configuration in the CONFIG file

    @return cfg_hpc['DEPEND'] (string): hpc_depend configuration in the CONFIG
        file
    """
    # ~~> Executable
    if 'DEPEND' in cfg_hpc:
        return cfg_hpc['DEPEND']

    return ''


def get_conlim(cas):
    """
    @brief Gets the name of file for boundary conditions in the working
    directory (defined in dictionary)


    @param cas (TelemacCas): name of the CAS file

    @return conlim: name of the conlim file (.cli)
    """
    # @TODO: Include in TelemacCas ?

    # ~~ look for conlim ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    conlim = ''
    for k in cas.in_files:
        if cas.in_files[k].split(';')[5] == 'CONLIM':
            conlim = cas.in_files[k].split(';')[1]
    return conlim


def get_glogeo(cas):
    """
    @brief Gets the GEO file for the whole domain out of the
        list of input files

    @param cas (string):  name of the CAS file

    @return glogeo: name of the glogeo file (.geo)
    @return fmtgeo: format of the glogeo file (serafin or med)
    @return globnd: name of the CONLIM file (.cli) associated to the
        GEO file. Warning; CONLIM and globnd are maybe the same file,
        actually.
    """

    # ~~ look for GLOBAL GEO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    glogeo = ''
    fmtgeo = ''
    globnd = ''
    for k in cas.in_files:
        submit = cas.in_files[k].split(';')
        if submit[5][-4:] == 'GEOM':
            glogeo = submit[1]
            fmtgeo = get_file_format(cas, k)
        if submit[5] == 'CONLIM':
            globnd = submit[1]

    return glogeo, fmtgeo, globnd


def get_file_format(cas, keyword):
    """
    Search in a cas object for the format key word
    associated with the keyword in argument
    """
    # Loop on all the keywords in the cas file
    for k in cas.values:
        # The keyword we are searching for contains both the keyword 'keyword'
        # and the word FORMAT (same word in french and english)
        if keyword in k and ('FORMAT ' in k or ' FORMAT' in k):
            return cas.values[k]
    # By default if there is no format keyword the file is SERAFIN
    return 'SERAFIN'


def get_partitionner(partitionner):
    """
    Return the partionner to use in partel

    @param partitionner (string) string containing name of partitionner

    @return (int) Integer value associated to the partitionner
    """
    part2int = {"METIS": 1, "SCOTCH": 2, "PARMETIS": 3,
                "PTSCOTCH": 4}
    # If the key in not in the steering file we use metis
    i_part = 1 if partitionner == '' else part2int[partitionner]

    return i_part
