#!/usr/bin/env python3
"""@author TELEMAC-MASCARET Consortium
    @brief runcode is the execution launcher for all TELEMAC modules
"""
# _____             ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from argparse import ArgumentParser, RawDescriptionHelpFormatter
from os import path
# ~~> dependencies towards other pytel/modules
from utils.files import check_sym_link
from utils.messages import Messages, git_banner
from utils.exceptions import TelemacException
from config import add_config_argument, update_config, CFGS
from execution.run_cas import run_study

def add_mpi_argument(parser):
    """
    Adding argument for an mpi run

    @param parser (ArgumentParser) where to add the arguments

    @return (ArgumentParser) The updated parser
    """
    parser.add_argument(
        "--hosts",
        dest="hosts", default='',
        help="specify the list of hosts available for parallel mode, "
             "';' delimited")
    parser.add_argument(
        "--ncsize",
        dest="ncsize", default=-1, type=int,
        help="the number of processors forced in parallel mode")
    parser.add_argument(
        "--nctile",
        dest="nctile", default=0, type=int,
        help="the number of core per node. ncsize/nctile is the "
             "number of compute nodes")
    parser.add_argument(
        "--ncnode",
        dest="ncnode", default=0, type=int,
        help="the number of of nodes. ncsize = ncnode*nctile is "
             "the total number of compute nodes")

    return parser

def add_hpc_argument(parser, module=None):
    """
    Adding argument for an mpi run

    @param parser (ArgumentParser) where to add the arguments
    @param module (string) Telemac module

    @return (ArgumentParser) The updated parser
    """
    if module is None:
        parser.add_argument(
            "--jobname",
            dest="jobname", default=path.basename(sys.argv[0]),
            help="specify a jobname for HPC queue tracking")
    else:
        parser.add_argument(
            "--jobname",
            dest="jobname", default=module,
            help="specify a jobname for HPC queue tracking")
    parser.add_argument(
        "--queue",
        dest="hpc_queue", default='',
        help="specify a queue for HPC queue tracking")
    parser.add_argument(
        "--walltime",
        dest="walltime", default='01:00:00',
        help="specify a walltime for HPC queue tracking")
    parser.add_argument(
        "--email",
        dest="email", default='',
        help="specify an e-mail adress to warn when HPC job is finished")
    return parser

def add_runcode_argument(parser, module=None):
    """
    Adding argument for runcode

    @param parser (ArgumentParser) where to add the arguments
    @param module (string) Telemac module

    @return (ArgumentParser) The updated parser
    """
    # ~~> Environment
    parser = add_config_argument(parser)
    parser.add_argument(
        "-s", "--sortiefile", action="store_true",
        dest="sortie_file", default=False,
        help="specify whether there is a sortie file, default is no")
    parser.add_argument(
        "-t", "--tmpdirectory", action="store_false",
        dest="tmpdirectory", default=True,
        help="specify whether the temporary directory is removed, "
             "default is yes")
    parser.add_argument(
        "-w", "--workdirectory",
        dest="w_dir", default='',
        help="specify whether to re-run within a defined subdirectory")
    parser.add_argument(
        "--nozip", action="store_true",
        dest="nozip", default=False,
        help="specify whether to zip the extra sortie file if "
             "simulation in parallel")
    # ~~> HPC / parallel
    parser = add_hpc_argument(parser, module=module)
    parser = add_mpi_argument(parser)


    parser.add_argument(
        "--sequential",
        action="store_true",
        dest="sequential", default=False,
        help="if present, imposes that multiple CAS files are launched "
             "one after the other")
    parser.add_argument(
        "--mpi", action="store_true",
        dest="mpi", default=False,
        help="make sure the mpi command is executed, ignoring any hpc command")

    # Splitting execution arguments
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--split", action="store_true",
        dest="split", default=False,
        help="will only do the trace (and the split in parallel) if option "
             "there")
    group.add_argument(
        "--run", action="store_true",
        dest="run", default=False,
        help="will only run the simulation if option there")
    group.add_argument(
        "-x", "--compileonly", action="store_true",
        dest="compileonly", default=False,
        help="specify whether to only create an executable but not run, "
             "default is no")
    group.add_argument(
        "--merge", action="store_true",
        dest="merge", default=False,
        help="will only do the output copying (and recollection in parallel) "
             "if option there")
    # ~~> Other
    parser.add_argument(
        "--use-link", action="store_true",
        dest="use_link", default=False,
        help="Will use link instead of copy in the temporary folder"
             " (Unix system only)")

    if module == "mascaret" or module == None:
        parser.add_argument(
            "--masc-cas", action="store_true",
            dest="mascaret_cas", default=False,
            help="option to indicate the Mascaret steering file is in Damocles"
                 " format (with keywords and needing a dico file to be parsed)."
                 " Without this option, extension of the parameter file is used"
                 " to detect the format (Damocles if .cas, xml xcas otherwise)")

    return parser

def main(module=None):
    """
    @brief Main function of the runcode.py module

    @param module (string): the name of the module to run (
      available modules are: telemac2d, telemac3d, artemis, tomawac,
      sisyphe, artemis, postel3d, ...)

    @return None
    """

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nLoading Options and Configurations\n'+72*'~'+'\n')
    if module is None:
        parser = ArgumentParser(
            formatter_class=RawDescriptionHelpFormatter,
            description=('''\n
runcode is the execution launcher for all TELEMAC modules.\n
where module can be:\n
    mascaret     the 1D hydrodyanmic / tracer / water quality solver
    telemac2d    the 2D hydrodyanmic / tracer / water quality solver
    telemac3d    the 3D hydrodynamic / tracer / water quality solver
    artemis      the phase resolving wave solver
    tomawac      the 3rd generation wave transformation solver
    stbtel       a pre-processor for the modules
    postel3d     a post-processor for telemac3d
            '''),
            usage=' (--help for help)\n---------\n        =>  '
                  '%(prog)s module [options] casfile(s)\n---------',
            epilog=('''\nexamples:\n---------
1:     => runcode.py telemac2d -s t2d.cas
---------'''))
        parser.add_argument(
            "module",
            default=None,
            choices=['telemac2d', 'telemac3d', 'artemis', 'tomawac',
                     'stbtel', 'postel3d', 'mascaret'])
    else:
        parser = ArgumentParser(
            formatter_class=RawDescriptionHelpFormatter,
            description=('''\n
%(prog)s is one of the execution launcher for the TELEMAC system.
            '''),
            epilog=('''\nexamples:\n---------
1:     => %(prog)s -s t2d.cas
---------'''))
        parser.set_defaults(module=module)

    parser = add_runcode_argument(parser, module=module)
    # Arguments
    parser.add_argument("args", metavar='cas file(s)', nargs="+")

    options = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_config(options)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ banners ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    git_banner(CFGS.get_root(), version=CFGS.get_version())

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for one configuration only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Checking if symlink is available
    if options.use_link and not check_sym_link(options.use_link):
        raise TelemacException(\
                '\nThe symlink option is only '
                'available on Linux systems. '
                'Remove the option and try again')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads command line arguments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    code_name = options.module
    cas_files = options.args

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for only one configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # bypass errors and carries on
    options.bypass = False
    if options.split or options.merge or options.run:
        if options.w_dir == '':
            raise TelemacException(\
                    '\nPlease use option -w (--workdirectory)'
                    ' with either of the options '
                    '--split, --run or --merge\n')
    # parsing for proper naming
    CFGS.compute_execution_info()
    cfg = CFGS.configs[CFGS.cfgname]

    print('\n\nRunning your CAS file(s) for:\n'+'~'*72+'\n')
    CFGS.light_dump()
    if options.w_dir != '':
        print('     +> directory        ' + options.w_dir)
        options.tmpdirectory = False
    print('\n\n'+'~'*72+'\n')

# >>> Check wether the config has been compiled for the runcode
    if options.compileonly:
        cfg['REBUILD'] = 1
    if code_name not in cfg['MODULES']:
        raise TelemacException(\
                '\nThe code requested is not installed '
                'on this system : {}\n'.format(code_name))

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xcpts = Messages()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Run the Code from the CAS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for cas_file in cas_files:
        run_study(cas_file, code_name, options)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if xcpts.not_empty():
        raise TelemacException(\
                '\n\nHummm ... I could not complete '
                'my work.\n{}{}'.format('~'*72, xcpts.except_messages()))

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nMy work is done\n\n')
    sys.exit(0)


if __name__ == "__main__":
    main(None)
