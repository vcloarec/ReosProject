#!/usr/bin/env python3
"""@author TELEMAC-MASCARET Consortium

   @brief display configuration information
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path, environ
from argparse import ArgumentParser
#from utils.messages import git_banner
from configuration.cfg import Config

CFGS = Config()
# ______                  _________________________________________
# ____/ Global Variables /_________________________________________/
#
"""
     Contains the entire configuration tree for all user configurations.
     By default the name is systel.cfg stored locally
"""
def add_config_argument(parser):
    """
    Adding to a parser the option for configuration

    @param parser (ArgumentParser) Parser to update

    @return (ArgumentParser) The udpated parser
    """

    parser.add_argument(
        "-c", "--configname", metavar="config name",
        dest="config_name", default='',
        help="specify configuration name, default is randomly "
             "found in the configuration file")
    parser.add_argument(
        "-f", "--configfile", metavar="config file",
        dest="config_file", default='',
        help="specify configuration file, default is systel.cfg")
    parser.add_argument(
        "-r", "--rootdir", metavar="TELEMAC root",
        dest="root_dir", default='',
        help="specify the root, default is taken from config file")

    return parser

def update_config(options):
    """
    Update to config class from environment, arguments...
    """
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The path to the root relates to the script launched, which implies
    # that the user environment knows which to run
    # (this script is stored under .../scripts/python3/)

    python_dir = path.dirname(path.realpath(__file__))

    # Defining root_dir
    root_dir = environ.get('HOMETEL', options.root_dir)
    if root_dir == '':
        # If root dir is in neither HOMETEL or options
        # It is computed from the path of the script
        root_dir = path.dirname(path.dirname(python_dir))

    # Defining user configuration name
    cfg_name = environ.get('USETELCFG', options.config_name)

    # Defining user configuration file
    cfg_file = environ.get('SYSTELCFG', options.config_file)
    if cfg_file == '':
        # Computing position from root_dir
        cfg_file = path.join(root_dir, 'configs', 'systel.cfg')

    CFGS.parse_cfg_file(cfg_file, cfg_name, root_dir, python_dir)


def main():
    """
        function to display configuration informations
    """
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nLoading Options and Configurations\n' + 72 * '~' + '\n')
    parser = ArgumentParser(
        description=('''\n
List active configurations:\n
1. check if your system points to a valid configuration file
2. parse your configuration file and display the briefs of your
 active configurations
          '''))

    parser = add_config_argument(parser)

    parser.add_argument(
        "--clean", action="store_true",
        dest="config_delete", default=False,
        help="remove the directories in relation to the"
             "named configuration(s)")
    options = parser.parse_args()

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    update_config(options)

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # ~~~~ banners ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    git_banner(CFGS.get_root(), version=CFGS.get_version())

    for cfgname in CFGS.configs:
        CFGS.cfgname = cfgname
        CFGS.compute_modules_info()

        CFGS.light_dump()

        if options.config_delete:
            CFGS.clean_install(cfgname)

    print('\n\n' + '~' * 72 + '\n')
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nMy work is done\n\n')
    sys.exit(0)


if __name__ == "__main__":
    main()
