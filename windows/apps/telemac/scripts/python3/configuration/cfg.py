"""
Contains the class for configuration
"""

import re
from os import environ, walk, path, remove
from socket import gethostname
import configparser
from utils.files import remove_directories
from utils.exceptions import TelemacException
from configuration.config_tools import get_folders_modules_telemac, \
    get_tags, add_externals, get_externals, parse_user_modules, \
    get_files_validation_telemac

class Config():
    """
    Class containing all information on a configuration
    """
    def __init__(self):
        self.general = None
        self.configs = None
        self.cfgname = ''
        self.cfg_file = ''

    def read_cfg(self):
        """
        Read the content of the config file and extract all cfgs,
        and their key/values -- Returns a dictionary of all configs in
        the files that are highlighted in [Configurations]
        """
        bypass = False
        # ~~ Read Configuration File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cfgfile = configparser.RawConfigParser()
        try:
            cfgfile.read(self.cfg_file, encoding='utf-8')
        except configparser.Error as xcpt:
            raise TelemacException(\
                 'Error while reading {}'.format(self.cfg_file)+str(xcpt))
        # ~~ Read Config Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cfgs = cfgfile.get('Configurations', 'configs')
        if cfgs == '':
            raise TelemacException(\
                 '\nPlease specify appropriate configuration names for '
                 'key [Configurations] in the config file\n')
        # ~~ Filter Config Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cfgnames = cfgs.split()
        if self.cfgname != '':
            if self.cfgname not in cfgnames:
                print('\nNot able to find your configuration [{}]'
                      ' in the configuration file: {}'.format(self.cfgname,
                                                              self.cfg_file))
                if bypass:
                    print(' ... will try to gess the configuration from '
                          'the general keys and move on ...')
                else:
                    err = '\n ... use instead:\n'
                    for cfg in cfgnames:
                        err += '    +> {}\n'.format(cfg)
                    raise TelemacException(err)

            cfgnames = [self.cfgname]
        else:
            # If no configuration given taking the first one
            self.cfgname = cfgnames[0]

        # ~~ Verify presence of configs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for cfg in cfgnames:
            if cfg not in cfgfile.sections():
                raise TelemacException(\
                   '\nNot able to find the configuration [{}] '
                   'in the configuration file: {}'.format(cfg, self.cfg_file))
        # ~~ Read General ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if 'general' in cfgfile:
            self.general = dict(cfgfile.items('general'))
        else:
            self.general = {}
        # ~~ Loads Configurations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.configs = {}
        for cfg in cfgnames:
            if cfg in cfgfile:
                self.configs.update({cfg: dict(cfgfile.items(cfg))})
            else:
                self.configs.update({cfg: {}})

    def parse_cfg_file(self, cfg_file, name, root_dir, python_dir):
        """
        Get the name of the config file from command line arguments
        and store its rough content in a dict -- Returns the dict
        set globals CONFIGS

        @param cfg_file (string) the name of the configuration file
        @param name (string) configuration name
        @param root_dir (string) Path to root of sources
        @param python_dir (string) Path to root of Python scripts

        @return config_dict (dictionary) information for the configuration
        """
        # Checking that file exist
        if not path.exists(cfg_file):
            raise TelemacException('Could not find {}'.format(cfg_file))

        self.cfg_file = cfg_file
        self.cfgname = name
        # ~~ Parse CFG File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.read_cfg()

        # ~~ Replacing user keys throughout ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        key_sqbrack = re.compile(r'(?P<brack>\[[\w_.-~=+]*?\])')  # ,re.I)
        for cfgname in self.configs:
            # ~~> making sure cfgname also includes all keys from general
            cfg = self.configs[cfgname]
            for genkey in self.general:
                if genkey not in cfg:
                    cfg.update({genkey: self.general[genkey]})
                    # ~~> listing all [key] up for replacement
                    # (avoiding recursive referencing)
            key_records = []
            for cfgkey in cfg:
                for k in re.findall(key_sqbrack, cfg[cfgkey]):
                    key_records.append(k)
            # ~~> replacing [key] by its value, if so defined
            for k in key_records:
                key = k.strip('[]')
                for cfgkey in cfg:
                    if key in cfg:
                        cfg[cfgkey] = cfg[cfgkey].replace(k, cfg[key])
            # ~~> defaulting [key] to the environment, if so defined
            for cfgkey in cfg:
                for k in re.findall(key_sqbrack, cfg[cfgkey]):
                    key = k.strip('[]')
                    if key in environ:
                        cfg[cfgkey] = \
                           cfg[cfgkey]\
                           .replace(k, environ[key])
                    else:
                        print('... Could not find your special key '+k+
                              ' in key '
                              + cfgkey + ' of configuration ' + cfgname)
            # Setting root key if not defined
            if 'root' not in cfg:
                cfg['root'] = path.normpath(root_dir)
            if cfg['root'] == '':
                cfg['root'] = path.normpath(root_dir)
            else:
                cfg['root'] = path.normpath(cfg['root'])
            # Setting root key if not defined
            if 'pytel' not in cfg:
                cfg['pytel'] = python_dir
            if cfg['pytel'] == '':
                cfg['pytel'] = python_dir

    def get_config_key(self, key, there=False, empty=False):
        """
        Get the value of a key from the config cfg. Further, test if
        the key is there and if it is empty -- Return the value

        @param key (string) key of cfg dict.
        @param there (boolean) test if the key is there
        @param empty (boolean) test if it is empty

        @return value of a key from the config cfg
        """
        cfg = self.configs[self.cfgname]
        # ~~ Get the root of the System ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if there and key not in cfg:
            raise TelemacException(\
                    '\nKey {} required in configuration '
                    '(or on the command line as an option) \n'.format(key))
        if empty and cfg[key] == '':
            raise TelemacException(\
                    '\nKey {} required non empty field '
                    '(or on the command line as an option) \n'.format(key))
        return cfg.get(key, '')

    def get_root(self):
        """
        Returns the root dir of the Telemac installation
        """
        return self.configs[self.cfgname].get('root', '')

    def get_version(self):
        """
        Returns the version version of the Telemac configuration if none in
        systel.cfg file will return main
        """
        return self.configs[self.cfgname].get('version', 'main')

    def light_dump(self):
        """
        Light dump for a configuration
        """
        cfg = self.configs[self.cfgname]
        print('\n\n' + '~' * 72 + '\n')
        print(self.cfgname + ': \n    ')
        if 'brief' in cfg:
            print('\n    +> '
                  + '\n    |  '.join(cfg['brief'].split('\n'))
                  + '\n')
        print('    +> root:    ' + cfg['root'])

        modules = sorted(cfg['MODULES'].keys())
        n_mod = len(modules)
        n_mod_per_line = 4
        str_mod = ' / '.join(modules[0:n_mod_per_line]) + "\n"
        intent = '               '
        for i in range(1, n_mod//n_mod_per_line):
            tmp_mod = modules[i*n_mod_per_line:(i+1)*n_mod_per_line]
            str_mod += intent+'  / '.join(tmp_mod) + "\n"
            last = i
        str_mod += intent+' / '.join(modules[(last+1)*n_mod_per_line:])

        print('    +> module: ' + str_mod)

    def compute_zip_info(self):
        """
        Extract information for zipping files
        """
        self.configs[self.cfgname]['ZIPPER'] = \
                self.get_config_key('sfx_zip', there=True)[1:]


    def compute_modules_info(self, rescan=False, bypass=False, add_odd=False):
        """
        Deduce the actual list of modules existing within the root teldir,
        identified by matching the directory structure to the template
        teldir\\module_name\\
        Filling the 'MODULES' key in the configuration

        @param rescan (boolean) check if it must rescan
        @param bypass (boolean) continue with a raise exception
        @param add_odd (boolean) If true computing ADDONES and ODDONES as well
        """
        # ~~ List all files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #
        #  Search all files within the <root> following a special template
        #  following
        #     <root>|sources|module-name or <root>|sources|utils|sub-module-name
        #  All files means here, all files with extensions:
        #    .cmdf or .f. f90 .F .F90 or .dico files
        #
        cfg = self.configs[self.cfgname]

        modules, to_del = get_folders_modules_telemac(cfg['root'], rescan,
                                                      bypass)
        cfg['MODULES'] = modules
        #  ... at this stage, you may not have found any or all cmdf files, and
        #  thus not know yet about the tags.
        #
        if add_odd:
            #  Get the configuration key tag_... for special effects, These
            #  will be compiled together with the main compilation as one.  The
            #  odd ones are here only to counteract the add ones, in case the
            #  main should be compiled as a self comtained program. If absent,
            #  the main would be incomplete and dependent on the compilation of
            #  the add ones.
            #
            tags, modules = get_tags('tag', cfg, cfg['MODULES'])
            cfg['ODDONES'] = tags
            cfg['MODULES'].update(modules)
            #
            #  Get the configuration key add_... for special effects, added to
            #  the main compilation as a separate tree with its own compilation
            #  options. The main program requires this additional tree to be
            #  complete, and therefore linked here as external.
            #
            adds, modules = get_tags('add', cfg, cfg['MODULES'])
            cfg['ADDONES'] = adds
            cfg['MODULES'].update(modules)
            #  ... at this stage, you still do not know where are the files
            #  referred to in the tags even thouh get_folders_modules_telemac()
            #  has listed all on the system. Later on parser_fortran() will be
            #  going through that.
        return to_del

    def compute_trace_info(self):
        """
        Compute tagged fields for cmdx and cmdo
        Filling the 'TRACE' key
        """
        cfg = self.configs[self.cfgname]

        cfg['TRACE'] = {}
        for k in cfg:
            if k[0:4] in ['root', 'libs']:
                get = self.get_config_key(k)
                if get != '':
                    cfg['TRACE'][k] = get

    def compute_system_info(self):
        """
        Get system's suffixes for obj, lib, mod, and exe
        """
        # Get system's suffixes for obj, lib, mod, and exe
        cfg = self.configs[self.cfgname]

        cfg['SYSTEM'] = {}
        system = cfg['SYSTEM']
        system['sfx_obj'] = self.get_config_key('sfx_obj', there=True).lower()
        system['sfx_exe'] = self.get_config_key('sfx_exe', there=True).lower()
        system['sfx_lib'] = self.get_config_key('sfx_lib', there=True).lower()
        system['sfx_mod'] = self.get_config_key('sfx_mod', there=True).lower()
        system['sfx_pyf'] = self.get_config_key('sfx_pyf').lower()
        system['sfx_pyd'] = self.get_config_key('sfx_pyd').lower()

    def compute_compilation_info(self, rescan=False, bypass=False):
        """
        Extract all the information required for
        the Compilation of TELEMAC
        Requires: root,
        Optional: mods_, incs_, libs_, ... and options

        @param cfgname (string) Name of the configuration
        @param rescan(boolean) check if it must rescan
        @param bypass(boolean) continue with a raise exception

        """
        cfg = self.configs[self.cfgname]

        tbd = self.compute_modules_info(rescan, bypass, add_odd=True)

        # ~~ Compilation options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #
        #  Get libs_all: ... libs_artemis: ... libs_passive: ...
        #  mods_all: ... etc.
        #  for every module in the list of modules to account for
        #  specific external includes for all or each module
        for mod in cfg['MODULES']:
            for ext in ['mods', 'incs', 'libs']:
                externals = add_externals(cfg, ext, mod)\
                            .replace('<root>', cfg['root'])
                cfg['MODULES'][mod][ext] = externals
        # Get cmd_obj: ... cmd_lib: ... cmd_exe: ...
        # the compiler dependent command lines to create obj, lib and exe
        for mod in cfg['MODULES']:
            for ext in ['obj', 'lib', 'exe', 'pyf', 'pyd']:
                externals = get_externals(cfg, ext, mod)\
                         .replace('<root>', cfg['root'])
                cfg['MODULES'][mod]['x'+ext] = externals

        cfg['COMPILER'] = {}
        #  Get modules: user list of module
        #  in which 'system' means all existing modules,
        #  and in which 'update' means a rebuild of the lib and exe
        #  and in which 'clean' means a rebuild of the obj, lib and exe
        #  and Get options: for the switches such as parallel, openmi, mumps,
        #  etc.
        get, tbr = parse_user_modules(cfg, cfg['MODULES'])
        get = get.split()
        #  Add extra modules for special effects as priority items (insert(0,))
        #  where can be passive, for instance
        for mod in cfg['ADDONES']:
            if mod not in get:
                get.insert(0, mod)
        #  Delayed removal of the relevant CMDF - exception made for mascaret
        for mod in tbd:
            if mod in get and mod != 'mascaret':
                for fle in tbd[mod]:
                    remove(fle)

        cfg['COMPILER']['MODULES'] = get
        cfg['COMPILER']['REBUILD'] = tbr
        for mod in get:
            if mod not in cfg['MODULES']:
                raise TelemacException(\
                        '\nThe following module does not exist '
                        '{} \n'.format(mod))

        self.compute_zip_info()

        self.compute_system_info()

        self.compute_trace_info()

    def compute_partel_info(self):
        """
        Get path_parallel: for parallel option
        the parallel dependent command line executables (partel, gretel, ...)
        """
        cfg = self.configs[self.cfgname]
        cfg['PARTEL'] = {}

        if 'par_path' in cfg:
            cfg['PARTEL']['PATH'] = cfg['par_path']
        if 'par_cmdexec' in cfg:
            cfg['PARTEL']['EXEC'] = cfg['par_cmdexec']

    def compute_mpi_info(self):
        """
        Get mpi options for systel
        """

        cfg = self.configs[self.cfgname]
        cfg['MPI'] = {}
        mpi = cfg['MPI']
        if 'mpi_cmdexec' in cfg:
            mpi['EXEC'] = cfg['mpi_cmdexec']
            if 'mpi_infile' in cfg:
                mpi['INFILE'] = cfg['mpi_infile']
            # /!\ defaulting on  the local hostname
            mpi['HOSTS'] = gethostname().split('.')[0]
            if 'mpi_hosts' in cfg:
                if len(cfg['mpi_hosts'].split()) > 0:
                    mpi['HOSTS'] = cfg['mpi_hosts']
            mpi['HOSTFILE'] = 'MPI_HOSTFILE'

    def compute_hpc_info(self):
        """
        Get hpc options from systel
        """
        cfg = self.configs[self.cfgname]
        cfg['HPC'] = {}
        hpc = cfg['HPC']
        if 'hpc_cmdexec' in cfg:
            hpc['EXCODE'] = cfg['hpc_cmdexec']
            if 'hpc_stdin' in cfg:
                hpc['STDIN'] = ['HPC_STDIN', cfg['hpc_stdin']\
                           .replace(r'\n', '\n')]
        if 'hpc_runcode' in cfg:
            hpc['PYCODE'] = cfg['hpc_runcode']
            if 'hpc_stdin' in cfg:
                hpc['STDIN'] = ['HPC_STDIN', cfg['hpc_stdin']\
                           .replace(r'\n', '\n')]
        if 'hpc_depend' in cfg:
            hpc['DEPEND'] = cfg['hpc_depend']


    def compute_execution_info(self):
        """
        Extract all the information required for the launch and execution
        of TELEMAC based on one CAS file, for each configuration
        """
        self.compute_modules_info()

        self.compute_partel_info()

        self.compute_mpi_info()

        self.compute_hpc_info()

        self.compute_zip_info()

        self.compute_system_info()

        self.compute_trace_info()

    def clean_install(self, cfgname):
        """
        Clean configuration install folder (deletes it)

        @param cfgname (string) Name of the configuration
        """
        cfg_dir = path.join(self.configs[cfgname]['root'], 'builds', cfgname)
        if path.exists(cfg_dir):
            remove_directories(cfg_dir)
            print('\n    +> build deleted!')
        else:
            print('\n ... Found no build to delete!')

    def compute_compact_info(self):
        """
        Extract information for compactTELEMAC
        """
        self.compute_modules_info()

        self.compute_zip_info()

    def compute_vnv_info(self):
        """
        Extract all the information required for the validation
        of the relevant modules for each configuration
        The principal assumption is that the validation cases are
        either under:
          + val_root\\*
          + teldir\\examples\\module\\val_root\\*
        If the 'val_root' key is not in the config, the default
        path is assumed to be based on the second option
        """
        cfg = self.configs[self.cfgname]

        self.compute_modules_info()

        # Get libs_all: ... libs_artemis: ... mods_all: ... etc.
        # for every module in the list of modules to account for
        # specific external includes for all or each module
        for mod in cfg['MODULES']:
            cfg['MODULES'][mod].update(\
                {'mods': add_externals(cfg, 'mods', mod)\
                         .replace('<root>', cfg['root'])})
            cfg['MODULES'][mod].update(\
                {'incs': add_externals(cfg, 'incs', mod)\
                         .replace('<root>', cfg['root'])})
            cfg['MODULES'][mod].update(\
                {'libs': add_externals(cfg, 'libs', mod)\
                         .replace('<root>', cfg['root'])})

        cfg['VALIDATION'] = {}
        # Get validation: user list of module and there associated directories
        # in which 'system' means all existing modules,
        # and in which 'update' means a continuation,
        #     ignoring previously completed runs
        # and in which 'clean' means a re-run of all validation tests
        if 'val_root' not in cfg:
            val_root = path.realpath(path.join(cfg['root'], 'examples'))
            if not path.isdir(val_root):
                raise TelemacException(\
                 '\nNot able to find your validation set from the path: {} \n\n'
                 ' ... check the val_root key in your configuration file'
                 ''.format(val_root))
        else:
            val_root = cfg['val_root'].replace('<root>', cfg['root'])
        cfg['val_root'] = val_root

        _, examples, _ = next(walk(val_root))
        get, tbr = parse_user_modules(cfg, cfg['MODULES'])
        cfg['REBUILD'] = tbr
        # Removing specials module if we are not in system or if they are
        # not explicitly given
        specials = ['python3']
        # Removing python2 examples
        if 'python27' in examples:
            examples.remove('python27')

        for mod in specials:
            if not ("system" in cfg['modules'] or mod in cfg['modules']):
                examples.remove(mod)

        # Removing module that are not in the configuration
        for mod in list(examples):
            if mod not in get and mod in cfg['MODULES']:
                examples.remove(mod)
        # Exception for mascaret as it is not in cfg_telemac['MODULES']
        # Because it does not have a mascaret.dico file
        if 'mascaret' not in get:
            if 'mascaret' in examples:
                examples.remove('mascaret')
        for mod in examples:
            val_dir = path.join(val_root, mod)
            val_mod = get_files_validation_telemac(val_dir)
            if val_mod != {}:
                cfg['VALIDATION'].update(\
                     {mod: {'path': path.realpath(val_dir)}})
                cfg['VALIDATION'][mod].update(val_mod)

        self.compute_partel_info()

        self.compute_mpi_info()

        self.compute_hpc_info()

        self.compute_zip_info()

        self.compute_system_info()

        self.compute_trace_info()

    def compute_doxy_info(self):
        """
        Extract all the information required for
        the Documentation of TELEMAC preparing for Doxygen
        """
        cfg = self.configs[self.cfgname]
        # Get destination doxydocs: ...
        self.get_config_key('doxydocs', there=True, empty=True)
        # Get doxygen command: ...
        self.get_config_key('cmd_doxygen', there=True, empty=True)

        self.compute_modules_info()

        cfg['COMPILER'] = {}
        # Get modules: user list of module
        # in which 'system' means all existing modules,
        # and in which 'update' means an update only of the source files and
        # tags
        # and in which 'clean' means a rebuild of all source files and tags
        # and Get options: for the switches such as parallel, openmi, mumps,
        # etc.
        get, tbr = parse_user_modules(cfg, cfg['MODULES'])
        cfg['COMPILER'].update({'MODULES': get.split()})
        cfg['COMPILER'].update({'REBUILD': tbr})
        for mod in get.split():
            if mod not in cfg['MODULES']:
                raise TelemacException(\
                     '\nThe following module does not exist {}\n'.format(mod))
        for mod in get.split():
            if mod not in cfg['MODULES']:
                mod_idx = cfg['COMPILER']['MODULES'].index(mod)
                del cfg['COMPILER']['MODULES'][mod_idx]

        self.compute_zip_info()

        # Get system's suffixes for obj, lib, mod, and exe
        cfg['SYSTEM'] = {}
