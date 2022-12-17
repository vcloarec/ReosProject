"""
 Config.py Tools
 @note ... this work is based on a collaborative effort
 of the Telemac-Mascaret consortium

"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import re
from os import path, walk, listdir
from compilation.scan_tools import get_scan_content
from utils.exceptions import TelemacException

# _____                  ___________________________________________
# ____/ TELEMAC Toolbox /__________________________________________/
#

def get_files_validation_telemac(root):
    r"""
    Get the root of the validation directory and walk from there
    through the directory structure to identify modules with the template
    val_root|module_name|*|vnv_*

    @param root (string) root of the system to replace reference to <root>

    @return validation (dictionary) Python files of validation directory
    """
    validation = {}
    dirpath, dirnames, _ = next(walk(root))
    for ddir in dirnames:
        val = {ddir: []}
        _, _, filenames = next(walk(path.join(dirpath, ddir)))
        for fle in filenames:
            if path.splitext(fle)[0][0:4] == 'vnv_':
                val[ddir].append(fle)
        if val[ddir] != []:
            validation.update(val)

    return validation


def get_folders_modules_telemac(root, rescan, bypass):
    """
    Walk through the directory structure available from the root
    and identifies modules with the template:
          sources|module-name or sources|utils|sub-module-name

    @param root (string) root of the system to replace reference to <root>
    @param rescan (boolean) check if it must rescan
    @param bypass (boolean) continue with a raise exception

    @return modules (dictionnary) module information
    @return tobedel (dictionary)  Contains cmdf to de delete when doing a rescan
                                 and a clean
    """
    #
    # note that the references to the cmdf will be stored with their path.
    # the dico and the fortran files will be stored without their path.
    #
    modules = {}
    tobedel = {}
    sources = path.join(root, 'sources')
    if not path.exists(sources):
        raise TelemacException(\
                 'I could not locate the source code in '
                 + sources +
                 '\n     ... you may not have told me where '
                 'the root is on your system.')

    for moddir in listdir(sources):
        # ~~> excluding files and .git or other hidden directories
        if moddir[0] == '.' or path.isfile(path.join(sources, moddir)):
            continue
        # ~~> moddir in lower case please
        modroot = path.join(sources, moddir.lower())
        if not path.exists(modroot):
            continue

        # ~~ Walk through ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        dirpath, dirnames, filenames = next(walk(modroot))

        # ~~> One level in modroot
        for fle in filenames:
            f = path.splitext(fle)
            # /!\ you found an executable module
            if f[len(f) - 1].lower() in ['.dico']:
                if moddir not in modules:
                    modules.update({moddir: {'path': dirpath,
                                             'dico': fle,
                                             'cmdfs': []}})
                else:
                    modules[moddir].update({'dico': fle})

            # /!\ you found at least one cmdf file
            if f[len(f) - 1].lower() in ['.cmdf']:
                if rescan:
                    if moddir not in tobedel:
                        tobedel.update({moddir: [path.join(dirpath, fle)]})
                    else:
                        tobedel[moddir].append(path.join(dirpath, fle))
                else:
                    cmdf = get_scan_content(path.join(dirpath, fle),
                                            root, bypass)
                    genmod = cmdf['general']['module']
                    if dirpath != cmdf['general']['path']:
                        raise TelemacException(\
                            ' This cmdf file ' + fle +
                            ' is out of place\n     ... '
                            'check its internal path.')
                    if genmod not in modules:
                        modules.update({genmod: {'path': dirpath,
                                                 'cmdfs': [fle]}})
                    elif 'cmdfs' not in modules[genmod]:
                        modules[genmod].update({'cmdfs': [fle]})
                    else:
                        modules[genmod]['cmdfs'].append(fle)
            # /!\ you found a source file
            if f[len(f) - 1].lower() in ['.f', '.f90']:
                if moddir not in modules:
                    modules.update({moddir: {'path': dirpath,
                                             'files': [fle],
                                             'cmdfs': []}})
                elif 'files' not in modules[moddir]:
                    modules[moddir].update({'files': [fle]})
                elif fle not in modules[moddir]['files']:
                    modules[moddir]['files'].append(fle)

        # ~~> Higher levels in modroot
        for subdir in dirnames:
            if subdir[0] == '.':
                # .git directories filter for old releases
                continue
            for subpath, _, filenames in walk(path.join(modroot, subdir)):
                if moddir not in ['utils']:
                    for fle in filenames:
                        f = path.splitext(fle)
                        # /!\ you found a source file
                        if f[len(f) - 1].lower() in ['.f', '.f90', '.c']:
                            file_path = path.join(subpath, fle)\
                                            .replace(modroot, '')[1:]
                            if moddir not in modules:
                                modules.update(
                                    {moddir: {'path': modroot,
                                              'files': [file_path],
                                              'cmdfs': []}})
                            elif 'files' not in modules[moddir]:
                                modules[moddir].update({'files': [file_path]})
                            else:
                                modules[moddir]['files'].append(file_path)
                else:
                    for fle in filenames:
                        f = path.splitext(fle)
                        # /!\ you found an executable module
                        if f[len(f) - 1].lower() in ['.dico']:
                            if subdir not in modules:
                                modules.update({subdir: {'path': subpath,
                                                         'dico': fle,
                                                         'cmdfs': []}})
                            else:
                                modules[subdir].update({'dico': fle})
                        # /!\ you found at least one cmdf file
                        if f[len(f) - 1].lower() in ['.cmdf']:
                            if rescan:
                                if subdir not in tobedel:
                                    tobedel.update({subdir: [path.join(subpath,
                                                                       fle)]})
                                else:
                                    tobedel[subdir].append(path.join(subpath,
                                                                     fle))
                            else:
                                cmdf = get_scan_content(path.join(subpath, fle),
                                                        root, bypass)
                                genmod = cmdf['general']['module']
                                if subpath != cmdf['general']['path']:
                                    raise TelemacException(\
                                         'This cmdf file ' + fle +
                                         ' is out of place\n     ... '
                                         'check its internal path.')
                                if genmod not in modules:
                                    modules.update({genmod: {'path': subpath,
                                                             'cmdfs': [fle]}})
                                elif 'cmdfs' not in modules[genmod]:
                                    modules[genmod].update({'cmdfs': [fle]})
                                else:
                                    modules[genmod]['cmdfs'].append(fle)
                        # /!\ you found a source file
                        if f[len(f) - 1].lower() in ['.f', '.f90']:
                            if subdir not in modules:
                                modules.update({subdir: {'path': subpath,
                                                         'files': [fle],
                                                         'cmdfs': []}})
                            elif 'files' not in modules[subdir]:
                                modules[subdir].update({'files': [fle]})
                            elif fle not in modules[subdir]['files']:
                                modules[subdir]['files'].append(fle)

    return modules, tobedel


def add_externals(cfg_dict, ext, mod):
    """
    Etract links to user defined external dependencies
    whether a lib, a mod or an include following the template
    key ext_all and ext_..., with ext = mods, incs, libs, cmd_*

    @param cfg_dict (dictionary) configuration informations
    @param ext (string) ext = mods, incs, libs, cmd_*
    @param mod (string) module in the list of modules
                       to account for specific external

    @return ext_list (string) links to user defined external dependencies
    """
    # ~~ Loads External Dependencies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ext_list = ''
    if ext in cfg_dict:
        ext_list = ' '.join(cfg_dict[ext].split())
    ext_all = ext + '_all'
    if ext_all in cfg_dict:
        ext_list += ' ' + ' '.join(cfg_dict[ext_all].split())
    ext_mod = ext + '_' + mod
    if ext_mod in cfg_dict:
        ext_list += ' ' + ' '.join(cfg_dict[ext_mod].split())
    return ext_list


def get_externals(cfg_dict, ext, mod):
    """
    Etract links to user defined external dependencies
    whether a lib, a mod or an include following the template
    key ext_all and ext_..., with ext = cmd_obj, cmd_lib, cmd_exe

    @param cfg_dict (dictionary) Globals configuration
    @param ext (string) ext = cmd_obj, cmd_lib, cmd_exe
    @param mod (string) module in the list of modules
                       to account for specific external

    @return ext_list(string) links to user defined external dependencies
    """
    # ~~ Loads External Dependencies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ext_list = ''
    if 'cmd_' + ext in cfg_dict:
        ext_list = ' '.join(cfg_dict['cmd_' + ext].split())
    ext_all = 'cmd_' + ext + '_all'
    if ext_all in cfg_dict:
        ext_list = ' '.join(cfg_dict[ext_all].split())
    ext_mod = 'cmd_' + ext + '_' + mod
    if ext_mod in cfg_dict:
        ext_list = ' '.join(cfg_dict[ext_mod].split())
    return ext_list


def get_tags(key, cfg_dict, sources):
    """
    Extract full user defined command line for the treatment of the tags, where
    tag_(name) produce a special effect on (name)

    @param key (string) key allowing to filter cfg_dict
    @param cfg_dict (dictionary) globale configuration
    @param sources (dictionary) globale modules information

    @return tags (dictionary) tags dict
    @return modules (dictionary)  user defined modules information
    """
    # ~~ Loads tag and creates exception ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    modules = {}
    tags = {}
    for k in cfg_dict:
        if k.split('_')[0] != key:
            continue
        tag = '_'.join(k.split('_')[1:])
        found_files = cfg_dict[k].split()
        tags.update({tag: []})
        for fle in found_files:
            if tag not in modules:
                modules.update({tag: {'path': '', 'cmdfs': [], 'files': []}})
            if tag in sources:
                modules[tag]['path'] = sources[tag]['path']
                modules[tag]['cmdfs'] = sources[tag]['cmdfs']
            if fle not in modules[tag]['files']:
                modules[tag]['files'].append(fle)
            tags[tag].append(fle)

    return tags, modules


def parse_user_modules(cfg_dict, modules):
    """
    Read the list of user defined modules for action -- Certain
    keyword such as clean, update, system will trigger additonal
    behaviours:
      - clean: rebuilt = 1, rebuild object, libs and executables
      - update: rebuilt = 2, rebuild libs and executables
      - modify: rebuilt = 3, compile only the necessary files and
                                               rebuild libs and executables
      - system (or nothing): include all modules
    Will also read the specif option and build the list of modules
    according to the stwiches, i.e. parallel, openmi, xdmf, etc.

    @param cfg_dict (dictionary) Globals configuration
    @param modules (dictionary) modules information

    @return user_list (string)  modules list
    @return type_build (integer) options for the switches
                               such as parallel, openmi, mumps, etc.
    """
    # ~~ Loads To-Be Compiled ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    type_build = 0
    user_list = 'clean ' + ' '.join(modules.keys())
    if 'modules' in cfg_dict:
        if cfg_dict['modules'].strip() != '':
            user_list = cfg_dict['modules']
    # ~~ Loads To-Be Compiled ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    word = r'\s*(?P<before>.*)\s*(?P<this>(\b(%s)\b))\s*(?P<after>.*)\s*\Z'
    proc = re.match(re.compile(word % ('clean'), re.I), user_list)
    if proc:
        user_list = proc.group('before') + ' ' + proc.group('after')
        type_build = 2
    proc = re.match(re.compile(word % ('update'), re.I), user_list)
    if proc:
        user_list = proc.group('before') + ' ' + proc.group('after')
        type_build = 1
    proc = re.match(re.compile(word % ('modify'), re.I), user_list)
    if proc:
        user_list = proc.group('before') + ' ' + proc.group('after')
        type_build = 3
    # ~~ Remove unwanted ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    word = r'(?P<before>.*?)\s*?-(?P<this>(%s)\b)\s*(?P<after>.*)\s*\Z'
    for mod in list(modules.keys()):
        proc = re.match(re.compile(word % (mod), re.I), user_list)
        if proc:
            user_list = proc.group('before') + ' ' + proc.group('after')
            del modules[mod]
    # ~~ Check forgotten ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for mod in user_list.split():
        if '-' == mod[0:1]:
            raise TelemacException(\
                    '\nCould not find the following module: '
                    '{}'.format(mod[1:]))
    # ~~ Deal with all ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if 'system' in user_list:
        user_list = ' '.join(modules.keys())

    return user_list, type_build


def parse_validation_ranks(user_list):
    """
         Read the list of user defined ranks used as a filter of the
         validation test case directory names in the form rnn_*,
         where r takes values from 0 to 9 inclusive. Special keywords
         such as all, >, or < will trigger additonal behaviours:
           - all: includes all numbers from 0 to 9
           - <n: includes all numbers below n
           - >n: includes all numbers above n
           - n: includes the number n

        @param user_list (string)  modules list

        @return ranks (list) user list of ranks to filter the list of validation
                            cases
    """

    # ~~ 'all' in key ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    word = r'(?P<before>.*)\s*(?P<this>(\b(%s)\b))\s*(?P<after>.*)\s*\Z'
    proc = re.match(re.compile(word % ('all'), re.I), user_list)
    if proc:
        return range(10)

    ranks = []
    # ~~ '<' and '>' in key ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    word = \
     r'(?P<before>.*?)\s*(?P<symbol>([><]|))(?P<this>\d\b)\s*(?P<after>.*)\s*\Z'
    while 1:
        tmp_ul = user_list
        proc = re.match(re.compile(word, re.I), user_list)
        if proc:
            if proc.group('symbol') == '<':
                ranks.extend(range(0, int(proc.group('this'))))
            elif proc.group('symbol') == '>':
                ranks.extend(range(int(proc.group('this')) + 1, 10))
            else:
                ranks.append(int(proc.group('this')))
            user_list = proc.group('before') + proc.group('after')
        if tmp_ul == user_list:
            break

    # ~~ sorting out duplicates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # there must be a better way (with tuples maybe ?)
    for irank in range(10):
        while ranks.count(irank) > 1:
            ranks.remove(irank)

    return sorted(ranks)

