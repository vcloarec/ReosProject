r"""@author Sebastien E. Bourban and Noemie Durand

    @brief All the functions around the compilation
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
import configparser
from os import sep
from utils.files import put_file_content
from utils.exceptions import TelemacException
# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#
def get_scan_content(fle, root, bypass):
    """
     Title: Parser of the cmdf file.
     brief :File names and path are neutrally separated with the | separator.
    The cmdf file has a primary section called [general] with the following
    mandatory keys:
    [general]
     - path: <root>|sources|artemis, or <root>|sources|utils|bief
         the location of the source file, which is top of the tree of calls
         also the location of the cmdf file itself
     - module: artemis, or partel, or passive
         the module name used in the cfg and cmdf dictionaries to refer to
         a particular module of the system, noting that one module can have
         multiple tree tops, such as artemis.f, solve.f or partel_para.f
     - liborder: special hermes parllel ... module
         the ordered list of dependencies, which will be used to compile and
         link all libraries together in order of dependencies
     - name: not so useful at the n=moment (?)
    Optionaly, the [general] section can also include reference to other
    cmdf file, considered as external, with their own compilation options.
     - extrenal: passive.solve, or the name of a module defined in the cfg
         or cmdf dictionaries.
    The [general] section is then followed by other section taking their
    names in liborder [special], [hemes], [parallel], ... [module]. Each of
    these secondary section has two keys:
    [bief]
     - path: <root>|sources|utils|bief
         the common location of all files included in the tree of calls
     - files: bief_def.f
         bief.f
         ...
         the list of files included in the tree fo calls that are in the path,
         noting the indent with one file per line, and that file names could
         also include subdirectory path, relative to the common path location

    @param fle (string) name of the cmdf file (with its path) to be parsed
    @param root (string) root of the system to replace reference to <root>
    @param bypass (boolean) whether exception raised should be bypassed or not
    @return content (dictionary) a dictionary of sections and their keys

    """
    # ~~> cmdf format follows the raw config parser standard
    cfgfile = configparser.RawConfigParser()

    # ~~> return content, as a dictionary of sections and their keys
    content = {}

    # ~~ Parse CMDF file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    try:
        cfgfile.read(fle)
    except configparser.Error as excpt:
        raise TelemacException(\
             'Could not read the required parameters in the'
             ' cmdf-scan file: ' + fle +
             '\n     ... you may have to use the --rescan option'+str(excpt))

    # ~~ Interpret [general] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if 'general' not in cfgfile:
        raise TelemacException(\
             'Could not find the general section in the '
             'cmdf-scan file: ' + fle)
    else:
        general = dict(cfgfile.items('general'))

    # ~~> mandatory keys
    if 'liborder' not in general:
        raise TelemacException(\
             'Could not find the key liborder in the '
             'general section of the cmdf-scan file:' + fle)
    general['liborder'] = general['liborder'].split()

    if 'path' not in general:
        raise TelemacException(\
                'Could not find the key path in the '
                'general section of the cmdf-scan file:' + fle)
    general['path'] = general['path'].replace('<root>', root).replace('|', sep)

    if 'module' not in general:
        raise TelemacException(\
              'Could not find the key module in the '
              'general section of the cmdf-scan file:' + fle)

    if 'name' not in general:
        raise TelemacException(\
                'Could not find the key name in the '
                'general section of the cmdf-scan file:' + fle)
    # ~~> optional keys
    # ~~> final content
    content.update({'general': general})

    # ~~ Interpret all other sections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for lib in cfgfile.sections():
        # ~~> besides general
        if lib == 'general':
            continue
        # ~~> lib has to be in liborder
        if lib not in content['general']['liborder']:
            raise TelemacException(\
                  'Found a section not in the [general] '
                  'liborder key ' + lib + ' of the cmdf-scan file:' + fle)
        # ~~> content[lib] includes all keys of lib ...
        content.update({lib: dict(cfgfile.items(lib))})
        # ~~> ... of which path (may include <root> and | separators)
        if 'path' not in content[lib]:
            raise TelemacException(\
                'Could not find the key path in the section '
                + lib + ' of the cmdf-scan file:' + fle)
        content[lib]['path'] = content[lib]['path'].replace('<root>', root) \
                                                   .replace('|', sep)
        # ~~> ... and files
        if 'files' not in content[lib]:
            raise TelemacException(\
                 'Could not find the key files in the section '
                 + lib + ' of the cmdf-scan file:' + fle)
        content[lib]['files'] = content[lib]['files'].replace('\n', ' ') \
            .replace('  ', ' ').split()
    # ~~> last check on possibly missing libs
    for lib in content['general']['liborder']:
        if lib not in content:
            raise TelemacException(\
                 'The reference ' + lib +
                 ' in the [general] liborder key is not '
                 'defined in your cmdf-scan file:' + fle)

    # ~~ retrun ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return content


def put_scan_content(fle, root, content):
    """
    Writer of the cmdf file.
    See format description at get_scan_content()

    @param fle (string) name of the cmdf file (with its path) to be saved as
    @param root (sting)  root of the system to replace reference to <root>
    @param content (dict) dictionary containing scan information
    """

    # ~~> return lines to be stored
    lines = []

    # ~~ Write-up [general] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if 'general' in content:
        lines.append('[general]' + '\n' + 'path: ' +
                     content['general']['path'].replace(root, '<root>')\
                     .replace(sep, '|') +
                     '\n' + 'module: ' + content['general']['module'])
        lines.append('liborder: ' + ' '.join(content['general']['liborder']))
        lines.append('name: ' + content['general']['name'])
        if 'external' in content['general']:
            lines.append('external: ' + content['general']['external'])

    # ~~ Write-up [sections] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for lib in sorted(content.keys()):
        if lib == 'general':
            continue
        lines.append('\n[' + lib + ']' + '\n' + 'path: ' +
                     content[lib]['path'].replace(root, '<root>')\
                     .replace(sep, '|') + '\n' +
                     'files: ' + '\n  '.join(content[lib]['files']))

    # ~~ Store to cmdf file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    put_file_content(fle, lines)

    return
