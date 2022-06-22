r"""
    @note ... this work is based on a collaborative effort
    of the Telemac-Mascaret consortium

    @history 15/11/2011 -- Sebastien E. Bourban
            Addition of diff_text_files, a differential tool, which is
            called in the main.

    @history 15/11/2011 -- Sebastien E. Bourban
            Addition of move_file.

    @history 15/11/2011 -- Sebastien E. Bourban
            Addition of a progress bar to the put_file_content and
            add_file_content methods -- had to write by line, instead of just
            one.

    @history 04/06/2012 -- Fabien Decung
            Extension of get_these_files to include subdirectories of source/.

    @history 31/05/2012 -- Sebastien E. Bourban
            Addition of a simple unzipping method (unzip)

    @history 15/04/20124-- Sebastien E. Bourban
            function is_newer now processes files within a directory also.

    @brief
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import shutil
import time
import difflib
import codecs
from os import path, walk, mkdir, getcwd, chdir, remove, rmdir, listdir,\
                    stat, makedirs
try:
    from os import symlink
    SYMLINK_AVAIL = True
except ImportError:
    SYMLINK_AVAIL = False
import errno
from fnmatch import filter, fnmatch
import zipfile
from distutils.archive_util import make_archive
from distutils.dep_util import newer
from urllib.request import urlopen
from urllib.error import HTTPError

# ~~> dependencies towards other modules
from utils.progressbar import ProgressBar
from utils.exceptions import TelemacException

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#


def check_sym_link(use_link):
    """
    Checking if symlink is available
    @param use_link (boolean) option link instead of copy in the temporary
                              folder
    @return (boolean)
    """
    return SYMLINK_AVAIL and use_link


# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def recursive_glob(treeroot, pattern):
    """
    Returns list of files matching pattern in all subdirectories of treeroot
    (to avoid usage of glob.glob with recursive argument which is not suppoted
     by Python >3.5)

    @param treeroot (str) Path of folder
    @param pattern (str) Pattern to search for
    """
    results = []
    for base, _, files in walk(treeroot):
        goodfiles = filter(files, pattern)
        results.extend(path.join(base, f) for f in goodfiles)
    return results


def add_to_list(lst, name, value):
    """
    Add item to dictionary

    @param lst (dictionnary) list
    @param name (any) key
    @param value (any) adding value

    @return (dictionary) updating lst
    """
    if lst.get(name) is not None:
        if value not in lst[name]:
            lst[name].append(value)
    else:
        lst.update({name: [value]})
    return lst


def get_these_files(root, exts):
    """@brief Make a list of all files in root that have
        the extension in [ext]
        -- Return the list

        @param root (string) path root
        @param exts (string) extension file

        @return (list) files list
    """
    root = root.strip()
    files = []
    if path.exists(root):
        # @note FD@EDF : allow scan of subdirectories...
        # @note SEB@HRW : there must be aother way -- walk is too long
        dirpath, _, filenames = next(walk(root))
        for fle in filenames:
            for ext in exts:
                _, tail = path.splitext(fle)
                if tail.lower() == ext.lower():
                    files.append(path.join(dirpath, fle))
    return files


def is_newer(nfile, ofile):
    """
        Evaluate whether one file is more recent than the other
        Return 1 is ofile exists and is more recent than nfile, 0 otherwise
        > newer(ofile,nfile) is True if ofile exists and is more recent than
        nfile

        @param nfile (string) new file
        @param ofile (string) old file or directory

        @return (integer) Return 1 is ofile exists and
        is more recent than nfile, 0 otherwise
    """
    # TODO: use of newer_group for the second part of the test.
    i = 1
    if path.isfile(ofile):
        if newer(ofile, nfile):
            i = 0
    elif path.isdir(ofile):
        for fle in listdir(ofile):
            if path.isfile(path.join(ofile, fle)):
                if newer(path.join(ofile, fle), nfile):
                    i *= 0
    return 1 - min(i, 1)


def get_file_content(fle):
    """
    Read fle file

    @param fle (string) file

    @return ilines (list) content line file
    """
    ilines = []
    src_file = codecs.open(fle, 'r', encoding='utf-8')
    for line in src_file:
        ilines.append(line)
    src_file.close()
    return ilines


def put_file_content(fle, lines):
    """
    put line to file

    @param fle (string) file
    @param lines (string) adding line
    """
    if path.exists(fle):
        remove(fle)
    src_file = open(fle, 'wb')
    if len(lines) > 0:
        ibar = 0
        pbar = ProgressBar(maxval=len(lines)).start()
        src_file.write(bytes((lines[0].rstrip()).replace('\r', '')\
                                                .replace('\n\n', '\n'),
                             'utf-8'))
        for line in lines[1:]:
            pbar.update(ibar)
            ibar += 1
            src_file.write(bytes('\n'+(line.rstrip()).replace('\r', '')\
                                                     .replace('\n\n', '\n'),
                                 'utf-8'))
        pbar.finish()
    src_file.close()


def add_file_content(fle, lines):
    """
    Add line to file

    @param fle (string) file
    @param lines (string) adding line
    """
    src_file = open(fle, 'ab')
    ibar = 0
    pbar = ProgressBar(maxval=len(lines)).start()
    for line in lines[0:]:
        ibar += 1
        pbar.update(ibar)
        src_file.write(bytes('\n'+(line.rstrip()).replace('\r', '')\
                                                 .replace('\n\n', '\n'),
                             'utf-8'))
    pbar.finish()
    src_file.close()


def create_directories(p_o):
    """
    create  directories tree

    @param p_o (string) directory
    """
    p_r = p_o
    p_d = []
    while not path.isdir(p_r):
        p_d.append(path.basename(p_r))
        p_r = path.dirname(p_r)
    while p_d != []:
        p_r = path.join(p_r, p_d.pop())
        mkdir(p_r)


def symlink_file(src, dest):
    """ Copy a file to its destination
        @param src (string) source file
        @param dest (string) target file
    """
    # If link already exist overwrite it
    try:
        symlink(src, dest)
    except OSError as excpt:
        if excpt.errno == errno.EEXIST:
            remove(dest)
            symlink(src, dest)


def copy_files(src, dest):
    """
    Copy all the files within src
    @param src (string) source directory
    @param dest (string) target directory
    """
    l_d = listdir(src)
    ibar = 0
    pbar = ProgressBar(maxval=len(l_d)).start()
    for f in l_d:
        if path.isfile(path.join(src, f)):
            shutil.copy(path.join(src, f), dest)
        pbar.update(ibar)
        ibar += 1
    pbar.finish()

def copy_file(src, dest):
    """
    Copy one file to directory

    @param src (string) source file
    @param dest (string) target directory
    """
    if path.exists(path.join(dest, path.basename(src))):
        remove(path.join(dest, path.basename(src)))
    if path.isfile(src):
        shutil.copy(src, dest)

def copy_file2file(src, dest):
    """
    Copy one file to file

    @param src (string) source file
    @param dest (string) target file
    """
    if path.exists(path.join(path.dirname(dest), path.basename(src))):
        remove(path.join(path.dirname(dest), path.basename(src)))
    if path.isfile(src):
        shutil.copy(src, dest)

def move_file(src, dest):
    """
    Move file to directory

    @param src (string) source file
    @param dest (string) target directory
    """
    if path.exists(path.join(dest, path.basename(src))):
        try:
            remove(path.join(dest, path.basename(src)))
        except BaseException:
            time.sleep(5)  # /!\ addition for windows operating system
            try:
                remove(path.join(dest, path.basename(src)))
            except BaseException as excpt:
                raise TelemacException(\
                    'I could not remove your existing file: '+src)
    if path.exists(src):
        try:
            shutil.move(src, dest)
        except BaseException:
            time.sleep(5)  # /!\ addition for windows operating system
            try:
                shutil.move(src, dest)
            except BaseException as excpt:
                raise TelemacException(\
                   'I could not move your file: ' + src
                   + '\n   ... maybe the detination exists?')

def move_file2file(src, dest):
    """
    Move file to file

    @param src (string) source file
    @param dest (string) target file
    """
    if dest == src:
        return
    if path.exists(dest):
        try:
            remove(dest)
        except BaseException:
            time.sleep(5)  # /!\ addition for windows operating system
            try:
                remove(dest)
            except BaseException:
                raise TelemacException(
                    'I could not remove your existing file: '+dest)
    if path.exists(src):
        try:
            shutil.move(src, dest)
        except BaseException:
            time.sleep(5)  # /!\ addition for windows operating system
            try:
                shutil.move(src, dest)
            except BaseException:
                raise TelemacException(
                    'I could not move your file: ' + src
                    + '\n   ... maybe the detination exists?')

def remove_directories(root):
    """
    Walk through the directory structure available from the root
    and removes everything in it, including the root

    @param root (string) directory structure to remove
    """
    for path_dir, pdirs, pfiles in walk(root, topdown=False):
        for fle in pfiles:
            remove(path.join(path_dir, fle))
        for idir in pdirs:
            try:
                rmdir(path.join(path_dir, idir))
            except BaseException:
                time.sleep(5)  # /!\ addition for windows operating system
                rmdir(path.join(path_dir, idir))
    try:
        rmdir(root)
    except BaseException:
        time.sleep(5)  # /!\ addition for windows operating system
        rmdir(root)
    return


def check_safe(src, safe, c_k):
    """
    Check

    @param src (string) file to check
    @param safe (string) directory to check
    @param c_k (integer) ???? todo

    @return (boolean) True if file exists
    """
    dest = path.join(safe, path.basename(src))
    if not path.exists(dest):
        return True
    if is_newer(src, dest) == 1 and c_k < 2:
        return False
    remove(dest)
    return True


def match_safe(src, ex, safe, c_k):
    """
    ???????? todo

    @param src (string) file to check
    @param ex (string) pattern file to check
    @param safe (string) directory to check
    @param c_k (integer) ???? todo

    @return (boolean) True if file exists
    """
    # ~~> list all entries
    dir_p, _, filenames = next(walk(safe))
    if filenames == []:
        return True
    # ~~> match expression
    exnames = []
    for dest in filenames:
        if fnmatch(dest, ex):
            if c_k > 1:  # TODO: try except if file access not granted
                try:
                    remove(path.join(dir_p, dest))
                except BaseException:
                    time.sleep(5)  # /!\ addition for windows operating system
                    try:
                        remove(path.join(dir_p, dest))
                    except Exception:
                        raise TelemacException(
                            'I could not remove your existing file: '
                            + path.join(dir_p, dest))
                continue
            exnames.append(path.join(dir_p, dest))
    if exnames == []:
        return True
    # ~~> check if newer files
    found = False
    for dest in exnames:
        if is_newer(src, dest) == 0:
            found = True
    if not found:
        return False
    # ~~> remove all if one older
    for dest in exnames:
        remove(dest)
    return True

# _____                  ___________________________________________
# ____/ Archive Toolbox /__________________________________________/
#


def tel_zip(zname, bname, form):
    """
    bname is a the root directory to be archived --
    Return the name of the archive, zname, with its full path --
    form can be either 'zip', 'gztar' ... read from the config file

    @param zname (string) archive name
    @param bname (string) file or directory to archive
    @param form (string) archive format

    @return zipfile (string) name of the archive, zname, with its full path
    """
    cpath = getcwd()
    chdir(path.dirname(bname))
    zip_file = make_archive(zname, form, base_dir=path.basename(bname))
    chdir(cpath)
    return zip_file


def zipsortie(sortie):
    """
    zip files and remove virtually all of them !

    @param sortie (string) output to archive

    @return zname (string) name of the archive
    """
    head, tail = path.splitext(path.basename(sortie))
    zname = head + '.zip'
    if path.dirname(sortie) != '':
        for dirname, _, filenames in walk(path.dirname(sortie)):
            break
    else:
        for dirname, _, filenames in walk('.'):
            break
    cpath = getcwd()
    chdir(dirname)
    z = zipfile.ZipFile(zname, 'a', compression=zipfile.ZIP_DEFLATED,
                        allowZip64=True)
    for filename in filenames:
        if head == filename[:len(head)] and tail == path.splitext(filename)[1]:
            z.write(filename)
            if filename != path.basename(sortie):
                remove(filename)
    chdir(cpath)
    return zname


def unzip(zip_name, bname):
    """
    bname is a the root directory where the archive is to be extracted --

    @param zip_name (string) archive file
    @param bname (string) target directory
    """
    z = zipfile.ZipFile(path.realpath(zip_name), 'r')
    cpath = getcwd()
    chdir(bname)
    for f in z.namelist():
        if f.endswith('/'):
            if not path.exists(f):
                makedirs(f)
        else:
            z.extract(f)
    chdir(cpath)

# _____               ______________________________________________
# ____/ Diff Toolbox /_____________________________________________/
#


def diff_text_files(f_file, t_file, options):
    """ Command line interface to provide diffs in four formats:

    * ndiff:    lists every line and highlights interline changes.
    * context:  highlights clusters of changes in a before/after format.
    * unified:  highlights clusters of changes in an inline format.
    * html:     generates side by side comparison with change highlights.

    @param f_file (string)
    @param t_file (string)
    @param options (string)

    @return (str)
    """
    # we're passing these as arguments to the diff function
    f_date = time.ctime(stat(f_file).st_mtime)
    t_date = time.ctime(stat(t_file).st_mtime)
    f_lines = get_file_content(f_file)
    t_lines = get_file_content(t_file)

    if options.unified:
        return difflib.unified_diff(f_lines, t_lines,
                                    f_file, t_file, f_date, t_date,
                                    n=options.ablines)
    if options.ndiff:
        return difflib.ndiff(f_lines, t_lines)
    if options.html:
        return difflib.HtmlDiff().make_file(f_lines, t_lines,
                                            f_file, t_file,
                                            context=options.context,
                                            numlines=options.ablines)
    return difflib.context_diff(f_lines, t_lines,
                                f_file, t_file, f_date, t_date,
                                n=options.ablines)

# _____                      _______________________________________
# ____/ Internet connection /______________________________________/
#


def is_online(url='http://www.opentelemac.org/', timeout=5):
    """
    Check url
    @param url (string) url
    @param timeout (integer) timeout
    @return (bool)
    """
    try:
        urlopen(url, timeout=timeout)
        return True
    except HTTPError:
        print("... could not connect through to the internet.")
    return False

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#


__author__ = "Sebastien Bourban"
__date__ = "$19-Jul-2010 08:51:29$"
