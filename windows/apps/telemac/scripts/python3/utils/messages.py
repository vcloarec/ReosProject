r"""
    @note ... this work is based on a collaborative effort
    of the Telemac-Mascaret consortium

    @history 05/06/2012 -- Sebastien E. Bourban
        First draft

    @history 18/06/2012 -- Sebastien E. Bourban
        Final solution for the first implementation of the utility.
        Calls to sys.exit() and os.system() have been progressively captured
            into a try/except statement to better manage errors.
        This, however, assumes that all errors are anticipated.

    @history 05/12/2012 -- Sebastien E. Bourban
    Addition of a better capture of errors, particularly when the error is
        not thrown through run_cmd.

    @brief
        Catching and reporting on sys.exit / os.system errors
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
import traceback
import threading
import subprocess as sp
import re
from multiprocessing import Process, cpu_count, active_children
from multiprocessing.sharedctypes import Value, Array

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#


def filter_message(dic, excpt=None, bypass=True):
    """
    Add error in message dict.
    @param dic (dictionary) information to filter
    @param excpt (object) exception object
    @param bypass (boolean) continue with raise exception
    @return (dictionary) information for display
    """
    # ~~> case with an exception
    if excpt is not None:
        c_d = dic.copy()   # assumes dic is a dict and excpt is an Exception
        if isinstance(excpt.args, type(())):
            message = []
            if isinstance(excpt.args[0], type([])):
                for i in excpt.args:
                    message.extend(i)
                c_d.update({'tree': message})
            elif isinstance(excpt.args[0], type({})):
                for i in excpt.args:
                    message.append(i)
                c_d.update({'tree': message})
            else:
                c_d = {'name': 'uncontroled error from python:',
                       'msg': repr(excpt) + '\n' + '~' * 18 + '\n'
                       + ''.join(traceback.format_exception(*sys.exc_info()))
                       + '~' * 18}
                if 'name' in dic:
                    c_d['name'] = dic['name']+':\n      '+c_d['name']
                if 'msg' in dic:
                    c_d['msg'] = dic['msg']+':\n      '+c_d['msg']
            if bypass:
                return c_d
            raise Exception(repr_message([c_d]))

        if isinstance(excpt.args, type([])):
            message = []
            for i in excpt.args:
                message.append(i)
            c_d.update({'tree': message})
            raise Exception(c_d)  # > This should never happend

        c_d.update({'tree': [repr(excpt.args)]})
        raise Exception(c_d)  # > This should never happend
    # ~~> case without
    if isinstance(dic, type({})):
        if bypass:
            return dic.copy()
        raise Exception(repr_message([dic]))
    c_d = {'name': 'uncontroled error from python:',
           'msg': repr(dic)}
    raise Exception(c_d)  # > This should never happend or maybe ?


def repr_message(items):
    """
    Dict to string for message
    @param items (dictionary) items dict to add to the message
    @return (string) message to display
    """
    message = []
    for item in items:
        if isinstance(item, type({})):
            m_i = "_"*len(item['name'])+"\n"+item['name'] + ':\n'
            if 'msg' in item:
                m_i = m_i + ' ' + item['msg']
            if 'tree' in item:
                m_e = repr_message(item['tree'])
                m_i = m_i + '\n   |' + '\n   |'.join(m_e.split('\n'))
            message.append(m_i)
        else:
            raise Exception(items)  # > This should not happend
    return '\n'.join(message)

# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#


class Messages():
    """
    Managment messages class
    """

    def __init__(self, size=0, ncsize=0):
        """
        Initialization of messages class
        @param size (integer) size
        @param ncsize (integer) cpu number
        """
        self.messages = []
        self.tail = ''
        self.size = size
        self.ncsize = ncsize
        if ncsize == 0:
            self.ncsize = cpu_count()

    def add_messages(self, m_s):
        """
        Add item in message
        @param m_s (dictionary) item
        @return None
        """
        for item in m_s:
            self.messages.append(item)  # because of tuple to array

    def except_messages(self):
        """
        Display the message dictionary
        @return (string) message
        """
        return repr_message(self.messages)

    def not_empty(self):
        """
        Check if messages is no null
        @return (boolean)
        """
        return self.messages != []

    def start_cmd(self, tasks, args, memo):
        """
        Start task
        @param tasks (list) task list
        @param args (tuple)  task arguments
        @param memo (string) ?????todo
        @return task (Thread Objects)
        """
        # ~~> prevents from overloading your system
        while len(active_children()) >= self.ncsize:
            continue
        # ~~> subcontract the task out
        task = ParaProcess(self.run_cmd, args)
        # ~~> update the log pile
        tasks.append([task, args, memo])
        # ~~> order the work
        task.start()
        # ~~> return to high grounds
        return task

    def flush_cmd(self, tasks):
        """
        TODO ???????
        @param tasks (list) task list
        @return messages (list)  ????todo
        """
        messages = []
        while tasks != []:
            task, (exe, _, tail, code), memo = tasks.pop(-1)
            task.join()
            if tail.value.strip() != b'':
                self.clear_cmd(tasks)
            messages.append([exe, '', tail.value.strip().decode('utf-8'),
                             code.value, memo])
        return messages

    def clean_cmd(self, tasks):
        """
        Clean the tasks
        @param tasks (list) task list
        @return messages (list)
        """
        i = len(tasks)
        messages = []
        while i > 0:
            task, (exe, _, tail, code), memo = tasks[i-1]
            if not task.is_alive():
                tasks.pop(i-1)
                task.join()
                if tail.value.strip() != b'':
                    messages.append([exe, '',
                                     tail.value.strip().decode('utf-8'),
                                     code.value, memo])
                    self.clear_cmd(tasks)
                    break
                messages.append([exe, '', '', code.value, memo])
            i -= 1
        return messages

    def clear_cmd(self, tasks):
        """
        Terminate all task
        @param tasks (list)
        """
        while tasks != []:
            task, _, _ = tasks.pop()
            task.terminate()

    def run_cmd(self, exe, bypass,
                tail=Array('c', b' '*10000), code=Value('i', 0)):
        """
        Run executable
        @param exe (string) executable
        @param bypass (boolean) continue with raise exception
        @param tail (c_char) intial message
        @param code (c_int) initial status process
        @return tail (c_char) output message
        @return code.value (integer) status process
        """
        if bypass:
            proc = sp.Popen(exe, bufsize=1024, stdout=sp.PIPE,
                            stderr=sp.PIPE, shell=True)
            t1 = threading.Thread(target=self.buffer_screen,
                                  args=(proc.stdout,))
            t1.start()
            t1.join()
            proc.wait()
            code.value = proc.returncode
            if code.value != 0 and tail.value.strip() == b'':
                tail.value = \
                    'I was only able to capture the following execution '\
                    'error while executing the following:\n{}'\
                    '\n... you may wish to re-run without bypass option.'\
                    '\n{}\n{}\n{}'.format(exe, '~'*18,
                                          proc.stderr.read().strip(),
                                          '~' * 18).encode('utf-8')
                self.tail += '\n{}'.format(tail.value)
        else:
            code.value = sp.call(exe, shell=True)
            if code.value != 0:
                tail.value = \
                    '... The following command failed for the reason '\
                    'in the listing\n{}\n'\
                    .format(exe).encode('utf-8')
                self.tail += '\n{}'.format(tail.value.decode('utf-8'))
        return self.tail, code.value

    def buffer_screen(self, pipe):
        """
        Add messsage tail
        @param pipe(????) todo
        @return Nothing
        """
        lastlineempty = False
        for line in iter(pipe.readline, b''):
            dat = line.rstrip()
            if dat == b'':
                if not lastlineempty:
                    self.tail += '\n{}'.format(dat)
                    lastlineempty = True
            else:
                lastlineempty = False
                self.tail += '\n{}'.format(dat)
        if len(self.tail.split('\n')) > self.size:
            self.tail = '\n'.join((self.tail.split('\n'))[-self.size:])

# _____                     ________________________________________
# ____/ Parallel Utilities /_______________________________________/
#


class ParaProcess(Process):
    """
        Equivalent to eval function
    """

    def __init__(self, fct, args):
        """
        Initialization ParaProcess Class
        @param fct (string) function
        @param args (tuple) arguments function
        """
        Process.__init__(self)
        self.fct = fct
        self.args = args

    def run(self):
        """
        Run function
        """
        self.fct(*self.args)

# _____                  ___________________________________________
# ____/ Other Utilities /__________________________________________/
#


def banner(text, size=1):
    """
    Use Letters class
    @param text (string)
    @param size (integer) one size supported for now
    @return (list) list of characters set inspired from Glenn Chappell
    """

    litterature = Letters(size)
    return litterature.render(text)


def git_banner(root_dir, version='main'):
    """
    Print a banner with the Git branch.

    @param root_dir (str) Path to the root of Telemac-Mascaret
    @param version (str) Fallback version, when using the released sources
                         or when the repository is in a detached HEAD state
    """
    mes = Messages()  # runcode takes its version number from the CAS file
    branch = ''
    try:
        tail, status = mes.run_cmd(str.format(
            'cd {} && git rev-parse --abbrev-ref HEAD', root_dir), True)
        if status == 0:
            branch = tail.split('\n')[1][2:-1]
    except Exception:
        pass
    if branch != '' and branch != 'HEAD':
        print('\n'.join(banner(branch)))
    else:
        print('\n'.join(banner(version)))


class Letters():
    """
       Translate text to characters set inspired from Glenn Chappell
    """

    def __init__(self, size=1):
        """
        Initialization function fo Letters Class
        @param size (integer) one size supported for now
        """

        self.size = size         # only one size supported for now
        self.ascii = {}
        # 95 characters of the ASCII set are supported from char 32
        for i in range(126-31):
            self.ascii.update({i+32: [chr(i+32), '']})

        # ~~> character set inspired from Glenn Chappell
        self.ascii[32][1] = ['  ',
                             '  ',
                             '  ',
                             '  ',
                             '  ',
                             '  ',
                             '  ',
                             '  ']  # >  single space
        self.ascii[33][1] = ['  _ ',
                             ' | |',
                             ' | |',
                             ' | |',
                             ' |_|',
                             ' (_)',
                             '    ',
                             '    ']  # >  !
        self.ascii[34][1] = ['  _ _ ',
                             ' ( | )',
                             '  V V ',
                             '      ',
                             '      ',
                             '      ',
                             '      ',
                             '      ']  # >  "
        self.ascii[35][1] = ['    _  _   ',
                             '  _| || |_ ',
                             ' |_  __  _|',
                             '  _| || |_ ',
                             ' |_  __  _|',
                             '   |_||_|  ',
                             '           ',
                             '           ']   # >  #
        self.ascii[36][1] = ['   _  ',
                             '  | | ',
                             ' / __)',
                             ' \\__ \\',
                             ' (   /',
                             '  |_| ',
                             '      ',
                             '      ']  # >  $
        self.ascii[37][1] = ['  _   __',
                             ' (_) / /',
                             '    / / ',
                             '   / /  ',
                             '  / / _ ',
                             ' /_/ (_)',
                             '        ',
                             '        ']  # >  %
        self.ascii[38][1] = ['         ',
                             '   ___   ',
                             '  ( _ )  ',
                             '  / _ \\/\\',
                             ' | (_>  <',
                             '  \\___/\\/',
                             '         ',
                             '         ']  # >  &
        self.ascii[39][1] = ['  _ ',
                             ' ( )',
                             ' |/ ',
                             '    ',
                             '    ',
                             '    ',
                             '    ',
                             '    ']  # >  '
        self.ascii[40][1] = ['   __',
                             '  / /',
                             ' | | ',
                             ' | | ',
                             ' | | ',
                             ' | | ',
                             '  \\_\\',
                             '       ']  # >  (
        self.ascii[41][1] = [' __  ',
                             ' \\ \\ ',
                             '  | |',
                             '  | |',
                             '  | |',
                             '  | |',
                             ' /_/ ',
                             '     ']  # >  )
        self.ascii[42][1] = ['     _    ',
                             '  /\\| |/\\ ',
                             '  \\ ` \' / ',
                             ' |_     _|',
                             '  / , . \\ ',
                             '  \\/|_|\\/ ',
                             '          ',
                             '          ']  # >  *
        self.ascii[43][1] = ['        ',
                             '    _   ',
                             '  _| |_ ',
                             ' |_   _|',
                             '   |_|  ',
                             '        ',
                             '        ',
                             '        ']  # > +
        self.ascii[44][1] = ['    ',
                             '    ',
                             '    ',
                             '    ',
                             '  _ ',
                             ' ( )',
                             ' |/ ',
                             '    ']  # > ,
        self.ascii[45][1] = ['         ',
                             '         ',
                             '  ______ ',
                             ' |______|',
                             '         ',
                             '         ',
                             '         ',
                             '         ']  # > -
        self.ascii[46][1] = ['    ',
                             '    ',
                             '    ',
                             '    ',
                             '  _ ',
                             ' (_)',
                             '    ',
                             '    ']  # > .
        self.ascii[47][1] = ['      __',
                             '     / /',
                             '    / / ',
                             '   / /  ',
                             '  / /   ',
                             ' /_/    ',
                             '        ',
                             '        ']  # > /
        self.ascii[48][1] = ['   ___  ',
                             '  / _ \\ ',
                             ' | | | |',
                             ' | | | |',
                             ' | |_| |',
                             '  \\___/ ',
                             '        ',
                             '        ']  # > 0
        self.ascii[49][1] = ['  __ ',
                             ' /_ |',
                             '  | |',
                             '  | |',
                             '  | |',
                             '  |_|',
                             '     ',
                             '     ']  # > 1
        self.ascii[50][1] = ['  ___  ',
                             ' |__ \\ ',
                             '    ) |',
                             '   / / ',
                             '  / /_ ',
                             ' |____|',
                             '       ',
                             '       ']  # > 2
        self.ascii[51][1] = ['  ____  ',
                             ' |___ \\ ',
                             '   __) |',
                             '  |__ < ',
                             '  ___) |',
                             ' |____/ ',
                             '        ',
                             '        ']  # > 3
        self.ascii[52][1] = ['  _  _   ',
                             ' | || |  ',
                             ' | || |_ ',
                             ' |__   _|',
                             '    | |  ',
                             '    |_|  ',
                             '         ',
                             '         ']  # > 4
        self.ascii[53][1] = ['  _____ ',
                             ' | ____|',
                             ' | |__  ',
                             ' |___ \\ ',
                             '  ___) |',
                             ' |____/ ',
                             '        ',
                             '        ']  # > 5
        self.ascii[54][1] = ['    __  ',
                             '   / /  ',
                             '  / /_  ',
                             ' | \'_ \\ ',
                             ' | (_) |',
                             '  \\___/ ',
                             '        ',
                             '        ']  # > 6
        self.ascii[55][1] = ['  _____ ',
                             ' |___  |',
                             '   _/ / ',
                             '  |_ _| ',
                             '   / /  ',
                             '  /_/   ',
                             '        ',
                             '        ']  # > 7
        self.ascii[56][1] = ['   ___  ',
                             '  / _ \\ ',
                             ' | (_) |',
                             '  > _ < ',
                             ' | (_) |',
                             '  \\___/ ',
                             '        ',
                             '        ']  # > 8
        self.ascii[57][1] = ['   ___  ',
                             '  / _ \\ ',
                             ' | (_) |',
                             '  \\__, |',
                             '    / / ',
                             '   /_/  ',
                             '        ',
                             '        ']  # > 9
        self.ascii[58][1] = ['    ',
                             '  _ ',
                             ' (_)',
                             '    ',
                             '  _ ',
                             ' (_)',
                             '    ',
                             '    ']  # > :
        self.ascii[59][1] = ['    ',
                             '  _ ',
                             ' (_)',
                             '    ',
                             '  _ ',
                             ' ( )',
                             ' |/ ',
                             '    ']  # > ;
        self.ascii[60][1] = ['    __',
                             '   / /',
                             '  / / ',
                             ' < <  ',
                             '  \\ \\ ',
                             '   \\_\\',
                             '      ',
                             '      ']  # > <
        self.ascii[61][1] = ['         ',
                             '  ______ ',
                             ' |______|',
                             '  ______ ',
                             ' |______|',
                             '         ',
                             '         ',
                             '         ']  # > =
        self.ascii[62][1] = [' __   ',
                             ' \\ \\  ',
                             '  \\ \\ ',
                             '   > >',
                             '  / / ',
                             ' /_/  ',
                             '      ',
                             '      ']  # > >
        self.ascii[63][1] = ['  ___  ',
                             ' |__ \\ ',
                             '    ) |',
                             '   / / ',
                             '  |_|  ',
                             '  (_)  ',
                             '       ',
                             '       ']  # > ?
        self.ascii[64][1] = ['          ',
                             '    ____  ',
                             '   / __ \\ ',
                             '  / / _` |',
                             ' | | (_| |',
                             '  \\ \\__,_|',
                             '   \\____/ ',
                             '          ']  # > @
        self.ascii[65][1] = ['           ',
                             '     /\\    ',
                             '    /  \\   ',
                             '   / /\\ \\  ',
                             '  / ____ \\ ',
                             ' /_/    \\_\\',
                             '           ',
                             '           ']  # > A
        self.ascii[66][1] = ['  ____  ',
                             ' |  _ \\ ',
                             ' | |_) |',
                             ' |  _ < ',
                             ' | |_) |',
                             ' |____/ ',
                             '        ',
                             '        ']  # > B
        self.ascii[67][1] = ['   _____ ',
                             '  / ____|',
                             ' | |     ',
                             ' | |     ',
                             ' | |____ ',
                             '  \\_____|',
                             '         ',
                             '         ']  # > C
        self.ascii[68][1] = ['  _____  ',
                             ' |  __ \\ ',
                             ' | |  | |',
                             ' | |  | |',
                             ' | |__| |',
                             ' |_____/ ',
                             '         ',
                             '         ']  # > D
        self.ascii[69][1] = ['  ______ ',
                             ' |  ____|',
                             ' | |__   ',
                             ' |  __|  ',
                             ' | |____ ',
                             ' |______|',
                             '         ',
                             '         ']  # > E
        self.ascii[70][1] = ['  ______ ',
                             ' |  ____|',
                             ' | |__   ',
                             ' |  __|  ',
                             ' | |     ',
                             ' |_|     ',
                             '         ',
                             '         ']  # > F
        self.ascii[71][1] = ['   _____ ',
                             '  / ____|',
                             ' | |  __ ',
                             ' | | |_ |',
                             ' | |__| |',
                             '  \\_____|',
                             '         ',
                             '         ']  # > G
        self.ascii[72][1] = ['  _    _ ',
                             ' | |  | |',
                             ' | |__| |',
                             ' |  __  |',
                             ' | |  | |',
                             ' |_|  |_|',
                             '         ',
                             '         ']  # > H
        self.ascii[73][1] = ['  _____ ',
                             ' |_   _|',
                             '   | |  ',
                             '   | |  ',
                             '  _| |_ ',
                             ' |_____|',
                             '        ',
                             '        ']  # > I
        self.ascii[74][1] = ['       _ ',
                             '      | |',
                             '      | |',
                             '  _   | |',
                             ' | |__| |',
                             '  \\____/ ',
                             '         ',
                             '         ']  # > J
        self.ascii[75][1] = ['  _  __',
                             ' | |/ /',
                             ' | \' / ',
                             ' |  <  ',
                             ' | . \\ ',
                             ' |_|\\_\\',
                             '       ',
                             '       ']  # > K
        self.ascii[76][1] = ['  _      ',
                             ' | |     ',
                             ' | |     ',
                             ' | |     ',
                             ' | |____ ',
                             ' |______|',
                             '         ',
                             '         ']  # > L
        self.ascii[77][1] = ['  __  __ ',
                             ' |  \\/  |',
                             ' | \\  / |',
                             ' | |\\/| |',
                             ' | |  | |',
                             ' |_|  |_|',
                             '         ',
                             '         ']  # > M
        self.ascii[78][1] = ['  _   _ ',
                             ' | \\ | |',
                             ' |  \\| |',
                             ' | . ` |',
                             ' | |\\  |',
                             ' |_| \\_|',
                             '        ',
                             '        ']  # > N
        self.ascii[79][1] = ['   ____  ',
                             '  / __ \\ ',
                             ' | |  | |',
                             ' | |  | |',
                             ' | |__| |',
                             '  \\____/ ',
                             '         ',
                             '         ']  # > O
        self.ascii[80][1] = ['  _____  ',
                             ' |  __ \\ ',
                             ' | |__) |',
                             ' |  ___/ ',
                             ' | |     ',
                             ' |_|     ',
                             '         ',
                             '         ']  # > P
        self.ascii[81][1] = ['   ____  ',
                             '  / __ \\ ',
                             ' | |  | |',
                             ' | |  | |',
                             ' | |__| |',
                             '  \\___\\_\\',
                             '         ',
                             '         ']  # > Q
        self.ascii[82][1] = ['  _____  ',
                             ' |  __ \\ ',
                             ' | |__) |',
                             ' |  _  / ',
                             ' | | \\ \\ ',
                             ' |_|  \\_\\',
                             '         ',
                             '         ']  # > R
        self.ascii[83][1] = ['   _____ ',
                             '  / ____|',
                             ' | (___  ',
                             '  \\___ \\ ',
                             '  ____) |',
                             ' |_____/ ',
                             '         ',
                             '         ']  # > S
        self.ascii[84][1] = ['  _______ ',
                             ' |__   __|',
                             '    | |   ',
                             '    | |   ',
                             '    | |   ',
                             '    |_|   ',
                             '          ',
                             '          ']  # > T
        self.ascii[85][1] = ['  _    _ ',
                             ' | |  | |',
                             ' | |  | |',
                             ' | |  | |',
                             ' | |__| |',
                             '  \\____/ ',
                             '         ',
                             '         ']  # > U
        self.ascii[86][1] = [' __     __',
                             ' \\ \\   / /',
                             '  \\ \\ / / ',
                             '   \\ V /  ',
                             '    \\ /   ',
                             '     V    ',
                             '          ',
                             '          ']  # > V
        self.ascii[87][1] = [' __         __',
                             ' \\ \\   _   / /',
                             '  \\ \\ / \\ / / ',
                             '   \\ V _ V /  ',
                             '    \\ / \\ /   ',
                             '     V   V    ',
                             '              ',
                             '            ']  # > W
        self.ascii[88][1] = [' __   __',
                             ' \\ \\ / /',
                             '  \\ V / ',
                             '   > <  ',
                             '  / . \\ ',
                             ' /_/ \\_\\',
                             '        ',
                             '        ']  # > X
        self.ascii[89][1] = [' __     __',
                             ' \\ \\   / /',
                             '  \\ \\_/ / ',
                             '   \\   /  ',
                             '    | |   ',
                             '    |_|   ',
                             '          ',
                             '          ']  # > Y
        self.ascii[90][1] = ['  ______',
                             ' |___  /',
                             '    / / ',
                             '   / /  ',
                             '  / /__ ',
                             ' /_____|',
                             '        ',
                             '        ']  # > Z
        self.ascii[91][1] = ['  ___ ',
                             ' |  _|',
                             ' | |  ',
                             ' | |  ',
                             ' | |  ',
                             ' | |_ ',
                             ' |___|',
                             '      ']  # > [
        self.ascii[92][1] = [' __     ',
                             ' \\ \\    ',
                             '  \\ \\   ',
                             '   \\ \\  ',
                             '    \\ \\ ',
                             '     \\_\\',
                             '        ',
                             '        ']  # > \
        self.ascii[93][1] = ['  ___ ',
                             ' |_  |',
                             '   | |',
                             '   | |',
                             '   | |',
                             '  _| |',
                             ' |___|',
                             '      ']  # > ]
        self.ascii[94][1] = ['  /\\ ',
                             ' |/\\|',
                             '     ',
                             '     ',
                             '     ',
                             '     ',
                             '     ',
                             '     ']  # > ^
        self.ascii[95][1] = ['         ',
                             '         ',
                             '         ',
                             '         ',
                             '         ',
                             '         ',
                             '  ______ ',
                             ' |______|']  # > _
        self.ascii[96][1] = ['  _ ',
                             ' ( )',
                             '  \\|',
                             '    ',
                             '    ',
                             '    ',
                             '    ',
                             '    ']  # > `
        self.ascii[97][1] = ['        ',
                             '        ',
                             '   __ _ ',
                             '  / _` |',
                             ' | (_| |',
                             '  \\__,_|',
                             '        ',
                             '        ']  # > a
        self.ascii[98][1] = ['  _     ',
                             ' | |    ',
                             ' | |__  ',
                             ' | \'_ \\ ',
                             ' | |_) |',
                             ' |_.__/ ',
                             '        ',
                             '        ']  # > b
        self.ascii[99][1] = ['       ',
                             '       ',
                             '   ___ ',
                             '  / __|',
                             ' | (__ ',
                             '  \\___|',
                             '       ',
                             '       ']  # > c
        self.ascii[100][1] = ['      _ ',
                              '     | |',
                              '   __| |',
                              '  / _` |',
                              ' | (_| |',
                              '  \\__,_|',
                              '        ',
                              '        ']  # > d
        self.ascii[101][1] = ['       ',
                              '       ',
                              '   ___ ',
                              '  / _ \\',
                              ' |  __/',
                              '  \\___|',
                              '       ',
                              '       ']  # > e
        self.ascii[102][1] = ['   __ ',
                              '  / _|',
                              ' | |_ ',
                              ' |  _|',
                              ' | |  ',
                              ' |_|  ',
                              '      ',
                              '      ']  # > f
        self.ascii[103][1] = ['        ',
                              '        ',
                              '   __ _ ',
                              '  / _` |',
                              ' | (_| |',
                              '  \\__, |',
                              '   __/ |',
                              '  |___/ ']  # > g
        self.ascii[104][1] = ['  _     ',
                              ' | |    ',
                              ' | |__  ',
                              ' | \'_ \\ ',
                              ' | | | |',
                              ' |_| |_|',
                              '        ',
                              '        ']  # > h
        self.ascii[105][1] = ['  _ ',
                              ' (_)',
                              '  _ ',
                              ' | |',
                              ' | |',
                              ' |_|',
                              '    ',
                              '    ']  # > i
        self.ascii[106][1] = ['    _ ',
                              '   (_)',
                              '    _ ',
                              '   | |',
                              '   | |',
                              '   | |',
                              '  _/ |',
                              ' |__/ ']  # > j
        self.ascii[107][1] = ['  _    ',
                              ' | |   ',
                              ' | | __',
                              ' | |/ /',
                              ' |   < ',
                              ' |_|\\_\\',
                              '       ',
                              '       ']  # > k
        self.ascii[108][1] = ['  _ ',
                              ' | |',
                              ' | |',
                              ' | |',
                              ' | |',
                              ' |_|',
                              '    ',
                              '    ']  # > l
        self.ascii[109][1] = ['            ',
                              '            ',
                              '  _ __ ___  ',
                              ' | \'_ ` _ \\ ',
                              ' | | | | | |',
                              ' |_| |_| |_|',
                              '            ',
                              '            ']  # > m
        self.ascii[110][1] = ['        ',
                              '        ',
                              '  _ __  ',
                              ' | \'_ \\ ',
                              ' | | | |',
                              ' |_| |_|',
                              '        ',
                              '        ']  # > n
        self.ascii[111][1] = ['        ',
                              '        ',
                              '   ___  ',
                              '  / _ \\ ',
                              ' | (_) |',
                              '  \\___/ ',
                              '        ',
                              '        ']  # > o
        self.ascii[112][1] = ['        ',
                              '        ',
                              '  _ __  ',
                              ' | \'_ \\ ',
                              ' | |_) |',
                              ' | .__/ ',
                              ' | |    ',
                              ' |_|    ']  # > p
        self.ascii[113][1] = ['        ',
                              '        ',
                              '   __ _ ',
                              '  / _` |',
                              ' | (_| |',
                              '  \\__, |',
                              '     | |',
                              '     |_|']  # > q
        self.ascii[114][1] = ['       ',
                              '       ',
                              '  _ __ ',
                              ' | \'__|',
                              ' | |   ',
                              ' |_|   ',
                              '       ',
                              '       ']  # > r
        self.ascii[115][1] = ['      ',
                              '      ',
                              '  ___ ',
                              ' / __|',
                              ' \\__ \\',
                              ' |___/',
                              '      ',
                              '      ']  # > s
        self.ascii[116][1] = ['  _   ',
                              ' | |  ',
                              ' | |_ ',
                              ' | __|',
                              ' | |_ ',
                              '  \\__|',
                              '      ',
                              '      ']  # > t
        self.ascii[117][1] = ['        ',
                              '        ',
                              '  _   _ ',
                              ' | | | |',
                              ' | |_| |',
                              '  \\__,_|',
                              '        ',
                              '        ']  # > u
        self.ascii[118][1] = ['        ',
                              '        ',
                              ' __   __',
                              ' \\ \\ / /',
                              '  \\ V / ',
                              '   \\_/  ',
                              '        ',
                              '        ']  # > v
        self.ascii[119][1] = ['           ',
                              '           ',
                              ' __      __',
                              ' \\ \\ /\\ / /',
                              '  \\ V  V / ',
                              '   \\_/\\_/  ',
                              '           ',
                              '           ']  # > w
        self.ascii[120][1] = ['       ',
                              '       ',
                              ' __  __',
                              ' \\ \\/ /',
                              '  >  < ',
                              ' /_/\\_\\',
                              '       ',
                              '       ']  # > x
        self.ascii[121][1] = ['        ',
                              '        ',
                              '  _   _ ',
                              ' | | | |',
                              ' | |_| |',
                              '  \\__, |',
                              '   __/ |',
                              '  |___/ ']  # > y
        self.ascii[122][1] = ['      ',
                              '      ',
                              '  ____',
                              ' |_  /',
                              '  / / ',
                              ' /___|',
                              '      ',
                              '      ']  # > z
        self.ascii[123][1] = ['   __',
                              '  / /',
                              '  | |',
                              ' / / ',
                              ' \\ \\ ',
                              '  | |',
                              '  \\_\\',
                              '     ']  # > {
        self.ascii[124][1] = ['  _ ',
                              ' | |',
                              ' | |',
                              ' | |',
                              ' | |',
                              ' | |',
                              ' | |',
                              ' |_|']  # > |
        self.ascii[125][1] = [' __  ',
                              ' \\ \\ ',
                              ' | | ',
                              '  \\ \\',
                              '  / /',
                              ' | | ',
                              ' /_/ ',
                              '     ']  # > }
        self.ascii[126][1] = ['      ',
                              '      ',
                              '      ',
                              '  /\\/|',
                              ' |/\\/ ',
                              '      ',
                              '      ',
                              '      ']  # >  ~

    def render(self, text):
        """
        Translate text to characters set inspired from Glenn Chappell

        @param text (string) text to translate with the letters
        @return lines (list) list of characters set inspired from Glenn Chappell
        """
        lines = [' ' for i in self.ascii[32][self.size]]
        for char in text:
            i = ord(char)
            for j in range(len(self.ascii[i][self.size])):
                lines[j] += self.ascii[i][self.size][j][1:]
        return lines
