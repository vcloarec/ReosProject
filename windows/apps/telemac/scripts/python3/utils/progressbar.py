#!/usr/bin/python
r"""
    @note ... this work is based on a collaborative effort
    of the Telemac-Mascaret consortium

    @details
    This library provides a text mode progressbar. This is tipically used
    to display the progress of a long running operation, providing a
    visual clue that processing is underway.

    The ProgressBar class manages the progress, and the format of the line
    is given by a number of widgets. A widget is an object that may
    display diferently depending on the state of the progress. There are
    three types of widget:
        - a string, which always shows itself (such as "Time(s): ") ;
        - a ProgressBarWidget, which may return a diferent value every time
                  it's update method is called (such as "  3%"); and
        - a ProgressBarWidgetHFill, which is like ProgressBarWidget,
                  except it expands to fill the remaining width of the line
                  (such as "[#####   ]").

    The progressbar module is very easy to use, yet very powerful. And
    automatically supports features like auto-resizing when available.

    Since the progress bar is incredibly customizable you can specify
    different widgets of any type in any order. You can even write your own
    widgets! However, since there are already a good number of widgets you
    should probably play around with them before moving on to create your own
    widgets.
"""
from os import environ
import sys
import time
import signal

# try:
#    from abc import ABCMeta, abstractmethod
# except ImportError:
#    AbstractWidget = object
#    abstractmethod = lambda fn: fn
# else:
#    AbstractWidget = ABCMeta('AbstractWidget', (object,), {})

# FD@EDF : idea taken from Dendright from Clint module
# Only show bar in terminals by default
# (better for Jenkins console outputs, piping, logging etc.)
# However it is said that sys.stderr may not always support .isatty(),
# e.g. when it has been replaced by something that partially implements the
# File interface
try:
    HIDE_DEFAULT = not sys.stderr.isatty()
except AttributeError:  # output does not support isatty()
    HIDE_DEFAULT = True

try:
    PROMPT_MODE = 'FANCY_PROMPT' in environ
    USER = environ.get('USER')
    SMALL_MODE = USER == chr(72)+chr(50)+chr(51)+chr(57)+chr(55)+chr(51)
except:
    PROMPT_MODE = False
    SMALL_MODE = False


class UnknownLength():
    """
    This is an element of ProgressBar formatting.
    The ProgressBar object will call it's update value when an update
        is needed. It's size may change between call, but the results will
        not be good if the size changes drastically and repeatedly.
    """

    pass




class ProgressBarWidget():
    """

    This is a variable width element of ProgressBar formatting.
    The ProgressBar object will call it's update value, informing the
        width this object must the made. This is like TeX \\hfill, it will
        expand to fill the line. You can use more than one in the same
        line, and they will all have the same width, and together will
        fill the line.
    """

    def update(self, pbar):
        """
        Returns the string representing the widget.
          The parameter pbar is a reference to the calling ProgressBar,
        where one can access attributes of the class for knowing how
        the update must be made.
         At least this function must be overriden.

        @param pbar (object) pbar  ProgressBar object

        @return nothing
        """
        pass



class ProgressBarWidgetHFill():
    """
      This is a variable width element of ProgressBar formatting.
    The ProgressBar object will call it's update value, informing the
        width this object must the made. This is like TeX \\hfill, it will
        expand to fill the line. You can use more than one in the same
        line, and they will all have the same width, and together will
        fill the line.
    """

    def update(self, pbar, width):
        """
            Returns the string representing the widget.
            The parameter pbar is a reference to the calling ProgressBar,
            where one can access attributes of the class for knowing how
            the update must be made. The parameter width is the total
            horizontal width the widget must have.

            At least this function must be overriden.

            @param pbar (object) pbar  ProgressBar object

            @param width (float) total horizontal
            width the widget must have
        """
        pass


class ETA(ProgressBarWidget):
    """
        Widget for the Estimated Time of Arrival
    """

    def format_time(self, seconds):
        """
        Float time to string format
        @param seconds (float) Time in s
        @return (string)
        """
        return str(int(seconds+1))+'s'

    # ~~> Updates the widget to show the ETA or total time when finished.
    def update(self, pbar):
        """
        Update progress bar

        @param pbar (object) ProgressBar object

        @return (string) new sprogress bar
        """
        if pbar.currval == 0:
            return ' | ---s'  # 'ETA:  --:--:--'
        if pbar.finished:
            return ' | %s' % self.format_time(pbar.seconds_elapsed)
        elapsed = pbar.seconds_elapsed
        eta = elapsed * pbar.maxval / pbar.currval - elapsed
        return ' | %s' % self.format_time(eta)


class FileTransferSpeed(ProgressBarWidget):
    """
        Widget for showing the transfer speed (useful for file transfers).
    """

    def __init__(self):
        """
        Initialization of FileTransferSpeed class
        """
        self.fmt = '%6.2f %s'
        self.units = ['B', 'K', 'M', 'G', 'T', 'P']

    # ~~> Updates the widget with the current SI prefixed speed.
    def update(self, pbar):
        """
        Update progress bar element
        @param pbar (object) ProgressBar object
        @return (string) new element progress bar
        """
        if pbar.seconds_elapsed < 2e-6:
            bps = 0.0
        else:
            bps = float(pbar.currval) / pbar.seconds_elapsed
        spd = bps
        for u in self.units:
            if spd < 1000:
                break
            spd /= 1000

        return self.fmt % (spd, u+'/s')


class RotatingMarker(ProgressBarWidget):
    """
        A rotating marker for filling the bar of progress.
    """

    def __init__(self, markers='|/-\\'):
        """
        Initialization function for RotatingMarker class
        @param markers (string or updatable object) to use as a marker
        """
        self.markers = markers
        self.curmark = -1

    # ~~> An animated marker for the progress bar which defaults to appear
    #     as if it were rotating.
    def update(self, pbar):
        """
        Update progress bar element
        @param pbar (object) ProgressBar object
        @return (string) new element progress bar
        """
        if pbar.finished:
            return self.markers[0]
        self.curmark = (self.curmark + 1) % len(self.markers)
        return self.markers[self.curmark]


class Percentage(ProgressBarWidget):
    """
        Just the percentage done.
    """

    def update(self, pbar):
        """"
        Update progress bar percentage
        @param pbar (object) ProgressBar object
        @return (string) new percentage progress bar
        """
        return '%3d%%' % pbar.percentage()


class Bar(ProgressBarWidgetHFill):
    """
    The bar of progress. It will strech to fill the line.
        marker - string or updatable object to use as a marker
        left - string or updatable object to use as a left border
        right - string or updatable object to use as a right border
        fill - character to use for the empty part of the progress bar
        fill_left - whether to fill from the left or the right
    """

    def __init__(self, marker='#', left='|', right='|'):
        """
        Initialization function for Bar class
        @param marker (string or updatable object) to use as a marker
        @param left (string or updatable object) to use as a left border
        @param right (string or updatable object) use as a right border
        """
        self.marker = marker
        self.left = left
        self.right = right

    def _format_marker(self, pbar):
        """

        @param pbar (object) ProgressBar object
        @return marker( basestring or string)
        """
        found = isinstance(self.marker, str)
        if found:
            return self.marker
        return self.marker.update(pbar)

    # ~~> Updates the progress bar and its subcomponents
    def update(self, pbar, width):
        """
        Update progress bar
        @param pbar (object) ProgressBar object
        @param width (float) width of the progress bar
        @return (string) new percentage progress bar
        """
        percent = pbar.percentage()
        cwidth = width - len(self.left) - len(self.right)
        marked_width = int(percent * cwidth / 100)
        if SMALL_MODE:
            marked_width = 1
        m = self._format_marker(pbar)
        if PROMPT_MODE:
            ibar = (self.left + (m*(marked_width-1)+'D').ljust(cwidth)
                    + self.right)
        else:
            ibar = (self.left + (m*marked_width).ljust(cwidth) + self.right)
        return ibar

class ReverseBar(Bar):
    """
        The reverse bar of progress, or bar of regress. :)
    """

    def update(self, pbar, width):
        """
        Update progress bar
        @param pbar (object) ProgressBar object
        @param width (float) width of the progress bar
        @return (string) new percentage progress bar
        """
        percent = pbar.percentage()
        cwidth = width - len(self.left) - len(self.right)
        marked_width = int(percent * cwidth / 100)
        m = self._format_marker(pbar)
        ibar = (self.left + (m*marked_width).rjust(cwidth) + self.right)
        return ibar


if PROMPT_MODE:
    DEFAULT_WIDGETS = [Bar(marker='=', left='8', right=' '), ' ', Percentage(),
                       ' ', ETA()]
else:
    DEFAULT_WIDGETS = [Bar(marker='\\', left='[', right=']'), ' ', Percentage(),
                       ' ', ETA()]


DEFAULT_SUBWIDGETS = [Bar(marker='.', left='[', right=']')]


class ProgressBar():
    """
        The ProgressBar class which updates and prints the bar.

         A common way of using it is like:
              pbar = ProgressBar().start()
              for i in range(100):
         ...    # do something
         ...    pbar.update(i+1)
              pbar.finish()

         You can also use a ProgressBar as an iterator:
              progress = ProgressBar()
              for i in progress(some_iterable):
         ...    # do something

         The term_width parameter represents the current terminal width. If the
         parameter is set to an integer then the progress bar will use that,
         otherwise it will attempt to determine the terminal width falling
          back to 80 columns if the width cannot be determined.

         When implementing a widget's update method you are passed a reference
         to the current progress bar. As a result, you have access to the
         ProgressBar's methods and attributes. Although there is nothing
         preventing you from changing the ProgressBar you should treat it as
         read only.

         Useful methods and attributes include:
             - currval: current progress (0 <= currval <= maxval)
             - maxval: maximum (and final) value
             - finished: True if the bar has finished (reached 100%)
             - start_time: the time when start() method of ProgressBar
             - seconds_elapsed: seconds elapsed since start_time and last call
                                    to update
             - percentage(): progress in percent [0..100]
    """

    def __init__(self, maxval=100, widgets=DEFAULT_WIDGETS, term_width=None,
                 f_d=sys.stderr):
        assert maxval > 0
        self.maxval = maxval
        self.widgets = widgets
        self.f_d = f_d
        self.signal_set = False
        self.term_width = 79
        if term_width is None:
            try:
                self.handle_resize(None, None)
                signal.signal(signal.SIGWINCH, self.handle_resize)
                self.signal_set = True
            except Exception:
                self.term_width = 79
        else:
            self.term_width = term_width

        self.currval = 0
        self.finished = False
        self.prev_percentage = -1
        self.start_time = None
        self.seconds_elapsed = 0

    def handle_resize(self, signum, frame):
        pass

    def percentage(self):
        """
        @return Returns the percentage of the progress.
        """
        return self.currval*100.0 / self.maxval

    def _format_widgets(self):
        res = []
        hfill_inds = []
        num_hfill = 0
        currwidth = 0
        for i, w in enumerate(self.widgets):
            found = False
            try:
                found = isinstance(w, basestring)
            except Exception:
                found = isinstance(w, str)
            if isinstance(w, ProgressBarWidgetHFill):
                res.append(w)
                hfill_inds.append(i)
                num_hfill += 1
            elif found:
                res.append(w)
                currwidth += len(w)
            else:
                weval = w.update(self)
                currwidth += len(weval)
                res.append(weval)
        for i_w in hfill_inds:
            res[i_w] = res[i_w].update(\
                    self, int((self.term_width-currwidth)/num_hfill))
        return res

    def _format_line(self):
        return ''.join(self._format_widgets()).ljust(self.term_width)

    def _need_update(self):
        return int(self.percentage()) != int(self.prev_percentage)

    def update(self, value, carriage='\r'):
        "Updates the progress bar to a new value."
        assert 0 <= value <= self.maxval
        self.currval = value
        if not self._need_update() or self.finished:
            return
        if not self.start_time:
            self.start_time = time.time()
        self.seconds_elapsed = time.time() - self.start_time
        self.prev_percentage = self.percentage()
        if not HIDE_DEFAULT:
            if value != self.maxval:
                self.f_d.write(self._format_line() + carriage)
            else:
                self.finished = True
                # /!\ remove the progress bar from display
                self.f_d.write(' '*79+'\r')

    def write(self, string, value):
        "Move the progress bar along."
        self.f_d.write(' '*79+'\r')
        self.f_d.write(string+'\n')
        if not self.start_time:
            self.start_time = time.time()
        self.seconds_elapsed = time.time() - self.start_time
        self.prev_percentage = self.percentage()
        if value != self.maxval:
            self.f_d.write(self._format_line() + '\r')
        else:
            self.finished = True
            self.f_d.write(' '*79+'\r')
            # /!\ remove the progress bar from display

    def trace(self):
        """
            Leave a trace on screen of the progress bar and carry on
            to the next line.
        """
        if self.currval > 0:
            self.f_d.write(self._format_line() + '\n')

    def start(self):
        """ Start progress bar """
        # ~~> Start measuring time, and prints the bar at 0%.
        self.update(0)
        return self

    def finish(self):
        """ End progress bar """
        # ~~> Used to tell the progress is finished.
        self.update(self.maxval)
        if self.signal_set:
            signal.signal(signal.SIGWINCH, signal.SIG_DFL)


class SubProgressBar(ProgressBar):

    def __init__(self, maxval=100):
        ProgressBar.__init__(self, maxval=maxval, widgets=DEFAULT_SUBWIDGETS)

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#


__author__ = "Nilton Volpato"
__date__ = "$07-May-2006 08:51:29$"
