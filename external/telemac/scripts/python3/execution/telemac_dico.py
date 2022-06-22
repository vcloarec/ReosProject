"""
Class for Telemac dictionary file manipulation
"""
from collections import OrderedDict
import re

from execution.tools import ENTRY_QUOTE, EXIT_SQUOTE, EXIT_DQUOTE,\
                            KEY_EQUALS, DICO_KEYS, EMPTY_LINE, VAL_EQUALS, \
                            convert_to_type
from utils.exceptions import TelemacException

# Global variable containing parsed dictionaries
DICOS = {}


class TelemacDico():
    """
    Class to manipulation a Telemac-Mascaret dictionary
    """

    def __init__(self, file_name):
        """
        Init function

        @param file_name (string) Name of the dictionary
        """
        self.file_name = file_name
        self.data = OrderedDict()
        self.fr2gb = {}
        self.gb2fr = {}
        self.rub_fr2gb = {}
        self.rub_gb2fr = {}
        self.dict_corresp_pathnode_key = {}
        self._scan_dico()

    def _scan_dico(self):
        """
        Scan the dictionnary and set default values
        """
        keylist = []
        with open(self.file_name, 'r', encoding="utf-8") as f:
            dico_lines = f.readlines()
        # Cleaning up dictonary
        # ~~ buddle continuations (long strings) and remove comments and empty
        # lines
        core = []
        i = -1
        while i < len(dico_lines) - 1:
            i = i + 1
            line = ''
            ll = dico_lines[i].strip()
            # Empty line
            proc = re.match(EMPTY_LINE, ll)
            if proc:
                continue
            # Comment line or special keyword
            if ll[0] == '/' or ll[0] == '&':
                continue
            # Beginning of a string if not 'after' will be empty
            proc = re.match(ENTRY_QUOTE, ll)
            line = proc.group('before')
            ll = proc.group('after').strip()
            # Merging the whole string into one line
            # TODO: Do not merge AIDE* keep the linebreaks
            while ll != '':
                # Double quote string "....."
                if ll[0:1] == '"':
                    proc = re.match(EXIT_DQUOTE, ll+' ')
                    if proc:
                        # Replace single quote inside string by double one
                        line += "'" + proc.group('before').replace("'", '"') \
                                + "'"
                        # TODO: See if the lines below are useful
                        proc = re.match(ENTRY_QUOTE,
                                        proc.group('after').strip())
                        line += proc.group('before')
                        ll = proc.group('after').strip()
                    else:
                        i = i + 1
                        ll = ll + ' ' + dico_lines[i].strip()
                # Single quote string ('.....')
                elif ll[0:1] == "'":
                    proc = re.match(EXIT_SQUOTE, ll+' ')
                    if proc:
                        # Replace single quote inside string by double one
                        line += "'" + proc.group('before').replace("'", '"') \
                                + "'"
                        proc = re.match(ENTRY_QUOTE,
                                        proc.group('after').strip())
                        line += proc.group('before')
                        ll = proc.group('after').strip()
                    else:
                        i = i + 1
                        ll = ll + ' ' + dico_lines[i].strip()
            # Adding new merged line
            core.append(line)

        # Mergin all lines into a one line
        dico_stream = (' '.join(core)).replace('  ', ' ').replace('""', '"')
        # ~~ Identify key values for each keyword

        while dico_stream != '':
            # ~~ key
            proc = re.match(KEY_EQUALS, dico_stream)
            keyword = proc.group('key').strip()
            if keyword not in DICO_KEYS:
                raise TelemacException(
                 'unknown key {} for {} '.format(keyword, proc.group('after')))

            dico_stream = proc.group('after')    # still hold the separator
            # ~~ val
            proc = re.match(VAL_EQUALS, dico_stream)
            if not proc:
                raise TelemacException('no value to keyword '+keyword)
            val = []
            # Finding values (loop for multiple values ; separated)
            while proc:
                # If string removing single quote beofre and after value
                if proc.group('val')[0] == "'":
                    val.append(proc.group('val')[1:-1])
                else:
                    val.append(proc.group('val'))
                dico_stream = proc.group('after')    # still hold the separator
                proc = re.match(VAL_EQUALS, dico_stream)
            keylist.append([keyword, val])

        if 'PATHNODE' in [duet[0] for duet in keylist]:
            self.dico_mascaret = True
        else:
            self.dico_mascaret = False

        # ~~ Group pairs of keyword/val by each occurence of NOM
        while keylist != []:
            if keylist[0][0] != 'NOM' and keylist[1][0] != 'NOM1':
                raise TelemacException('could not read NOM or NOM1 '
                                       'from {}'.format(keylist[0][1]))

            fr_name = keylist[0][1][0].replace('"', "'")
            gb_name = keylist[1][1][0].replace('"', "'")
            self.fr2gb[fr_name] = gb_name
            self.gb2fr[gb_name] = fr_name
            self.data[gb_name] = {}
            keylist.pop(0)
            keylist.pop(0)
            while keylist != []:
                if keylist[0][0] == 'NOM':
                    break
                val = keylist[0][1]
                self.data[gb_name][keylist[0][0]] = val
                keylist.pop(0)
        # converting each key value to its proper format
        for keyword, key_info in self.data.items():
            key_info['TYPE'] = key_info['TYPE'][0]
            for key in key_info:
                if key == 'TYPE':
                    continue

                if key in ['DEFAUT', 'DEFAUT1']:
                    key_info[key] = convert_to_type(key_info['TYPE'],
                                                    key_info[key])
                elif key in ['INDEX', 'NIVEAU', 'TAILLE']:
                    key_info[key] = convert_to_type('INTEGER',
                                                    key_info[key])
                elif key in ['CHOIX', 'CHOIX1']:
                    # We have two type of choix integer or string
                    # The integer ones follow this syntax for each value:
                    # index="comment"
                    # Integer type
                    if '=' in key_info[key][0]:
                        values = key_info[key]
                        key_info[key] = {}
                        # Filling a dictionary with index:comment as
                        # index/values
                        for val in values:
                            str_index, comment = val.split('=', maxsplit=1)
                            key_info[key][str_index.strip(' ')] = \
                                comment.strip('"')
                    else:
                        # List of strings just removing quotes for each value
                        key_info[key] = \
                             [val.strip("'\" ") for val in key_info[key]]
                elif key in ['RUBRIQUE', 'RUBRIQUE1']:
                    # If we have only one rubrique it will return a str
                    rub = key_info[key]
                    # Checking toat first rubique is not empty
                    if rub[0] == '':
                        raise TelemacException(
                           "First Rubrique for {} is empty".format(keyword))

                    # rubrique must have 3 levels filling with empty ones
                    for i in range(3-len(rub)):
                        rub.append('')
                    key_info[key] = rub
                # AIDE*, APPARENCE, CHOIX*, COMPORT
                # COMPOSE, CONTROLE, RUBRIQUE*, TYPE, SUBMIT
                elif len(key_info[key]) == 1:
                    key_info[key] = key_info[key][0]
            if 'RUBRIQUE' not in key_info:
                raise TelemacException(
                        "Missing RUBRIQUE for {}".format(keyword))
            if 'RUBRIQUE1' not in key_info:
                raise TelemacException(
                        "Missing RUBRIQUE1 for {}".format(keyword))

        if self.dico_mascaret:
            for key in self.data:
                self.dict_corresp_pathnode_key[self.data[key]['PATHNODE'].replace(' ', '')] = key
            return

        #### Additional check on dictionary

        # Checking that keyword names are not used as rubrique
        print("  ~> Checking keyword/rubrique coherence")
        for key, data in self.data.items():
            if key in data["RUBRIQUE1"]:
                raise Exception(
                    "Keyword: {} is also a rubrique rename one of them"\
                    .format(key))

        # Checking rubrique translation ( lost in translation :) )
        for data in self.data.values():
            for lvl, rub_gb in enumerate(data["RUBRIQUE1"]):
                # If we found an empty rubrique we reached the end of the
                # rubriques for that keyword
                if rub_gb == '':
                    break

                # Checking that english rubrique matches french translation
                # Adding translation if new
                if rub_gb not in self.rub_gb2fr:
                    self.rub_gb2fr[rub_gb] = data["RUBRIQUE"][lvl]
                elif self.rub_gb2fr[rub_gb] != data["RUBRIQUE"][lvl]:
                    raise TelemacException(\
                         "Two french translations found for {}:\n"
                         "{} and {}".format(rub_gb,
                                            data["RUBRIQUE"][lvl],
                                            self.rub_gb2fr[rub_gb]))

                # Checking that french rubrique matches english translation
                rub_fr = data["RUBRIQUE1"][lvl]

                if rub_fr not in self.rub_fr2gb:
                    self.rub_fr2gb[rub_fr] = data["RUBRIQUE1"][lvl]
                elif self.rub_fr2gb[rub_fr] != data["RUBRIQUE1"][lvl]:
                    raise TelemacException(\
                         "Two english translations) found for {}:\n"
                         "{} and {}".format(rub_fr,
                                            data["RUBRIQUE1"][lvl],
                                            self.rub_fr2gb[rub_fr]))


    def __str__(self):
        """
        Ascii representation of a dictionnary
        """
        # TODO: Make a fancy following section order
        string = "Printing: " + self.file_name + "\n\n"
        # Name of current rubriques
        rubs = ['', '', '']
        # Numerotation of current rubrique
        irubs = [0, 0, 0]
        for key in self.data:
            data = self.data[key]
            for irub in range(3):
                # Empty rubrique displaying keyword
                rub = data['RUBRIQUE1'][irub]
                if rub == '':
                    indent = (irub-1)*2
                    break
                if rub != rubs[irub]:
                    indent = irub
                    irubs[irub] += 1
                    for i in range(irub+1, 3):
                        irubs[i] = 0
                    string += "{indent}{sep}\n{indent}~> {num}-{rub}\n"\
                    "{indent}{sep}\n".format(
                        sep="~"*(72-(indent)),
                        indent=" "*indent,
                        num=".".join([str(i) for i in irubs[:(irub+1)]]),
                        rub=rub)
                    rubs[irub] = rub
            string += "  "*indent+"~> Key: {}\n   fr {}\n"\
                .format(key, self.gb2fr[key])
            for keyword in DICO_KEYS:
                if keyword in data:
                    # Specific display for CHOIX and CHOIX1
                    if keyword in ['CHOIX', 'CHOIX1']:
                        string += "   {} = \n".format(keyword)
                        # Integer choix
                        if isinstance(data[keyword], dict):
                            for idx, comment in data[keyword].items():
                                string += "   -  {} : {}\n"\
                                    .format(idx, comment)
                        # String choix
                        else:
                            for val in data[keyword]:
                                string += "   -  {}\n".format(val)
                    else:
                        string += "   {} = {}\n"\
                                  .format(keyword, data[keyword])

        return string
