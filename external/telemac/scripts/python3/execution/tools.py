"""
Tools for parsing keywords
"""
import re
from utils.exceptions import TelemacException

KEY_COMMENT =\
 re.compile(r"(?P<before>([^'/]*'[^']*'[^'/]*|[^/]*)*){1}(?P<after>.*)", re.I)
CONTINUED = re.compile(r"(?P<before>[^']*)(?P<after>'[^']*)\s*\Z", re.I)
EMPTY_LINE = re.compile(r'\s*\Z', re.I)

ENTRY_QUOTE = re.compile(r'(?P<before>[^\'"]*)(?P<after>.*)\s*\Z', re.I)
EXIT_SQUOTE =\
 re.compile(r"'(?P<before>(.*?[^']+|))'(?P<after>[^']+.*)\s*\Z", re.I)
EXIT_DQUOTE =\
 re.compile(r'"(?P<before>(.*?[^"]+|))"(?P<after>[^"]+.*)\s*\Z', re.I)

KEY_NONE = re.compile(r'\s*(?P<key>&\w*)\s+(?P<after>.*)', re.I)
KEY_EQUALS = re.compile(r'(?P<key>[^=:]*)(?P<after>.*)', re.I)
VAL_EQUALS =\
 re.compile(r"[=:;]\s*(?P<val>('.*?'|[^\s;']*))\s*(?P<after>.*)", re.I)

KEY_WORD = r'\s*(?P<this>(%s))\s*(?P<after>.*)\s*\Z'
VAL_WORD = r"\s*[=:;]\s*(?P<this>('.*?'|%s))\s*(?P<after>.*)\s*\Z"

DICO_KEYS = ['NOM',
             'NOM1',
             'TYPE',
             'INDEX',
             'TAILLE',
             'SUBMIT',
             'DEFAUT',
             'DEFAUT1',
             'MNEMO',
             'CONTROLE',
             'CHOIX',
             'CHOIX1',
             'APPARENCE',
             'RUBRIQUE',
             'RUBRIQUE1',
             'COMPOSE',
             'COMPORT',
             'NIVEAU',
             'AIDE',
             'AIDE1',
             'ALLOCATE',
             'ALLOCATED',
             'ALLOCATED2',
             'PATHNODE']


def format72(value):
    """
    Adapt string to fit on a 72 line

    @param value (string) The string to adapt

    @return (string) The updated string
    """

    val72 = ''
    for i in range(int(len(value)/71)+1):
        val72 = val72 + (value+71*' ')[71*i:71*i+71] + '\n'

    return val72.rstrip()


def convert_to_type(var_type, value):
    """
    Convert a string (value in steering file or dictionnary) into its
    Python equivalent

    @param var_type (string) Type of val (INTEGER, REAL, LOGICAL, STRING)
    @param value (list) list of Values in string

    @return The value in (int, float, boolean, string)
    """
    # Regular exepression to describe an integer
    vint = re.compile(r'(-)?\d+\Z')
    # Regular exepression to describe a float
    vflt = re.compile(r'(-)?\d*(|\.)\d*([dDeE](\+|\-)?\d+|)\Z')
    # Possible value for logicals
    true_val = ['YES', 'Y', 'TRUE', '.TRUE.', 'OUI', 'O', '0', 'VRAI']
    false_val = ['NO', 'N', 'FALSE', '.FALSE.', 'NON', 'N', '1', 'FAUX']

    if var_type in ['LOGIQUE', 'LOGICAL']:
        vals = []
        for val in value:
            if val.upper() in true_val:
                vals.append(True)
            elif val.upper() in false_val:
                vals.append(False)
            else:
                raise TelemacException(
                   'Key value should be a LOGICAL but found an '
                   'inapropriate value: '+val)
    elif var_type in ['ENTIER', 'INTEGER']:
        vals = []
        for val in value:
            # Checking that it follows convention for describing
            # integers
            if re.match(vint, val.strip("'")):
                vals.append(int(val.strip("'")))
            else:
                raise TelemacException(
                   'Key value should be an INTEGER but found '
                   'an inapropriate value: '+val)
    elif var_type in ['REEL', 'REAL']:
        vals = []
        for val in value:
            if re.match(vflt, val.lower().replace('d', 'e')):
                vals.append(float(val.lower().replace('d', 'e').strip("'")))
            else:
                raise TelemacException(
                    'Key value should be a FLOAT but found an '
                    'inapropriate value: '+val)
    else:
        if isinstance(value, str):
            vals = [value]
        else:
            vals = []
            for val in value:
                vals.append(val.replace('"', "''"))

    # If only one value returning the value itself not the list
    if len(vals) == 1:
        return vals[0]
    else:
        return vals


def check_type(var_type, value):
    """
    Check that the type of value is the Python equivalent of var_type or a list
    of it

    @param var_type (string) Type of val (INTEGER, REAL, LOGICAL, STRING)
    @param value (int/boolean/float/string/list) Value to check

    @return The value in (int, float, boolean, string)
    """

    res = True
    if var_type in ['LOGIQUE', 'LOGICAL']:
        py_type = bool
    elif var_type in ['ENTIER', 'INTEGER']:
        py_type = int
    elif var_type in ['REEL', 'REAL']:
        py_type = float
    else:
        py_type = str

    # If only one value returning the value itself not the list
    if isinstance(value, list):
        for val in value:
            res = res and isinstance(val, py_type)
    else:
        res = isinstance(value, py_type)

    return res
