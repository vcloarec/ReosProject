/* Qt container conversions not provided by PyQt */

%MappedType QMap<int, double>
{
%TypeHeaderCode
#include <QMap>
%End

%ConvertFromTypeCode
  PyObject *d = PyDict_New();
  if ( !d )
    return NULL;

  for ( auto it = sipCpp->constBegin(); it != sipCpp->constEnd(); ++it )
  {
    PyObject *k = PyLong_FromLong( it.key() );
    PyObject *v = PyFloat_FromDouble( it.value() );

    if ( !k || !v || PyDict_SetItem( d, k, v ) < 0 )
    {
      Py_XDECREF( k );
      Py_XDECREF( v );
      Py_DECREF( d );
      return NULL;
    }

    Py_DECREF( k );
    Py_DECREF( v );
  }

  return d;
%End

%ConvertToTypeCode
  PyObject *kobj, *vobj;
  Py_ssize_t i = 0;

  if ( sipIsErr == NULL )
  {
    if ( !PyDict_Check( sipPy ) )
      return 0;

    while ( PyDict_Next( sipPy, &i, &kobj, &vobj ) )
    {
      if ( !PyLong_Check( kobj ) || !PyNumber_Check( vobj ) )
        return 0;
    }
    return 1;
  }

  QMap<int, double> *qm = new QMap<int, double>;

  while ( PyDict_Next( sipPy, &i, &kobj, &vobj ) )
  {
    int k = static_cast<int>( PyLong_AsLong( kobj ) );
    double v = PyFloat_AsDouble( vobj );

    if ( PyErr_Occurred() )
    {
      delete qm;
      *sipIsErr = 1;
      return 0;
    }

    qm->insert( k, v );
  }

  *sipCppPtr = qm;
  return sipGetState( sipTransferObj );
%End
};

%MappedType QVector<QMap<int, double>>
{
%TypeHeaderCode
#include <QVector>
#include <QMap>
%End

%ConvertFromTypeCode
  const sipTypeDef *mapType = sipFindType( "QMap<int, double>" );

  PyObject *l = PyList_New( sipCpp->size() );
  if ( !l )
    return NULL;

  for ( int i = 0; i < sipCpp->size(); ++i )
  {
    QMap<int, double> *m = new QMap<int, double>( sipCpp->at( i ) );
    PyObject *pm = sipConvertFromNewType( m, mapType, sipTransferObj );
    if ( !pm )
    {
      delete m;
      Py_DECREF( l );
      return NULL;
    }
    PyList_SET_ITEM( l, i, pm );
  }

  return l;
%End

%ConvertToTypeCode
  const sipTypeDef *mapType = sipFindType( "QMap<int, double>" );

  if ( sipIsErr == NULL )
  {
    if ( !PyList_Check( sipPy ) )
      return 0;

    for ( Py_ssize_t i = 0; i < PyList_GET_SIZE( sipPy ); ++i )
    {
      if ( !sipCanConvertToType( PyList_GET_ITEM( sipPy, i ), mapType, SIP_NOT_NONE ) )
        return 0;
    }
    return 1;
  }

  QVector<QMap<int, double>> *qv = new QVector<QMap<int, double>>;
  qv->reserve( static_cast<int>( PyList_GET_SIZE( sipPy ) ) );

  for ( Py_ssize_t i = 0; i < PyList_GET_SIZE( sipPy ); ++i )
  {
    int state;
    QMap<int, double> *m = reinterpret_cast<QMap<int, double> *>(
                             sipConvertToType( PyList_GET_ITEM( sipPy, i ), mapType, sipTransferObj, SIP_NOT_NONE, &state, sipIsErr ) );

    if ( *sipIsErr )
    {
      sipReleaseType( m, mapType, state );
      delete qv;
      return 0;
    }

    qv->append( *m );
    sipReleaseType( m, mapType, state );
  }

  *sipCppPtr = qv;
  return sipGetState( sipTransferObj );
%End
};
