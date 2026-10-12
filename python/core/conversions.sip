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
