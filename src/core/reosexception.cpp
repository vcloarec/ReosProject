#include "reosexception.h"

ReosException::ReosException( const QString &message ): mWhat( message )
{

}

QString ReosException::what() const {return mWhat;}
