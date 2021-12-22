/***************************************************************************
  reosdataobject.h - ReosDataObject

 ---------------------
 begin                : 4.2.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSDATAOBJECT_H
#define REOSDATAOBJECT_H

#include <QObject>

#include "reoscore.h"

class ReosEncodedElement;

//! Abstract class uses to be an interface for data
class REOSCORE_EXPORT ReosDataObject: public QObject
{
    Q_OBJECT
  public:
    ReosDataObject( QObject *parent = nullptr );

    //! Returns the type
    virtual QString type() const {return staticType();}

    //! Returns the name of the data object
    QString name() const;

    void encode( ReosEncodedElement &element ) const;
    void decode( const ReosEncodedElement &element );

    //! Static method hat return the type of this class
    static QString staticType() {return QStringLiteral( "data" );}

  public slots:
    //! Sets the name of the data object
    void setName( const QString &name );
    virtual void updateData() const {}; //TODO to set pure virtual

  signals:
    void dataChanged() const;
    void nameChanged( const QString &name ) const;
    void isSetObsolete() const;
    void settingsChanged();

  protected:
    void registerUpstreamData( ReosDataObject *data );
    void deregisterUpstreamData( ReosDataObject *data );
    void setActualized() const;

    //! Return true whether the data need to be calculated ot updated
    bool isObsolete() const;

  protected slots:
    void setObsolete();

  private:
    QString mName;
    mutable bool mIsObsolete = true;

//*** for tests
    friend class ReosRainfallTest;
    friend class ReosWatersehdTest;
};
#endif // REOSDATAOBJECT_H
